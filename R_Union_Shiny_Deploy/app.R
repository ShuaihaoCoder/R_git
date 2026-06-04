# ============================================================
# ECO Screener Shiny Dashboard
# 这个文件是 Shiny 部署入口：负责检查依赖、读取数据、运行经济评分脚本、搭建 UI 和 server。
# ============================================================

# 定义这个 Shiny app 运行时必须安装的 R package 名单。
required_packages <- c(
  # 加载 Shiny、主题、绘图、表格、数据处理、编码和时间序列相关依赖名称。
  "shiny", "bslib", "ggplot2", "plotly", "DT",
  # 继续列出 tidyverse 辅助包、图片 base64 编码包、缺失值填充包和配色包。
  "dplyr", "tidyr", "base64enc", "zoo", "viridis"
)

# 检查 required_packages 里哪些包当前环境没有安装。
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
# 如果有缺失的包，就停止运行并提示用户安装。
if (length(missing_packages) > 0) {
  # 抛出一个清晰的错误信息，列出缺失包和 install.packages 安装命令。
  stop(
    # 错误信息第一部分：告诉用户缺了哪些 R 包。
    "Missing required R packages: ", paste(missing_packages, collapse = ", "), "\n",
    # 错误信息第二部分：提示下面会给出安装命令。
    "Install them with:\n",
    # 错误信息第三部分：动态拼出 install.packages(c(...)) 命令。
    "install.packages(c(", paste(sprintf('\"%s\"', missing_packages), collapse = ", "), "))",
    # 不显示函数调用栈，让错误提示更简洁。
    call. = FALSE
  )
}

# 获取当前 Shiny app 的项目目录，并统一使用 / 作为路径分隔符。
project_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
# 拼出优化版经济筛选脚本 Eco_screener_optimized.R 的完整路径。
optimized_script <- file.path(project_dir, "Eco_screener_optimized.R")
# 拼出宽表经济数据文件 WIDE_ALLX 的完整路径。
wide_eco_file <- file.path(project_dir, "WIDE_ALLX")

# 从 RDS 文件读取宽格式经济数据。
wide_eco_data <- readRDS(wide_eco_file)
# 把数据里的 date 列转换为 Date 类型，方便后续日期比较和筛选。
wide_eco_data$date <- as.Date(wide_eco_data$date)
# 提取数据里所有可用日期，去重后排序。
available_dates <- sort(unique(wide_eco_data$date))
# 计算数据中最早可用日期，作为日期控件和历史窗口的下限。
min_available_date <- min(available_dates, na.rm = TRUE)
# 计算数据中最晚可用日期，作为默认展示日期和日期控件上限。
max_available_date <- max(available_dates, na.rm = TRUE)

# 设置仪表盘默认展示的国家列表。
default_countries <- c("UNITED STATES", "EUROZONE", "JAPAN", "BRITAIN", "GERMANY")
# 设置仪表盘默认展示的宏观指标列表。
default_indicators <- c("CPI", "Real GDP", "PMI", "Unemployment")
# 定义历史窗口下拉菜单的显示文字和对应年数。
history_window_choices <- c("3 years" = 3, "5 years" = 5, "10 years" = 10, "All history" = 0)

# 定义一个函数：把用户选择的日期转换成实际有数据的日期。
resolve_effective_date <- function(selected_date, dates) {
  # 确保传入的 selected_date 是 Date 类型。
  selected_date <- as.Date(selected_date)
  # 如果用户选择的日期本身有数据，就直接返回这个日期。
  if (selected_date %in% dates) return(selected_date)
  # 找出所有小于等于用户选择日期的可用日期。
  earlier_dates <- dates[dates <= selected_date]
  # 如果用户选择日期之前没有任何数据，就停止并提示。
  if (length(earlier_dates) == 0) {
    # 抛出错误：没有找到所选日期之前的可用数据。
    stop("No data is available on or before ", selected_date, call. = FALSE)
  }
  # 返回距离用户选择日期最近的、更早的可用日期。
  max(earlier_dates)
}

# 定义一个函数：把数值向量缩放到 0 到 1 之间。
scale_to_01 <- function(values) {
  # 如果整个向量都是 NA，就返回同样长度的 NA。
  if (all(is.na(values))) return(rep(NA_real_, length(values)))
  # 计算非缺失值中的最小值。
  value_min <- min(values, na.rm = TRUE)
  # 计算非缺失值中的最大值。
  value_max <- max(values, na.rm = TRUE)
  # 如果最大值和最小值相同，就返回 0.5，避免除以 0。
  if (isTRUE(all.equal(value_min, value_max))) return(rep(0.5, length(values)))
  # 用 min-max scaling 把原始值转换为 0 到 1 的相对分数。
  (values - value_min) / (value_max - value_min)
}

# 定义一个函数：用上一个非缺失观测值填补后面的缺失值。
last_observation_carried_forward <- function(values) {
  # 使用 zoo::na.locf 执行 LOCF 填补，并保留开头无法填补的 NA。
  zoo::na.locf(values, na.rm = FALSE)
}

# 定义一个函数：返回向量里最后一个非缺失值。
last_non_missing <- function(values) {
  # 去掉所有 NA，只保留有效数值。
  values <- values[!is.na(values)]
  # 如果没有任何有效值，就返回 NA。
  if (length(values) == 0) return(NA_real_)
  # 返回最后一个有效值。
  dplyr::last(values)
}

# 定义一个函数：把本地图片文件转换成浏览器可直接显示的 data URI。
img_data_uri <- function(path) {
  # 如果路径为空或文件不存在，就返回 NULL，供 UI 显示缺失提示。
  if (is.null(path) || !file.exists(path)) return(NULL)
  # 把 png 图片编码为 base64，并加上 data URI 前缀。
  paste0("data:image/png;base64,", base64enc::base64encode(path))
}

# 定义一个函数：建立 “国家 + 指标” 到原始数据列名的映射。
build_country_indicator_map <- function(indicator_column_map, get_country_from_col) {
  # 创建一个空 list，用来存放最终映射结果。
  out <- list()
  # 遍历每一个宏观指标名称。
  for (indicator in names(indicator_column_map)) {
    # 取出这个指标对应的所有原始数据列名。
    columns <- indicator_column_map[[indicator]]
    # 如果这个指标没有对应列，就跳过。
    if (length(columns) == 0) next
    # 遍历这个指标下的每一个数据列。
    for (column in columns) {
      # 从列名中解析国家名称。
      country <- get_country_from_col(column)
      # 如果解析不出国家，就跳过这一列。
      if (is.na(country)) next
      # 用 “国家::指标” 作为 key，保存国家、指标和原始列名。
      out[[paste(country, indicator, sep = "::")]] <- list(
        # 保存国家名称。
        country = country,
        # 保存指标名称。
        indicator = indicator,
        # 保存该国家该指标对应的原始数据列名。
        column = column
      )
    }
  }
  # 返回完整的国家-指标-列名映射。
  out
}

# 定义一个函数：根据结束日期和历史年数计算历史数据起始日期。
history_start_date <- function(end_date, years) {
  # 如果 years 是 NULL 或 0，表示使用全部历史数据，从最早日期开始。
  if (is.null(years) || years == 0) return(min_available_date)
  # 用年数乘以平均一年天数，得到大致的历史窗口起点。
  as.Date(end_date) - round(as.numeric(years) * 365.25)
}

# 定义一个函数：根据选择的国家、指标和日期窗口构建原始历史数据。
build_history_raw_data <- function(data, column_map, countries, indicators, start_date, end_date) {
  # 创建日期筛选条件，只保留开始日期和结束日期之间的数据。
  date_mask <- data$date >= start_date & data$date <= end_date
  # 取出筛选后的日期向量，用于输出数据框。
  date_values <- data$date[date_mask]
  # 创建空 list，用于逐个保存国家-指标组合的数据框。
  rows <- list()

  # 遍历用户选择的国家。
  for (country in countries) {
    # 遍历用户选择的指标。
    for (indicator in indicators) {
      # 拼出当前国家和指标组合的映射 key。
      map_key <- paste(country, indicator, sep = "::")
      # 从映射表中取出当前组合对应的列信息。
      entry <- column_map[[map_key]]
      # 如果映射不存在或数据里没有对应列，就跳过。
      if (is.null(entry) || !entry$column %in% names(data)) next

      # 从原始数据列中取出日期窗口内的值，并尽量转换成 numeric。
      raw_values <- suppressWarnings(as.numeric(data[[entry$column]][date_mask]))
      # 把当前国家-指标组合整理成标准长表，并放入 rows。
      rows[[length(rows) + 1]] <- data.frame(
        # 保存每个观测对应的日期。
        date = date_values,
        # 保存国家名称。
        Country = country,
        # 保存指标名称。
        Indicator = indicator,
        # 保存经过 LOCF 填补后的指标值。
        Value = last_observation_carried_forward(raw_values),
        # 保存原始数据列名，方便图表 tooltip 展示来源。
        Source = entry$column,
        # 防止 R 自动改列名。
        check.names = FALSE
      )
    }
  }

  # 如果没有任何可用组合，就返回一个空的标准结构数据框。
  if (length(rows) == 0) {
    # 返回空数据框，列名和类型与正常输出保持一致。
    return(data.frame(date = as.Date(character()), Country = character(), Indicator = character(), Value = numeric()))
  }
  # 把所有国家-指标组合的数据框合并成一个长表。
  dplyr::bind_rows(rows)
}

# 定义一个函数：基于原始历史数据计算每个国家每天的综合分数。
build_history_score_data <- function(raw_history) {
  # 如果没有历史数据，就返回空的标准结构数据框。
  if (nrow(raw_history) == 0) {
    # 返回空数据框，供后续 plotly 校验使用。
    return(data.frame(date = as.Date(character()), Country = character(), Score = numeric()))
  }

  # 先按日期和指标分组，把同一天同一指标下各国的值缩放到 0-1。
  score_source <- raw_history |>
    # 按日期和指标分组，确保同类指标横向比较。
    dplyr::group_by(date, Indicator) |>
    # 在每个日期-指标组内计算标准化分数。
    dplyr::mutate(Scaled = scale_to_01(Value)) |>
    # 取消分组，避免影响后续汇总。
    dplyr::ungroup()

  # 再按日期和国家汇总，得到每个国家每天的平均宏观分数。
  score_source |>
    # 按日期和国家分组。
    dplyr::group_by(date, Country) |>
    # 对所选指标的标准化分数取平均，得到综合 Score。
    dplyr::summarise(Score = mean(Scaled, na.rm = TRUE), .groups = "drop") |>
    # 如果某天全是 NA 导致 NaN，就转换回 NA。
    dplyr::mutate(Score = ifelse(is.nan(Score), NA_real_, Score))
}

# 定义一个函数：计算当前有效日期下各国家各指标的横向强弱分数。
build_current_profile_data <- function(raw_history, effective_date) {
  # 如果没有历史数据，就返回空的标准结构数据框。
  if (nrow(raw_history) == 0) {
    # 返回空数据框，供 profile 图表校验使用。
    return(data.frame(Country = character(), Indicator = character(), Score = numeric()))
  }

  # 找出每个国家-指标在有效日期之前的最后一个有效值。
  latest_values <- raw_history |>
    # 只保留有效日期当天及之前的数据。
    dplyr::filter(date <= effective_date) |>
    # 按日期排序，保证 last_non_missing 能取到时间上最新的值。
    dplyr::arrange(date) |>
    # 按国家和指标分组。
    dplyr::group_by(Country, Indicator) |>
    # 汇总出每个组合的最后一个非缺失值。
    dplyr::summarise(Value = last_non_missing(Value), .groups = "drop")

  # 对每个指标横向比较各国，把最新值转换为 0-1 分数。
  latest_values |>
    # 按指标分组，让同一指标下不同国家互相比。
    dplyr::group_by(Indicator) |>
    # 在每个指标内计算国家相对分数。
    dplyr::mutate(Score = scale_to_01(Value)) |>
    # 取消分组，返回普通数据框。
    dplyr::ungroup()
}

# 定义一个函数：运行优化版 ECO 脚本并收集生成的结果。
run_eco_report <- function(selected_date) {
  # 把用户选择日期解析成数据中实际可用的有效日期。
  effective_date <- resolve_effective_date(selected_date, available_dates)
  # 检查全局环境里是否已经存在名为 date 的变量。
  previous_global_date_exists <- exists("date", envir = .GlobalEnv, inherits = FALSE)
  # 如果全局 date 已存在，就先保存旧值；否则保存 NULL。
  previous_global_date <- if (previous_global_date_exists) get("date", envir = .GlobalEnv) else NULL
  # 把有效日期写入全局环境，因为被 source 的脚本会读取这个 date。
  assign("date", as.character(effective_date), envir = .GlobalEnv)

  # 注册退出清理逻辑，确保函数结束后恢复或删除全局 date。
  on.exit({
    # 如果函数运行前全局 date 已存在，就恢复原来的值。
    if (previous_global_date_exists) {
      # 恢复旧的全局 date。
      assign("date", previous_global_date, envir = .GlobalEnv)
    # 如果函数运行前没有全局 date，但现在有，就删除它。
    } else if (exists("date", envir = .GlobalEnv, inherits = FALSE)) {
      # 删除本函数临时创建的全局 date。
      rm("date", envir = .GlobalEnv)
    }
  }, add = TRUE)

  # 创建一个新环境，用来隔离 source 脚本产生的变量。
  report_env <- new.env(parent = .GlobalEnv)
  # 执行优化版脚本，并把脚本里的对象放进 report_env。
  source(optimized_script, local = report_env)

  # 从 report_env 中取出脚本生成的列映射，并转换成 dashboard 更好用的 key-value 结构。
  column_map <- build_country_indicator_map(
    # 取出指标到列名列表的映射。
    get("indicator_column_map", envir = report_env),
    # 取出从列名解析国家的函数。
    get("get_country_from_col", envir = report_env)
  )

  # 返回一个 list，集中保存 dashboard 后续需要用到的所有报表结果和资产路径。
  list(
    # 保存用户原始请求日期。
    requested_date = as.Date(selected_date),
    # 保存脚本实际使用的数据日期。
    effective_date = as.Date(get("date", envir = report_env)),
    # 保存宏观评分表。
    table = get("macro_score_table", envir = report_env),
    # 保存 heatmap 图片文件路径。
    heatmap = get("heatmap_file", envir = report_env),
    # 保存 bar chart 图片文件路径。
    bars = get("bar_file", envir = report_env),
    # 保存 radar chart 图片文件路径。
    radar = get("radar_file", envir = report_env),
    # 保存生成的 HTML 报告路径。
    html = get("html_file", envir = report_env),
    # 保存生成的 CSV 分数表路径。
    csv = get("csv_file", envir = report_env),
    # 保存结果中包含的国家名称。
    countries = rownames(get("result_scaled", envir = report_env)),
    # 保存结果中包含的指标名称。
    indicators = names(get("macro_indicator_specs", envir = report_env)),
    # 保存国家-指标到原始列名的映射。
    column_map = column_map
  )
}

# 创建 Bootstrap 5 主题对象，供 Shiny UI 使用。
theme <- bslib::bs_theme(
  # 指定使用 Bootstrap 5。
  version = 5,
  # 使用 flatly 这个 bootswatch 主题。
  bootswatch = "flatly",
  # 设置主题主色，用于按钮等重点元素。
  primary = "#2563eb"
)

# 定义 Shiny 前端页面结构。
ui <- shiny::fluidPage(
  # 应用上面定义好的主题。
  theme = theme,
  # 向 HTML head 中加入标题和 CSS。
  shiny::tags$head(
    # 设置浏览器标签页标题。
    shiny::tags$title("ECO Screener Dashboard"),
    # 写入自定义 CSS 样式。
    shiny::tags$style(shiny::HTML("
      body { background: #f4f7fb; color: #18212f; } /* 设置页面背景和默认文字颜色。 */
      .app-shell { max-width: 1500px; margin: 0 auto; padding: 24px; } /* 控制主内容最大宽度、居中和内边距。 */
      .hero { display: flex; justify-content: space-between; gap: 18px; align-items: flex-end; margin-bottom: 18px; } /* 设置顶部标题区为横向布局。 */
      .hero h1 { margin: 0; font-weight: 760; letter-spacing: 0; } /* 设置标题的边距、字重和字距。 */
      .hero p { margin: 6px 0 0; color: #5d6b7c; } /* 设置标题下说明文字的边距和颜色。 */
      .control-panel, .panel, .metric-card { /* 给控制面板、内容面板和指标卡片统一卡片样式。 */
        background: #ffffff; border: 1px solid #dbe4ef; border-radius: 8px; /* 设置白色背景、边框和圆角。 */
        box-shadow: 0 8px 22px rgba(15, 23, 42, 0.05); /* 设置轻微阴影。 */
      } /* 结束统一卡片样式。 */
      .control-panel { padding: 16px; position: sticky; top: 12px; } /* 设置左侧控制面板内边距和滚动时吸顶。 */
      .metric-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 12px; margin-bottom: 16px; } /* 设置四列指标卡片网格。 */
      .metric-card { padding: 14px 16px; min-height: 92px; } /* 设置指标卡片内边距和最小高度。 */
      .metric-label { color: #687789; font-size: 12px; text-transform: uppercase; letter-spacing: .04em; } /* 设置指标卡片标签样式。 */
      .metric-value { font-size: 24px; font-weight: 760; line-height: 1.2; margin-top: 8px; word-break: break-word; } /* 设置指标卡片数值样式。 */
      .panel { padding: 16px; margin-bottom: 16px; } /* 设置内容面板内边距和底部间距。 */
      .panel-title { font-size: 18px; font-weight: 720; margin: 0 0 12px; } /* 设置面板标题字体和间距。 */
      .plot-img { display: block; width: 100%; height: auto; border-radius: 6px; border: 1px solid #e3eaf3; } /* 设置图片自适应宽度和边框。 */
      .muted { color: #687789; } /* 设置弱提示文字颜色。 */
      .report-link { word-break: break-all; } /* 允许长路径或链接自动换行。 */
      .nav-tabs { margin-bottom: 16px; } /* 设置 tab 导航和内容之间的间距。 */
      .form-label, label { font-weight: 650; color: #2c3a4b; } /* 设置表单标签字重和颜色。 */
      @media (max-width: 1000px) { /* 设置中小屏幕下的响应式样式。 */
        .hero { display: block; } /* 中小屏幕下把顶部标题区改成块级布局。 */
        .metric-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); } /* 中小屏幕下指标卡片改成两列。 */
        .control-panel { position: static; margin-bottom: 16px; } /* 中小屏幕下取消控制面板吸顶并增加底部间距。 */
      } /* 结束中小屏幕响应式规则。 */
      @media (max-width: 620px) { /* 设置手机屏幕下的响应式样式。 */
        .app-shell { padding: 14px; } /* 手机上减小主容器内边距。 */
        .metric-grid { grid-template-columns: 1fr; } /* 手机上指标卡片改成单列。 */
      } /* 结束手机屏幕响应式规则。 */
    "))
  ),
  # 创建页面最外层内容容器。
  shiny::div(
    # 给最外层容器加 CSS class。
    class = "app-shell",
    # 创建顶部 hero 区域。
    shiny::div(
      # 给 hero 区域加 CSS class。
      class = "hero",
      # 创建 hero 左侧标题和说明。
      shiny::div(
        # 显示应用主标题。
        shiny::h1("ECO Screener Dashboard"),
        # 显示应用说明文字。
        shiny::p("Developed-market macro scores, indicator history, and generated report assets.")
      ),
      # 创建刷新按钮，用于重新运行报表。
      shiny::actionButton("run", "Refresh Dashboard", class = "btn-primary")
    ),
    # 创建主页面的行布局。
    shiny::fluidRow(
      # 创建左侧控制栏列。
      shiny::column(
        # 设置左侧控制栏宽度为 Bootstrap 12 栅格中的 3 格。
        width = 3,
        # 创建控制面板容器。
        shiny::div(
          # 给控制面板加 CSS class。
          class = "control-panel",
          # 创建日期输入控件。
          shiny::dateInput(
            # 设置日期控件的 input id，server 通过 input$date 读取。
            inputId = "date",
            # 设置日期控件标签。
            label = "Requested date",
            # 默认日期使用数据中最新日期。
            value = max_available_date,
            # 限制最早可选日期。
            min = min_available_date,
            # 限制最晚可选日期。
            max = max_available_date,
            # 设置日期显示格式。
            format = "yyyy-mm-dd"
          ),
          # 预留国家选择器 UI，由 server 动态渲染。
          shiny::uiOutput("country_selector"),
          # 预留指标选择器 UI，由 server 动态渲染。
          shiny::uiOutput("indicator_selector"),
          # 创建历史窗口选择器。
          shiny::selectInput("history_years", "History window", choices = history_window_choices, selected = 5),
          # 插入横线分隔控件和说明。
          shiny::hr(),
          # 显示日期 fallback 说明。
          shiny::p(class = "muted", "If the requested date has no data, the dashboard uses the nearest earlier available date.")
        )
      ),
      # 创建右侧主内容列。
      shiny::column(
        # 设置右侧主内容宽度为 Bootstrap 12 栅格中的 9 格。
        width = 9,
        # 预留顶部指标卡片区域，由 server 动态渲染。
        shiny::uiOutput("metric_cards"),
        # 创建主内容 tab 面板。
        shiny::tabsetPanel(
          # 设置 tabset 的 id，方便后续需要时读取当前 tab。
          id = "main_tabs",
          # 创建 Overview tab。
          shiny::tabPanel(
            # 设置 tab 标题。
            "Overview",
            # 创建 heatmap 图片面板。
            shiny::div(class = "panel", shiny::div(class = "panel-title", "Scoreboard Heatmap"), shiny::uiOutput("heatmap_img")),
            # 创建 indicator bars 图片面板。
            shiny::div(class = "panel", shiny::div(class = "panel-title", "Indicator Bars"), shiny::uiOutput("bar_img")),
            # 创建 radar 图片面板。
            shiny::div(class = "panel", shiny::div(class = "panel-title", "All Countries Radar"), shiny::uiOutput("radar_img"))
          ),
          # 创建 History tab。
          shiny::tabPanel(
            # 设置 tab 标题。
            "History",
            # 创建原始指标趋势图面板。
            shiny::div(class = "panel", shiny::div(class = "panel-title", "Raw Indicator Trends"), plotly::plotlyOutput("raw_trend_plot", height = "440px")),
            # 创建综合分数趋势图面板。
            shiny::div(class = "panel", shiny::div(class = "panel-title", "Normalized Macro Score Trend"), plotly::plotlyOutput("score_trend_plot", height = "420px")),
            # 创建当前强弱对比图面板。
            shiny::div(class = "panel", shiny::div(class = "panel-title", "Current Strengths and Weaknesses"), plotly::plotlyOutput("profile_plot", height = "420px"))
          ),
          # 创建 Scoreboard tab。
          shiny::tabPanel(
            # 设置 tab 标题。
            "Scoreboard",
            # 创建评分表格面板。
            shiny::div(class = "panel", DT::DTOutput("score_table"))
          ),
          # 创建 Generated Report tab。
          shiny::tabPanel(
            # 设置 tab 标题。
            "Generated Report",
            # 创建报告摘要面板。
            shiny::div(class = "panel", shiny::uiOutput("report_summary"))
          )
        )
      )
    )
  )
)

# 定义 Shiny server：处理响应式数据、更新控件和渲染图表。
server <- function(input, output, session) {
  # 创建响应式报表结果；点击刷新按钮或初始化时会运行 ECO 报表。
  report_result <- shiny::eventReactive(input$run, {
    # 用当前日期输入运行经济筛选报表。
    run_eco_report(input$date)
  }, ignoreInit = FALSE)

  # 当 report_result 更新时，同步更新国家和指标选择器的选项。
  shiny::observeEvent(report_result(), {
    # 取出最新报表结果。
    res <- report_result()
    # 获取报表中实际可用的国家，并排序。
    country_choices <- sort(res$countries)
    # 获取报表中实际可用的指标。
    indicator_choices <- res$indicators
    # 默认国家只保留实际可用的那些。
    selected_countries <- intersect(default_countries, country_choices)
    # 默认指标只保留实际可用的那些。
    selected_indicators <- intersect(default_indicators, indicator_choices)

    # 更新国家 selectize 控件的选项和默认选择。
    shiny::updateSelectizeInput(session, "countries", choices = country_choices, selected = selected_countries, server = TRUE)
    # 更新指标 selectize 控件的选项和默认选择。
    shiny::updateSelectizeInput(session, "indicators", choices = indicator_choices, selected = selected_indicators, server = TRUE)
  })

  # 渲染国家多选控件。
  output$country_selector <- shiny::renderUI({
    # 创建 selectize 多选输入，用于选择国家。
    shiny::selectizeInput(
      # 设置 input id，server 通过 input$countries 读取。
      "countries",
      # 设置控件标签。
      "Countries",
      # 初始化时先使用默认国家列表作为候选项。
      choices = default_countries,
      # 初始化时默认选中默认国家列表。
      selected = default_countries,
      # 允许多选。
      multiple = TRUE,
      # 添加删除按钮插件，并最多允许选择 8 个国家。
      options = list(plugins = list("remove_button"), maxItems = 8)
    )
  })

  # 渲染指标多选控件。
  output$indicator_selector <- shiny::renderUI({
    # 创建 selectize 多选输入，用于选择宏观指标。
    shiny::selectizeInput(
      # 设置 input id，server 通过 input$indicators 读取。
      "indicators",
      # 设置控件标签。
      "Indicators",
      # 初始化时先使用默认指标列表作为候选项。
      choices = default_indicators,
      # 初始化时默认选中默认指标列表。
      selected = default_indicators,
      # 允许多选。
      multiple = TRUE,
      # 添加删除按钮插件，并最多允许选择 6 个指标。
      options = list(plugins = list("remove_button"), maxItems = 6)
    )
  })

  # 创建一个响应式数据集：根据控件选择生成历史原始数据。
  selected_history <- shiny::reactive({
    # 取出当前报表结果。
    res <- report_result()
    # 读取用户选择的国家；如果为空，就回退到默认国家和可用国家的交集。
    countries <- input$countries %||% intersect(default_countries, res$countries)
    # 读取用户选择的指标；如果为空，就回退到默认指标和可用指标的交集。
    indicators <- input$indicators %||% intersect(default_indicators, res$indicators)
    # 读取历史窗口年数；如果为空，就默认用 5 年。
    years <- as.numeric(input$history_years %||% 5)
    # 根据有效日期和年数计算历史数据起始日期。
    start_date <- history_start_date(res$effective_date, years)

    # 按当前选择构建历史原始数据长表。
    build_history_raw_data(
      # 使用全局读取的宽格式经济数据。
      data = wide_eco_data,
      # 使用当前报表生成的国家-指标列名映射。
      column_map = res$column_map,
      # 传入用户选择的国家。
      countries = countries,
      # 传入用户选择的指标。
      indicators = indicators,
      # 传入历史窗口开始日期。
      start_date = start_date,
      # 传入当前报表有效日期作为结束日期。
      end_date = res$effective_date
    )
  })

  # 渲染顶部四个指标卡片。
  output$metric_cards <- shiny::renderUI({
    # 取出当前报表结果。
    res <- report_result()
    # 取出当前宏观评分表。
    score_table <- res$table
    # 找出 Total_Score 最高的国家。
    top_country <- score_table$Country[which.max(score_table$Total_Score)]
    # 找出 Total_Score 最低的国家。
    low_country <- score_table$Country[which.min(score_table$Total_Score)]
    # 生成日期显示文字；如果发生 fallback，就显示 fallback 来源。
    date_caption <- if (res$requested_date == res$effective_date) {
      # 没有 fallback 时只显示有效日期。
      as.character(res$effective_date)
    } else {
      # 发生 fallback 时显示有效日期和用户请求日期。
      paste0(as.character(res$effective_date), " (fallback from ", as.character(res$requested_date), ")")
    }

    # 创建四列指标卡片网格。
    shiny::div(
      # 设置指标卡片网格 CSS class。
      class = "metric-grid",
      # 第一张卡片显示有效数据日期。
      shiny::div(class = "metric-card", shiny::div(class = "metric-label", "Effective Date"), shiny::div(class = "metric-value", date_caption)),
      # 第二张卡片显示分数最高国家。
      shiny::div(class = "metric-card", shiny::div(class = "metric-label", "Top Score"), shiny::div(class = "metric-value", top_country)),
      # 第三张卡片显示分数最低国家。
      shiny::div(class = "metric-card", shiny::div(class = "metric-label", "Lowest Score"), shiny::div(class = "metric-value", low_country)),
      # 第四张卡片显示评分表里的国家数量。
      shiny::div(class = "metric-card", shiny::div(class = "metric-label", "Countries"), shiny::div(class = "metric-value", nrow(score_table)))
    )
  })

  # 渲染 Overview tab 中的 heatmap 图片。
  output$heatmap_img <- shiny::renderUI({
    # 把 heatmap 图片路径转换成浏览器可显示的 base64 data URI。
    src <- img_data_uri(report_result()$heatmap)
    # 如果图片不存在，就显示缺失提示。
    if (is.null(src)) return(shiny::p(class = "muted", "Heatmap file is not available."))
    # 如果图片存在，就渲染 img 标签。
    shiny::tags$img(class = "plot-img", src = src, alt = "ECO macro scoreboard heatmap")
  })

  # 渲染 Overview tab 中的 bar chart 图片。
  output$bar_img <- shiny::renderUI({
    # 把 bar chart 图片路径转换成浏览器可显示的 base64 data URI。
    src <- img_data_uri(report_result()$bars)
    # 如果图片不存在，就显示缺失提示。
    if (is.null(src)) return(shiny::p(class = "muted", "Bar chart file is not available."))
    # 如果图片存在，就渲染 img 标签。
    shiny::tags$img(class = "plot-img", src = src, alt = "Indicator bar charts")
  })

  # 渲染 Overview tab 中的 radar 图片。
  output$radar_img <- shiny::renderUI({
    # 把 radar 图片路径转换成浏览器可显示的 base64 data URI。
    src <- img_data_uri(report_result()$radar)
    # 如果图片不存在，就显示缺失提示。
    if (is.null(src)) return(shiny::p(class = "muted", "Radar chart file is not available."))
    # 如果图片存在，就渲染 img 标签。
    shiny::tags$img(class = "plot-img", src = src, alt = "All countries radar chart")
  })

  # 渲染 History tab 中的原始指标趋势交互图。
  output$raw_trend_plot <- plotly::renderPlotly({
    # 根据当前国家、指标和历史窗口获取原始历史数据。
    raw_history <- selected_history()
    # 如果没有数据，就在图表区域显示提示信息。
    shiny::validate(shiny::need(nrow(raw_history) > 0, "No history data is available for the selected countries and indicators."))

    # 准备绘图数据：去掉缺失值，并创建图例系列名称。
    plot_data <- raw_history |>
      # 删除 Value 缺失的记录，避免画出无意义线段。
      dplyr::filter(!is.na(Value)) |>
      # 把国家和指标拼成一个 Series 字段，用于颜色和图例。
      dplyr::mutate(Series = paste(Country, Indicator, sep = " - "))

    # 用 ggplot 创建原始指标趋势图。
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = Value, color = Series, text = Source)) +
      # 添加折线层。
      ggplot2::geom_line(linewidth = 0.7, alpha = 0.9) +
      # 按指标分面，并允许每个分面的 y 轴独立缩放。
      ggplot2::facet_wrap(ggplot2::vars(Indicator), scales = "free_y", ncol = 2) +
      # 设置坐标轴和图例标题。
      ggplot2::labs(x = NULL, y = "Raw value", color = NULL) +
      # 使用简洁主题。
      ggplot2::theme_minimal(base_size = 12) +
      # 调整图例位置并移除次级网格线。
      ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())

    # 把 ggplot 转成 plotly，并设置 tooltip 和横向图例。
    plotly::ggplotly(p, tooltip = c("x", "y", "colour", "text")) |>
      # 设置 plotly 图例横向排列并放到图表下方。
      plotly::layout(legend = list(orientation = "h", y = -0.2))
  })

  # 渲染 History tab 中的标准化综合分数趋势图。
  output$score_trend_plot <- plotly::renderPlotly({
    # 根据当前历史原始数据计算国家综合分数历史。
    score_history <- build_history_score_data(selected_history())
    # 如果没有分数历史，就在图表区域显示提示信息。
    shiny::validate(shiny::need(nrow(score_history) > 0, "No score history is available for the selected controls."))

    # 用 ggplot 创建综合分数趋势图。
    p <- ggplot2::ggplot(score_history, ggplot2::aes(x = date, y = Score, color = Country)) +
      # 添加折线层。
      ggplot2::geom_line(linewidth = 0.9) +
      # 把 y 轴固定在 0 到 1，符合标准化分数范围。
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      # 设置坐标轴和图例标题。
      ggplot2::labs(x = NULL, y = "Normalized score", color = NULL) +
      # 使用简洁主题。
      ggplot2::theme_minimal(base_size = 12) +
      # 调整图例位置并移除次级网格线。
      ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())

    # 把 ggplot 转成 plotly，并设置 tooltip 和横向图例。
    plotly::ggplotly(p, tooltip = c("x", "y", "colour")) |>
      # 设置 plotly 图例横向排列并放到图表下方。
      plotly::layout(legend = list(orientation = "h", y = -0.2))
  })

  # 渲染 History tab 中的当前强弱对比柱状图。
  output$profile_plot <- plotly::renderPlotly({
    # 取出当前报表结果。
    res <- report_result()
    # 根据当前选择和有效日期计算最新 profile 数据。
    profile_data <- build_current_profile_data(selected_history(), res$effective_date)
    # 如果没有 profile 数据，就在图表区域显示提示信息。
    shiny::validate(shiny::need(nrow(profile_data) > 0, "No current profile data is available for the selected controls."))

    # 用 ggplot 创建当前指标强弱柱状图。
    p <- ggplot2::ggplot(profile_data, ggplot2::aes(x = Indicator, y = Score, fill = Country)) +
      # 添加并排柱状图层。
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.68) +
      # 把 y 轴固定在 0 到 1，符合标准化分数范围。
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      # 设置坐标轴和图例标题。
      ggplot2::labs(x = NULL, y = "Current normalized score", fill = NULL) +
      # 使用简洁主题。
      ggplot2::theme_minimal(base_size = 12) +
      # 调整图表主题细节。
      ggplot2::theme(
        # 把图例放在底部。
        legend.position = "bottom",
        # 去掉次级网格线。
        panel.grid.minor = ggplot2::element_blank(),
        # 旋转 x 轴标签，避免指标名称重叠。
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
      )

    # 把 ggplot 转成 plotly，并设置 tooltip 和横向图例。
    plotly::ggplotly(p, tooltip = c("x", "y", "fill")) |>
      # 设置 plotly 图例横向排列并放到图表下方。
      plotly::layout(legend = list(orientation = "h", y = -0.25))
  })

  # 渲染 Scoreboard tab 中的交互式数据表。
  output$score_table <- DT::renderDT({
    # 创建 DT 表格。
    DT::datatable(
      # 使用当前报表生成的评分表作为数据源。
      report_result()$table,
      # 不显示行名。
      rownames = FALSE,
      # 在表头下方显示筛选框。
      filter = "top",
      # 设置分页、横向滚动和表格控件显示方式。
      options = list(pageLength = 12, scrollX = TRUE, dom = "tip")
    ) |>
      # 把所有 numeric 列保留 3 位小数显示。
      DT::formatRound(columns = names(report_result()$table)[vapply(report_result()$table, is.numeric, logical(1))], digits = 3)
  })

  # 渲染 Generated Report tab 中的报告路径和日期摘要。
  output$report_summary <- shiny::renderUI({
    # 取出当前报表结果。
    res <- report_result()
    # 创建多个 HTML 标签组成的报告摘要。
    shiny::tagList(
      # 显示用户请求日期。
      shiny::p(shiny::strong("Requested date: "), as.character(res$requested_date)),
      # 显示实际使用的数据日期。
      shiny::p(shiny::strong("Effective data date: "), as.character(res$effective_date)),
      # 显示生成的 HTML 报告路径。
      shiny::p(shiny::strong("HTML report: "), shiny::span(class = "report-link", res$html)),
      # 显示生成的 CSV 评分表路径。
      shiny::p(shiny::strong("CSV scoreboard: "), shiny::span(class = "report-link", res$csv)),
      # 显示图片资产位置说明。
      shiny::p(class = "muted", "Generated image assets are embedded in the Overview tab. The standalone HTML report is written next to those assets in the output folder.")
    )
  })
}

# 定义一个空值回退运算符：x 为空时使用 y。
`%||%` <- function(x, y) {
  # 如果 x 是 NULL 或长度为 0，就返回 y；否则返回 x。
  if (is.null(x) || length(x) == 0) y else x
}

# 启动 Shiny 应用，把 UI 和 server 绑定在一起。
shiny::shinyApp(ui, server)
