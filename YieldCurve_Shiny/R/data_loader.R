# ============================================================
# 市场数据读取与曲线提取
# ============================================================
# 整体作用：定位原始 RDS，只读加载数据，并把宽表报价整理成 curve_engine.R 可使用的
# tenor + rate 两列数据。本文件不做拟合，也不修改 R_Union 中的源文件。

find_project_dir <- function() {
  # 同时检查当前目录和当前目录下的 YieldCurve_Shiny，
  # 让用户从项目内或仓库根目录运行时都能找到 app.R。
  candidates <- unique(c(
    normalizePath(".", winslash = "/", mustWork = FALSE),
    normalizePath(file.path(".", "YieldCurve_Shiny"), winslash = "/", mustWork = FALSE)
  ))
  found <- candidates[file.exists(file.path(candidates, "app.R"))]
  if (length(found) == 0) stop("Cannot locate YieldCurve_Shiny project directory.", call. = FALSE)
  found[[1]]
}

source_data_paths <- function(project_dir = find_project_dir()) {
  # 所有源文件路径都从项目位置向上推导，避免写死某台电脑的盘符路径。
  repo_dir <- normalizePath(file.path(project_dir, ".."), winslash = "/", mustWork = TRUE)
  list(
    wide_rates = file.path(repo_dir, "R_Union", "WIDE_RATES"),
    zero_curve = file.path(repo_dir, "R_Union", "ZERORATE_CURVE"),
    original_script = file.path(repo_dir, "R_Union", "YieldCurve.R")
  )
}

load_market_data <- function(project_dir = find_project_dir()) {
  # 每次启动或点击 Refresh local RDS 都会调用这里。
  # 返回的 list 被 app.R 保存进 market()，随后所有页面共享同一份数据。
  paths <- source_data_paths(project_dir)
  missing <- names(paths)[!file.exists(unlist(paths))]
  if (length(missing) > 0) stop("Missing source files: ", paste(missing, collapse = ", "), call. = FALSE)

  # WIDE_RATES 保存时可能带有 data.table class。这里统一转成 data.frame，
  # 让后续的 [row, columns] 在是否加载 data.table package 时都保持同一种行为。
  wide_rates <- as.data.frame(readRDS(paths$wide_rates), check.names = FALSE)
  wide_rates$date <- as.Date(wide_rates$date)
  zero_curve <- as.data.frame(readRDS(paths$zero_curve), check.names = FALSE)
  list(
    wide_rates = wide_rates,
    zero_curve = zero_curve,
    paths = paths,
    loaded_at = Sys.time()
  )
}

parse_tenor_years <- function(labels) {
  # 从列名中的 1 WK、3 MO、10 YR 等文字提取期限，并统一换算成年。
  labels <- toupper(as.character(labels))
  values <- suppressWarnings(as.numeric(sub(".*?(\\d+(?:\\.\\d+)?)\\s*(WK|MO|YR).*", "\\1", labels)))
  units <- sub(".*?(WK|MO|YR).*", "\\1", labels)
  values[units == "WK"] <- values[units == "WK"] / 52
  values[units == "MO"] <- values[units == "MO"] / 12
  values[!units %in% c("WK", "MO", "YR")] <- NA_real_
  values
}

historical_curve_names <- function(wide_rates) {
  # 删除列名末尾的期限，统计每个曲线前缀拥有多少期限点；
  # 至少三个点的前缀才会出现在网页历史曲线下拉框中。
  names_without_date <- setdiff(names(wide_rates), "date")
  suffix <- "\\s+\\d+(?:\\.\\d+)?\\s*(?:WK|MO|YR)\\s*$"
  curve_names <- sub(suffix, "", names_without_date, perl = TRUE)
  valid <- grepl(suffix, names_without_date, perl = TRUE)
  counts <- sort(table(curve_names[valid]), decreasing = TRUE)
  names(counts[counts >= 3])
}

# resolve_historical_curve_date()
# 用户选的日期未必有该条曲线的报价。本函数从该日期开始向前寻找，
# 返回最近一个至少有三个有效期限点的日期，供拟合和历史比较继续使用。
resolve_historical_curve_date <- function(wide_rates, matched_columns, requested_date,
                                          minimum_points = 3) {
  requested_date <- as.Date(requested_date)
  eligible_rows <- which(wide_rates$date <= requested_date)
  if (length(eligible_rows) == 0) {
    stop("No historical data is available on or before ", requested_date, ".", call. = FALSE)
  }

  for (row_index in rev(eligible_rows)) {
    values <- suppressWarnings(as.numeric(unlist(
      wide_rates[row_index, matched_columns, drop = FALSE],
      use.names = FALSE
    )))
    if (sum(is.finite(values)) >= minimum_points) {
      return(list(row_index = row_index, effective_date = wide_rates$date[[row_index]]))
    }
  }

  stop("No date on or before ", requested_date, " has enough valid observations for this curve.", call. = FALSE)
}

extract_historical_curve <- function(wide_rates, curve_name, date) {
  # 正常网页路线传入的已经是 data.frame；这里再次标准化，是为了让该公共函数
  # 被单独调用并直接收到 data.table 时，也不会触发 data.table 的 j 选列规则。
  wide_rates <- as.data.frame(wide_rates, check.names = FALSE)
  wide_rates$date <- as.Date(wide_rates$date)
  requested_date <- as.Date(date)
  matched_columns <- names(wide_rates)[startsWith(names(wide_rates), paste0(curve_name, " "))]
  if (length(matched_columns) < 3) stop("Curve has fewer than three tenor columns.", call. = FALSE)

  # matched_columns 是运行时生成的列名向量。由于 load_market_data() 已统一为
  # data.frame，这里的动态选列不会再被 data.table 当成名为 matched_columns 的单列。
  resolved <- resolve_historical_curve_date(wide_rates, matched_columns, requested_date)
  values <- suppressWarnings(as.numeric(unlist(
    wide_rates[resolved$row_index, matched_columns, drop = FALSE],
    use.names = FALSE
  )))
  points <- clean_curve_points(parse_tenor_years(matched_columns), market_percent_to_decimal(values))
  if (nrow(points) < 3) stop("Not enough valid observations for this curve and date.", call. = FALSE)
  attr(points, "requested_date") <- requested_date
  attr(points, "effective_date") <- as.Date(resolved$effective_date)
  points
}

zero_curve_names <- function(zero_curve) {
  # ZERORATE_CURVE 是长表，DES 列就是用户可选择的正式零息曲线名称。
  sort(unique(as.character(zero_curve$DES[!is.na(zero_curve$DES)])))
}

extract_zero_curve <- function(zero_curve, curve_name) {
  # PX_MID 原始单位是百分数，这里转换成内部小数后返回标准曲线点。
  selected <- zero_curve[zero_curve$DES == curve_name, , drop = FALSE]
  points <- clean_curve_points(selected$MTY_YEARS_TDY, market_percent_to_decimal(selected$PX_MID))
  if (nrow(points) < 4) stop("Not enough valid zero-rate observations for this curve.", call. = FALSE)
  points
}

curve_source_label <- function(mode, curve_name, date = NULL) {
  # 这个标签跟随拟合结果进入 Forward、Carry 和 Diagnostics，
  # 用于区分正式零息快照与历史报价 Proxy。
  if (identical(mode, "zero")) return(paste0("ZERORATE_CURVE | ", curve_name))
  paste0("WIDE_RATES Proxy | ", curve_name, " | ", as.Date(date))
}

# prepare_curve_fit()
# Forward、Carry 和 Curve Trade 各自传入页面选择，本函数统一提取并拟合曲线。
# 返回 points、fit、请求日期和实际日期，避免不同页面产生不同口径。
prepare_curve_fit <- function(market, mode, curve_name, date = NULL,
                              method = c("nelson_siegel", "spline")) {
  method <- match.arg(method)
  if (identical(mode, "zero")) {
    points <- extract_zero_curve(market$zero_curve, curve_name)
    requested_date <- as.Date(NA)
    effective_date <- as.Date(NA)
  } else {
    points <- extract_historical_curve(market$wide_rates, curve_name, date)
    requested_date <- attr(points, "requested_date")
    effective_date <- attr(points, "effective_date")
  }
  source <- curve_source_label(mode, curve_name, date)
  list(
    points = points,
    fit = fit_curve(points$tenor, points$rate, method, source = source, proxy = identical(mode, "historical")),
    mode = mode,
    curve_name = curve_name,
    requested_date = requested_date,
    effective_date = effective_date,
    source = source,
    proxy = identical(mode, "historical")
  )
}

# build_history_comparison()
# 对每一个 curve × date 组合提取历史报价，并以每条曲线自己的 base_date 为基准
# 计算相同 tenor 的变化。返回长表，直接用于多曲线、多日期图和明细表。
build_history_comparison <- function(wide_rates, curve_names, dates, base_date) {
  curve_names <- unique(as.character(curve_names))
  dates <- sort(unique(as.Date(dates)))
  base_date <- as.Date(base_date)
  if (length(curve_names) == 0 || length(dates) == 0) stop("Select at least one curve and one date.", call. = FALSE)
  if (!base_date %in% dates) stop("Base date must be one of the selected dates.", call. = FALSE)

  rows <- list()
  for (curve_name in curve_names) {
    base_points <- extract_historical_curve(wide_rates, curve_name, base_date)
    names(base_points)[names(base_points) == "rate"] <- "base_rate"
    for (date_index in seq_along(dates)) {
      requested_date <- dates[[date_index]]
      points <- extract_historical_curve(wide_rates, curve_name, requested_date)
      effective_date <- attr(points, "effective_date")
      merged <- merge(points, base_points, by = "tenor", all.x = TRUE)
      rows[[length(rows) + 1]] <- data.frame(
        curve = curve_name,
        requested_date = as.Date(requested_date),
        effective_date = as.Date(effective_date),
        base_requested_date = base_date,
        base_effective_date = as.Date(attr(base_points, "effective_date")),
        tenor = merged$tenor,
        rate_percent = decimal_to_percent(merged$rate),
        change_bp = decimal_to_bp(merged$rate - merged$base_rate),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
