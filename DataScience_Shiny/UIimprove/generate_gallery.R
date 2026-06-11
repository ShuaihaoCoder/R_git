# ============================================================
# DataScience 原始图表图册生成器
# ============================================================
# 整体作用：逐段运行优化后的原始参考脚本，保存所有可生成的图，并与当前 App 图片一起写入 HTML。
# 某个原始步骤失败时，本文件会记录错误后继续运行，避免一张失败图阻断整个图册。

# find_project_dir()
# 功能：从当前目录或本文件位置找到 DataScience_Shiny 项目文件夹。
find_project_dir <- function() {
  source_file <- tryCatch(sys.frames()[[1]]$ofile, error = function(error) NULL)
  candidates <- unique(c(
    if (!is.null(source_file)) file.path(dirname(source_file), ".."),
    getwd(),
    file.path(getwd(), "DataScience_Shiny")
  ))
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  valid <- candidates[file.exists(file.path(candidates, "app.R"))]
  if (length(valid) == 0) stop("Cannot locate DataScience_Shiny.", call. = FALSE)
  valid[[1]]
}

project_dir <- find_project_dir()
ui_dir <- file.path(project_dir, "UIimprove")
image_dir <- file.path(ui_dir, "images")
dir.create(image_dir, recursive = TRUE, showWarnings = FALSE)
setwd(project_dir)

source(file.path(project_dir, "R", "packages.R"), encoding = "UTF-8")
use_project_library(project_dir)
load_required_packages()
source(file.path(project_dir, "R", "catalog.R"), encoding = "UTF-8")

reference_original <- file.path(project_dir, "DataScience_original_reference.R")
reference_optimized <- file.path(ui_dir, "DataScience_reference_optimized.R")
method_catalog <- get_method_catalog()
source_map <- get_source_method_map()

# is_plot_call()
# 功能：判断一个函数调用是否会直接创建图；递归检查可以识别 pipe 或赋值语句中的 ggplot()。
is_plot_call <- function(expression) {
  if (!is.call(expression)) return(FALSE)
  function_name <- as.character(expression[[1]])[[1]]
  plot_functions <- c(
    "plot", "plot.ts", "autoplot", "qplot", "ggplot", "boxplot", "barplot",
    "biplot", "corrplot", "ggcorrplot", "acf", "pacf", "ggAcf", "ggPacf",
    "monthplot", "interaction.plot", "fviz_eig", "fviz_pca_ind",
    "fviz_pca_biplot", "ggbiplot", "mosaic"
  )
  if (function_name %in% plot_functions) return(TRUE)
  any(vapply(as.list(expression)[-1], is_plot_call, logical(1)))
}

# is_plot_decoration()
# 功能：识别只给上一张 base plot 添加线、点、文字或图例的语句；单独保存图片时没有上一张图可装饰。
is_plot_decoration <- function(expression) {
  if (!is.call(expression)) return(FALSE)
  as.character(expression[[1]])[[1]] %in% c("abline", "points", "lines", "text", "legend", "qqline")
}

# expression_name()
# 功能：取得图对象赋值名称；没有名称的直接绘图使用 source_plot_编号。
expression_name <- function(expression, index) {
  if (is.call(expression) && as.character(expression[[1]])[[1]] %in% c("<-", "=")) {
    return(as.character(expression[[2]])[[1]])
  }
  sprintf("source_plot_%03d", index)
}

# infer_method_id()
# 功能：根据原始行号和绘图代码，将共享代码段中的图片分配给最贴近的网页方法。
infer_method_id <- function(line_number, code) {
  range_contains <- function(range_text) {
    limits <- as.integer(strsplit(range_text, "-", fixed = TRUE)[[1]])
    line_number >= limits[[1]] && line_number <= limits[[2]]
  }
  candidates <- unique(source_map$method_id[vapply(source_map$source_lines, range_contains, logical(1))])
  lower_code <- tolower(code)

  keyword_rules <- c(
    polynomial_regression = "i\\(|polynomial",
    subset_regression = "regsubsets|leaps|subset",
    partial_correlation = "pcor|partial",
    confusion_matrix = "confusion|predicted.*actual",
    roc = "roc|fpr|tpr|sensitivity",
    sarima = "sarima|season|monthplot|stl",
    garch = "garch|arch|squared|volatility",
    granger = "granger|irf|fevd|impulse",
    rolling_pca = "rolling|regime"
  )
  for (method_id in names(keyword_rules)) {
    if (method_id %in% candidates && grepl(keyword_rules[[method_id]], lower_code)) return(method_id)
  }
  if (length(candidates) > 0) return(candidates[[1]])
  "unmapped"
}

# describe_plot()
# 功能：为图册中的原始图提供短说明，帮助之后判断是否需要加入 Encyclopedia。
describe_plot <- function(method_id, code) {
  lower_code <- tolower(code)
  if (grepl("corrplot|ggcorrplot|heat|geom_tile", lower_code)) {
    return("颜色表示关系的方向和强度；格子或数字用于比较变量之间最强和最弱的联系。")
  }
  if (grepl("acf|pacf", lower_code)) {
    return("柱线表示不同滞后期的相关性；明显超出参考范围的柱线说明序列仍有可解释结构。")
  }
  if (grepl("boxplot|violin|density|ridge", lower_code)) {
    return("形状展示不同组的分布；箱体、密度宽度或山脊高度用于比较中心、离散程度和尾部。")
  }
  if (grepl("forecast|fitted|actual|predicted", lower_code)) {
    return("线或点比较模型结果与实际数据；两者越接近，模型在该部分的拟合或预测越一致。")
  }
  if (grepl("roc|fpr|tpr", lower_code)) {
    return("曲线展示不同分类阈值下的真阳性率与假阳性率；越靠近左上角通常代表区分能力越好。")
  }
  if (grepl("biplot|loading|factor|pca", lower_code)) {
    return("点表示样本或时点，箭头、颜色或位置表示变量对主成分或因子的贡献方向。")
  }
  if (grepl("irf|impulse|fevd|granger", lower_code)) {
    return("图形展示变量之间的动态影响；线、面积或柱高表示冲击强度、贡献或预测关系。")
  }
  paste0("该图来自原始 ", method_id, " 分析段。阅读时结合坐标轴、颜色图例和对应检验结果。")
}

# save_result_plot()
# 功能：把当前表达式生成的 base plot 或 ggplot 保存为 PNG；成功时返回 TRUE。
save_result_plot <- function(expression, environment, output_file) {
  grDevices::png(output_file, width = 1500, height = 950, res = 140)
  device_open <- TRUE
  on.exit(if (device_open) grDevices::dev.off(), add = TRUE)
  result <- eval(expression, envir = environment)
  if (inherits(result, c("ggplot", "ggplot2::ggplot", "grob", "gtable", "patchwork"))) print(result)
  grDevices::dev.off()
  device_open <- FALSE
  file.exists(output_file) && file.info(output_file)$size > 1000
}

# build_reference_gallery()
# 功能：顺序运行原始优化脚本，并为每个有效绘图表达式生成图片和 manifest 记录。
build_reference_gallery <- function() {
  original_lines <- readLines(reference_original, encoding = "UTF-8", warn = FALSE)
  active_plot_lines <- grep(
    "ggplot\\(|plot\\(|autoplot\\(|qplot\\(|corrplot\\(|ggcorrplot\\(|acf\\(|pacf\\(|ggAcf\\(|ggPacf\\(|boxplot\\(|barplot\\(|biplot\\(|monthplot\\(|interaction.plot\\(|fviz_|ggbiplot\\(|mosaic\\(",
    original_lines
  )
  active_plot_lines <- active_plot_lines[!grepl("^\\s*#", original_lines[active_plot_lines])]

  expressions <- parse(reference_optimized, keep.source = TRUE, encoding = "UTF-8")
  expression_refs <- attr(expressions, "srcref")
  plot_expression_indexes <- which(vapply(expressions, is_plot_call, logical(1)))
  run_environment <- new.env(parent = globalenv())
  records <- list()
  execution_errors <- list()
  plot_counter <- 0L

  for (expression_index in seq_along(expressions)) {
    expression <- expressions[[expression_index]]
    expression_line <- if (length(expression_refs) >= expression_index) expression_refs[[expression_index]][[1]] else NA_integer_
    if (!expression_index %in% plot_expression_indexes) {
      tryCatch(
        eval(expression, envir = run_environment),
        error = function(error) {
          # 这些语句依赖紧邻的上一张 base plot；对应完整图已经由前一个绘图表达式保存。
          if (is_plot_decoration(expression) && grepl("plot.new has not been called yet", conditionMessage(error), fixed = TRUE)) {
            return(invisible(NULL))
          }
          execution_errors[[length(execution_errors) + 1L]] <<- data.frame(
            optimized_line = expression_line,
            code = substr(paste(deparse(expression), collapse = " "), 1, 500),
            error = conditionMessage(error),
            stringsAsFactors = FALSE
          )
        }
      )
      next
    }

    plot_counter <- plot_counter + 1L
    # 优化副本保留原文件的整体行序；直接使用表达式起始行，比按绘图调用顺序匹配多行 ggplot 更准确。
    original_line <- expression_line
    original_code <- if (is.na(original_line)) paste(deparse(expression), collapse = " ") else trimws(original_lines[[original_line]])
    method_id <- if (is.na(original_line)) "unmapped" else infer_method_id(original_line, original_code)
    plot_name <- expression_name(expression, plot_counter)
    safe_name <- gsub("[^A-Za-z0-9_-]+", "_", plot_name)
    relative_image <- file.path("images", sprintf("original_%03d_%s.png", plot_counter, safe_name))
    output_file <- file.path(ui_dir, relative_image)

    error_text <- ""
    status <- tryCatch(
      if (save_result_plot(expression, run_environment, output_file)) "success" else "empty_image",
      error = function(error) {
        error_text <<- conditionMessage(error)
        try(grDevices::dev.off(), silent = TRUE)
        "failed_explained"
      }
    )
    records[[length(records) + 1L]] <- data.frame(
      method_id = method_id,
      method_name = method_catalog$method_name[match(method_id, method_catalog$method_id)] %||% method_id,
      source = "Original Reference",
      source_line = original_line,
      plot_name = plot_name,
      image_path = if (status == "success") gsub("\\\\", "/", relative_image) else "",
      status = status,
      explanation = describe_plot(method_id, original_code),
      code = original_code,
      error = error_text,
      stringsAsFactors = FALSE
    )
  }
  execution_error_table <- if (length(execution_errors) == 0) {
    data.frame(optimized_line = integer(), code = character(), error = character())
  } else {
    do.call(rbind, execution_errors)
  }
  utils::write.csv(execution_error_table, file.path(ui_dir, "execution_errors.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  do.call(rbind, records)
}

# build_current_app_gallery()
# 功能：运行当前 App 的 24 个缓存案例，并把现有图导出到图册作为对比。
build_current_app_gallery <- function() {
  source(file.path(project_dir, "app.R"), encoding = "UTF-8", local = globalenv())
  records <- list()
  for (method_id in method_catalog$method_id) {
    case <- get(method_id, envir = example_cache, inherits = FALSE)
    for (plot_index in seq_along(case$plots)) {
      plot_name <- names(case$plots)[[plot_index]]
      relative_image <- file.path("images", sprintf("app_%s_%02d.png", method_id, plot_index))
      output_file <- file.path(ui_dir, relative_image)
      ggplot2::ggsave(output_file, case$plots[[plot_index]], width = 10, height = 6.4, dpi = 140)
      records[[length(records) + 1L]] <- data.frame(
        method_id = method_id,
        method_name = method_catalog$method_name[match(method_id, method_catalog$method_id)],
        source = "Current App",
        source_line = NA_integer_,
        plot_name = plot_name,
        image_path = gsub("\\\\", "/", relative_image),
        status = "success",
        explanation = case$plot_notes[[plot_name]],
        code = "Current App plot from R/examples_complete.R",
        error = "",
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, records)
}

# html_escape()
# 功能：把代码和错误文字中的特殊符号转换为安全 HTML，避免破坏图册页面。
html_escape <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text
}

# write_gallery_html()
# 功能：按 24 个方法把原始图、当前 App 图和失败说明写入一个可浏览 HTML。
write_gallery_html <- function(manifest) {
  sections <- vapply(seq_len(nrow(method_catalog)), function(index) {
    method_id <- method_catalog$method_id[[index]]
    rows <- manifest[manifest$method_id == method_id, , drop = FALSE]
    cards <- if (nrow(rows) == 0) {
      "<div class='empty'>原始脚本没有该方法的独立有效图；请查看 Current App 图或后续新增案例。</div>"
    } else {
      paste(vapply(seq_len(nrow(rows)), function(row_index) {
        row <- rows[row_index, ]
        image <- if (nzchar(row$image_path)) {
          sprintf("<img loading='lazy' src='%s' alt='%s'>", row$image_path, html_escape(row$plot_name))
        } else {
          sprintf("<div class='failed'>未生成图片：%s</div>", html_escape(row$error))
        }
        sprintf(
          "<article class='plot-card'><div class='badge'>%s</div><h3>%s</h3>%s<p>%s</p><dl><dt>原始行号</dt><dd>%s</dd><dt>状态</dt><dd>%s</dd></dl><details><summary>查看绘图代码</summary><pre>%s</pre></details></article>",
          row$source, html_escape(row$plot_name), image, html_escape(row$explanation),
          ifelse(is.na(row$source_line), "-", row$source_line), row$status, html_escape(row$code)
        )
      }, character(1)), collapse = "\n")
    }
    sprintf("<section id='%s'><h2>%s</h2><div class='grid'>%s</div></section>", method_id, method_catalog$method_name[[index]], cards)
  }, character(1))

  original_lines <- readLines(reference_original, encoding = "UTF-8", warn = FALSE)
  inactive <- grep("^\\s*#.*(ggplot\\(|plot\\(|autoplot\\(|qplot\\(|corrplot\\(|acf\\(|pacf\\(|biplot\\(|fviz_)", original_lines, value = TRUE)
  inactive_html <- paste(sprintf("<li><code>%s</code></li>", html_escape(trimws(inactive))), collapse = "\n")
  nav <- paste(sprintf("<a href='#%s'>%s</a>", method_catalog$method_id, method_catalog$method_name), collapse = "\n")

  html <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>DataScience Reference Plot Gallery</title>",
    "<style>body{font-family:Segoe UI,Arial,sans-serif;margin:0;color:#24313a;background:#f4f7f8}header{padding:28px 34px;background:#17324d;color:white}nav{display:flex;flex-wrap:wrap;gap:8px;padding:14px 24px;position:sticky;top:0;background:white;border-bottom:1px solid #ccd6dc;z-index:3}nav a{color:#24547a;text-decoration:none;padding:5px 8px;border:1px solid #ccd6dc;border-radius:4px}section{padding:22px 28px}h2{border-bottom:2px solid #d08c60;padding-bottom:8px}.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(430px,1fr));gap:18px}.plot-card{background:white;border:1px solid #d7e0e5;border-radius:6px;padding:14px}.plot-card img{width:100%;height:auto;border:1px solid #e4eaed}.badge{display:inline-block;background:#e6f1f6;color:#24547a;padding:3px 7px;border-radius:3px;font-size:12px}.failed{padding:40px;background:#fff1f0;color:#9b2c2c}.empty{background:white;padding:22px}pre{white-space:pre-wrap;overflow-wrap:anywhere;background:#f5f7f8;padding:10px}dl{display:grid;grid-template-columns:90px 1fr;font-size:13px}dt{font-weight:600}</style></head><body>",
    "<header><h1>DataScience Original Reference Plot Gallery</h1><p>原始参考图与当前 App 图对比。请按方法选择需要正式加入 Encyclopedia 的图片。</p></header>",
    "<nav>", nav, "</nav>", paste(sections, collapse = "\n"),
    "<section><h2>Inactive Plot Candidates</h2><p>这些绘图代码在原始文件中被注释，因此不计入必须生成的有效图。</p><ul>", inactive_html, "</ul></section>",
    "</body></html>"
  )
  writeLines(html, file.path(ui_dir, "reference_plot_gallery.html"), useBytes = TRUE)
}

message("Step 1/4: Running original reference plots...")
reference_manifest <- build_reference_gallery()
message("Step 2/4: Exporting current App plots...")
app_manifest <- build_current_app_gallery()
manifest <- rbind(reference_manifest, app_manifest)

message("Step 3/4: Writing manifest...")
utils::write.csv(manifest, file.path(ui_dir, "plot_manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")
message("Step 4/4: Writing HTML gallery...")
write_gallery_html(manifest)

message("Gallery complete: ", file.path(ui_dir, "reference_plot_gallery.html"))
message("Original successful plots: ", sum(reference_manifest$status == "success"), "/", nrow(reference_manifest))
message("Current App plots: ", nrow(app_manifest))
