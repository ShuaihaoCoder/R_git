# ============================================================
# YieldCurve_Shiny 启动入口
# ============================================================
# 推荐直接运行这个文件。它先确定项目目录、准备 packages，
# 最后调用 shiny::runApp()，浏览器中的页面逻辑则来自 app.R。

find_launcher_project_dir <- function() {
  # Rscript 启动时 --file= 会记录当前脚本路径；从 R console 启动时则检查工作目录。
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
  candidates <- c(
    if (length(file_arg)) dirname(normalizePath(file_arg[[1]], winslash = "/", mustWork = FALSE)),
    normalizePath(".", winslash = "/", mustWork = FALSE),
    normalizePath(file.path(".", "YieldCurve_Shiny"), winslash = "/", mustWork = FALSE)
  )
  found <- unique(candidates)[file.exists(file.path(unique(candidates), "app.R"))]
  if (length(found) == 0) stop("Cannot locate YieldCurve_Shiny.", call. = FALSE)
  found[[1]]
}

project_dir <- find_launcher_project_dir()

# packages.R 中的函数先准备项目 package 环境，确保 app.R 可以加载 Shiny/Plotly/DT。
source(file.path(project_dir, "R", "packages.R"))
install_and_load_packages(project_dir)

message("YieldCurve Trader Dashboard")
message("Project: ", project_dir)
message("R: ", R.version.string)
shiny::runApp(project_dir, launch.browser = TRUE)
