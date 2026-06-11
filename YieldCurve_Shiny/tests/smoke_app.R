# ============================================================
# HTTP smoke test 启动器
# ============================================================
# 这个文件不打开浏览器，只在指定端口启动 app。
# 自动化验证随后请求该端口，确认网页至少能够真正返回 HTTP 200。

args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) as.integer(args[[1]]) else 7421L

command_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", command_args[grepl("^--file=", command_args)])
script_path <- if (length(file_arg)) file_arg[[1]] else file.path("YieldCurve_Shiny", "tests", "smoke_app.R")
project_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

setwd(project_dir)
source(file.path("R", "packages.R"))
install_and_load_packages(project_dir)
shiny::runApp(project_dir, port = port, host = "127.0.0.1", launch.browser = FALSE)
