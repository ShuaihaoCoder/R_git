# 中文说明：
# 用这个文件启动项目本地 Shiny app。它会先加入 R_library，
# 避免系统 R 没有安装 shiny 时无法调用 shiny::runApp。

find_project_dir <- function() {
  # 中文说明：
  # Rscript 启动时从 --file 获取路径；RStudio source() 启动时从 ofile 获取路径。
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  rscript_file <- if (length(script_arg) == 1) {
    sub("^--file=", "", script_arg)
  } else {
    NULL
  }

  source_file <- tryCatch(
    sys.frames()[[1]]$ofile,
    error = function(error) NULL
  )

  candidate_dirs <- unique(c(
    if (!is.null(rscript_file)) dirname(rscript_file),
    if (!is.null(source_file)) dirname(source_file),
    getwd(),
    file.path(getwd(), "DataScience_Shiny")
  ))

  candidate_dirs <- candidate_dirs[!is.na(candidate_dirs) & nzchar(candidate_dirs)]
  candidate_dirs <- vapply(
    candidate_dirs,
    normalizePath,
    character(1),
    winslash = "/",
    mustWork = FALSE
  )

  app_dirs <- candidate_dirs[file.exists(file.path(candidate_dirs, "app.R"))]
  if (length(app_dirs) == 0) {
    stop(
      "Cannot locate DataScience_Shiny/app.R. Current working directory: ",
      normalizePath(getwd(), winslash = "/", mustWork = FALSE),
      call. = FALSE
    )
  }

  app_dirs[[1]]
}

project_dir <- find_project_dir()

source(file.path(project_dir, "R", "packages.R"))
use_project_library(project_dir)

message("DataScience_Shiny project: ", project_dir)
message("R version: ", R.version.string)
message("Primary project library: ", project_library_path(project_dir))

install_missing_packages(required_packages, project_dir)
shiny::runApp(project_dir, host = "127.0.0.1", port = 7411, launch.browser = FALSE)
