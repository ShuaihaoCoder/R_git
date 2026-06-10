# ============================================================
# DataScience_Shiny 启动入口
# ============================================================
# 在整体项目中的作用：
# 这是用户从 VSCode / RStudio 启动网页时运行的第一个文件。
# 它负责定位项目、准备 package 环境，最后把控制权交给 app.R。

# find_project_dir()
# 功能：从多种启动方式中找到包含 app.R 的 DataScience_Shiny 根目录。
# 项目作用：后续读取 R/、data/ 和启动 app.R 都依赖这个返回路径。
# 输入：无。返回：项目根目录的绝对路径字符串。
find_project_dir <- function() {
  # 读取 R 的启动参数；Rscript 运行文件时通常包含 "--file=文件路径"。
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

  # 删除 "--file="，取得通过 Rscript 运行的文件路径；没有则返回 NULL。
  rscript_file <- if (length(script_arg) == 1) {
    sub("^--file=", "", script_arg)
  } else {
    NULL
  }

  # source() 运行文件时，尝试从调用环境取得当前文件路径；失败则返回 NULL。
  source_file <- tryCatch(
    sys.frames()[[1]]$ofile,
    error = function(error) NULL
  )

  # 汇总可能的项目目录，并删除重复路径。
  candidate_dirs <- unique(c(
    if (!is.null(rscript_file)) dirname(rscript_file),
    if (!is.null(source_file)) dirname(source_file),
    getwd(),
    file.path(getwd(), "DataScience_Shiny")
  ))

  # 删除 NA 和空字符串。
  candidate_dirs <- candidate_dirs[!is.na(candidate_dirs) & nzchar(candidate_dirs)]

  # 将所有候选目录转换为格式统一的绝对路径。
  candidate_dirs <- vapply(
    candidate_dirs,
    normalizePath,
    character(1),
    winslash = "/",
    mustWork = FALSE
  )

  # 只有包含 app.R 的目录才是有效 Shiny 项目目录。
  app_dirs <- candidate_dirs[file.exists(file.path(candidate_dirs, "app.R"))]

  # 如果找不到 app.R，停止运行并显示当前工作目录。
  if (length(app_dirs) == 0) {
    stop(
      "Cannot locate DataScience_Shiny/app.R. Current working directory: ",
      normalizePath(getwd(), winslash = "/", mustWork = FALSE),
      call. = FALSE
    )
  }

  # 返回第一个有效项目目录。
  app_dirs[[1]]
}

# 找到项目目录，并加载 package 管理函数。
project_dir <- find_project_dir()
source(file.path(project_dir, "R", "packages.R"), encoding = "UTF-8")

# 使用项目自己的 package 目录，并安装真正缺少的 packages。
use_project_library(project_dir)
install_missing_packages(required_packages, project_dir)

# 在 console 显示本次运行使用的路径和 R 版本。
message("DataScience_Shiny project: ", project_dir)
message("R version: ", R.version.string)
message("Primary project library: ", project_library_path(project_dir))

# 启动网页；浏览器访问 http://127.0.0.1:7411。
shiny::runApp(project_dir, host = "127.0.0.1", port = 7411, launch.browser = FALSE)
