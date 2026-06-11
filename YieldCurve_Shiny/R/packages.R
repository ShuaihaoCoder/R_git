# ============================================================
# 项目 package 环境
# ============================================================
# 整体作用：让 run_app.R 先准备 Shiny 页面需要的 packages，再启动网页。
# 项目 library 排在最前面；若用户 library 已有 package，也允许直接复用。

required_packages <- c("shiny", "bslib", "DT", "ggplot2", "plotly")

project_library_path <- function(project_dir) {
  # 按 R 主版本和次版本分目录，例如 R 4.5.2 使用 R_library/R-4.5。
  version <- paste0("R-", R.version$major, ".", strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1])
  file.path(project_dir, "R_library", version)
}

use_project_library <- function(project_dir) {
  # .libPaths() 是当前 R session 寻找 packages 的目录顺序。
  # 把项目目录放在最前面，避免不同编辑器启动后使用完全不同的 package 环境。
  library_path <- project_library_path(project_dir)
  dir.create(library_path, recursive = TRUE, showWarnings = FALSE)
  user_library <- file.path(
    Sys.getenv("LOCALAPPDATA"),
    "R", "win-library",
    paste0(R.version$major, ".", strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1])
  )
  available_user_library <- if (dir.exists(user_library)) user_library else character()
  .libPaths(unique(c(
    normalizePath(library_path, winslash = "/", mustWork = TRUE),
    available_user_library,
    .libPaths()
  )))
  invisible(library_path)
}

install_and_load_packages <- function(project_dir) {
  # requireNamespace() 只检查 package 能否加载，不把函数名直接放进当前环境。
  # 真正缺少时才联网安装，避免每次启动重复下载。
  library_path <- use_project_library(project_dir)
  missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("Installing missing packages into ", library_path, ": ", paste(missing, collapse = ", "))
    options(timeout = max(600, getOption("timeout")))
    install.packages(
      missing,
      lib = library_path,
      repos = "https://cloud.r-project.org",
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }
  unavailable <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(unavailable) > 0) stop("Packages unavailable: ", paste(unavailable, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}
