# ============================================================
# DataScience_Shiny 启动入口
# ============================================================
# 整体作用：这是启动网页的第一个文件。它先找到项目文件夹、准备需要的 packages，
# 最后启动 app.R。可以在 R 中用 source()，也可以在 command line 中用 Rscript.exe 运行。

# find_project_dir()
# 作用：寻找包含 app.R 的 DataScience_Shiny 文件夹。
# 返回：项目文件夹的完整路径，例如 "C:/Users/PC/Desktop/R_git/DataScience_Shiny"。
find_project_dir <- function() {
  # 当通过 command line 输入 Rscript.exe ".../run_app.R" 启动时，
  # commandArgs() 可以取得启动时提供的文件路径等信息，grep() 再找出以 "--file=" 开头的路径。
  # trailingOnly = FALSE 表示查看全部启动信息；value = TRUE 表示返回找到的内容，而不是位置。
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

  # 如果只找到一条文件路径信息，sub() 删除其中的 "--file="，留下实际的 .R 文件路径。
  # 如果没有找到，显示 NULL，说明用户是在 R console 中用 source() 运行。
  rscript_file <- if (length(script_arg) == 1) {
    sub("^--file=", "", script_arg)
  } else {
    NULL
  }

  # 使用 source() 运行文件时，R 通常会记录正在运行的文件路径；
  # sys.frames()[[1]]$ofile 尝试取出这条记录，取值失败时 tryCatch() 会改为返回 NULL。
  source_file <- tryCatch(
    sys.frames()[[1]]$ofile,
    error = function(error) NULL
  )

  # 将可能的项目文件夹放在一起，再用 unique() 删除重复项。
  # dirname() 取文件所在文件夹；getwd() 是当前工作文件夹；file.path() 在当前文件夹后拼接子文件夹名。
  candidate_dirs <- unique(c(
    if (!is.null(rscript_file)) dirname(rscript_file),
    if (!is.null(source_file)) dirname(source_file),
    getwd(),
    file.path(getwd(), "DataScience_Shiny")
  ))

  # 删除 NA 和空字符串，只保留有内容的候选路径。
  # nzchar() 判断字符串是否有内容；前面的 ! 表示排除 NA。
  candidate_dirs <- candidate_dirs[!is.na(candidate_dirs) & nzchar(candidate_dirs)]

  # vapply() 对每条候选路径运行 normalizePath()；例如把 "./DataScience_Shiny" 转成
  # "C:/Users/PC/Desktop/R_git/DataScience_Shiny"。character(1) 要求每次返回一个字符串。
  # winslash = "/" 让 Windows 路径使用正斜杠；mustWork = FALSE 表示路径不存在时暂不报错。
  candidate_dirs <- vapply(
    candidate_dirs,
    normalizePath,
    character(1),
    winslash = "/",
    mustWork = FALSE
  )

  # 为每个候选文件夹拼接 "app.R"，再用 file.exists() 只保留真正存在 app.R 的文件夹。
  app_dirs <- candidate_dirs[file.exists(file.path(candidate_dirs, "app.R"))]

  # 如果一个有效项目文件夹都没找到，stop() 停止程序并显示当前工作文件夹。
  # call. = FALSE 表示错误信息中不附加类似 "Error in find_project_dir()" 的调用来源。
  if (length(app_dirs) == 0) {
    stop(
      "Cannot locate DataScience_Shiny/app.R. Current working directory: ",
      normalizePath(getwd(), winslash = "/", mustWork = FALSE),
      call. = FALSE
    )
  }

  # 返回找到的第一个有效项目文件夹。
  app_dirs[[1]]
}

# 调用上面的 find_project_dir()，保存项目文件夹路径。
project_dir <- find_project_dir()

# source() 运行 packages.R，使该文件中定义的 package 管理函数可以在这里使用。
# encoding = "UTF-8" 让 R 按 UTF-8 读取文件，避免中文注释乱码。
source(file.path(project_dir, "R", "packages.R"), encoding = "UTF-8")

# 这两个函数来自 R/packages.R。use_project_library() 会把 project_dir/R_library/R-当前版本
# 放到 .libPaths() 最前面，让 R 优先从该文件夹寻找 packages；后者把缺少的 packages 安装进去。
use_project_library(project_dir)
install_missing_packages(required_packages, project_dir)

# message() 在 console 显示项目路径、R 版本和 package 文件夹，方便确认实际运行环境。
message("DataScience_Shiny project: ", project_dir)
message("R version: ", R.version.string)
message("Primary project library: ", project_library_path(project_dir))

# open_in_chrome()
# 作用：Shiny 准备完成后优先用本机 Chrome 打开网页；找不到 Chrome 时改用系统默认浏览器。
open_in_chrome <- function(url) {
  chrome_candidates <- c(
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Google", "Chrome", "Application", "chrome.exe")
  )
  chrome_path <- chrome_candidates[file.exists(chrome_candidates)][1]

  if (!is.na(chrome_path)) {
    system2(chrome_path, args = url, wait = FALSE)
  } else {
    utils::browseURL(url)
  }
}

# 启动 project_dir 中的 Shiny 网页；host 和 port 组成访问地址 http://127.0.0.1:7411。
# launch.browser 使用上面的函数，等全部案例预计算完成后自动打开 Chrome。
shiny::runApp(project_dir, host = "127.0.0.1", port = 7411, launch.browser = open_in_chrome)
