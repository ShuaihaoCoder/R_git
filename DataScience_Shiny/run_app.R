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

# loaded_package_conflicts()
# 作用：检查当前 R session 里是否已经加载了会锁住 namespace 的高风险 package。
# 为什么需要：VSCode 里反复 source 文件时，只要 plotly/data.table 等已经加载，
# 后续 package 检查或加载就可能触发 data.table unload 失败。
loaded_package_conflicts <- function(packages, project_dir, high_risk_packages = c("plotly", "data.table", "jsonlite", "ggplot2", "DT", "bslib", "shiny", "visNetwork")) {
  project_library <- normalizePath(project_library_path(project_dir), winslash = "/", mustWork = FALSE)
  loaded <- loadedNamespaces()
  already_loaded_high_risk <- intersect(intersect(packages, high_risk_packages), loaded)
  high_risk_conflicts <- vapply(
    already_loaded_high_risk,
    function(package) {
      package_path <- tryCatch(
        normalizePath(find.package(package), winslash = "/", mustWork = FALSE),
        error = function(error) "<unknown library path>"
      )
      paste0(package, " is already loaded from ", package_path)
    },
    character(1)
  )

  external_conflicts <- vapply(
    setdiff(intersect(packages, loaded), already_loaded_high_risk),
    function(package) {
      package_path <- tryCatch(
        normalizePath(find.package(package), winslash = "/", mustWork = FALSE),
        error = function(error) ""
      )
      if (!nzchar(package_path)) return("")
      if (startsWith(package_path, project_library)) return("")
      paste0(package, " loaded from ", package_path)
    },
    character(1)
  )
  conflicts <- c(high_risk_conflicts, external_conflicts)
  conflicts[nzchar(conflicts)]
}

# run_in_clean_rscript()
# 作用：当前 R session 已经有 package 冲突时，另外开一个干净 Rscript 进程启动网页。
# 这样你仍然 source 这个文件，但真正的 Shiny 会在没有旧 package 占用的新 R 进程里运行。
# wait = TRUE 会把 clean Rscript 的预计算进度直接显示在 VSCode console 中。
run_in_clean_rscript <- function(project_dir) {
  script_path <- file.path(project_dir, "run_app.R")
  rscript_path <- file.path(R.home("bin"), "Rscript.exe")
  if (!file.exists(rscript_path)) {
    rscript_path <- file.path(R.home("bin"), "Rscript")
  }
  log_dir <- file.path(project_dir, "logs")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(log_dir, "run_app_clean_child.log")
  runner_path <- file.path(log_dir, "run_app_clean_child.ps1")
  ps_quote <- function(path) {
    paste0("'", gsub("'", "''", normalizePath(path, winslash = "/", mustWork = FALSE)), "'")
  }
  message("Current VSCode R session already has package conflicts.")
  message("Starting DataScience_Shiny in a clean Rscript process instead...")
  message("Progress and errors will also be written to: ", normalizePath(log_path, winslash = "/", mustWork = FALSE))
  message("The console should show case precompute progress and then stay busy while Shiny is running.")
  if (identical(Sys.getenv("DATASCIENCE_SHINY_CLEAN_DRY_RUN"), "1")) {
    message("Dry run only: clean Rscript launch was skipped.")
    return(invisible(TRUE))
  }

  # The tiny PowerShell runner lets the clean Rscript output appear in the console
  # and saves the same output to a log file for debugging if the child process exits.
  writeLines(
    c(
      "$ErrorActionPreference = 'Continue'",
      "$env:DATASCIENCE_SHINY_CLEAN_CHILD = '1'",
      paste0("$logPath = ", ps_quote(log_path)),
      "\"Starting clean DataScience_Shiny child process...\" | Set-Content -Path $logPath -Encoding UTF8",
      paste0("& ", ps_quote(rscript_path), " ", ps_quote(script_path), " 2>&1 | Tee-Object -FilePath $logPath -Append"),
      "exit $LASTEXITCODE"
    ),
    runner_path,
    useBytes = TRUE
  )

  status <- system2(
    "powershell.exe",
    args = c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(runner_path)),
    wait = TRUE
  )
  if (!identical(status, 0L)) {
    stop(
      "Clean Rscript process exited early. Check log: ",
      normalizePath(log_path, winslash = "/", mustWork = FALSE),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

conflicting_packages <- loaded_package_conflicts(required_packages, project_dir)
run_current_session <- TRUE
if (length(conflicting_packages) > 0 && !identical(Sys.getenv("DATASCIENCE_SHINY_CLEAN_CHILD"), "1")) {
  message("Detected loaded packages that cannot be safely replaced inside this R session:")
  message(paste("- ", conflicting_packages, collapse = "\n"))
  run_in_clean_rscript(project_dir)
  run_current_session <- FALSE
}

if (run_current_session) {
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
}
