# ============================================================
# DataScience_Shiny 启动入口
# ============================================================
# 在整体项目中的作用：
# 这是用户从 VSCode / RStudio 启动网页时运行的第一个文件。
# 它负责定位项目、准备 package 环境，最后把控制权交给 app.R。

# find_project_dir()
# 功能：从多种启动方式中找到包含 app.R 的 DataScience_Shiny 根目录。
# 项目作用：后续读取 R/、data/ 和启动 app.R 都依赖这个返回路径。
# 输入：无。返回：项目根目录的绝对路径字符串(用大白话说，啥叫绝对路径字符串，就是所对应的根目录的路径呗）。
find_project_dir <- function() {
  # 读取 R 的启动参数；Rscript 运行文件（就是xxx.R文件通过Rscript运行时）时通常包含 "--file=文件路径"。
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)#解释一下commandArgs()函数返回R的命令行参数（啥叫命令行参数），trailingOnly=FALSE表示返回所有参数。grep("^--file=", ...)表示从这些参数中找到以"--file="开头的参数，value=TRUE表示返回匹配的参数值（就是具体结果），而不是它们的位置（grep的解释可以放在下一行）。

  # 删除 "--file="，取得通过 Rscript 运行的文件路径；没有则返回 NULL。
  rscript_file <- if (length(script_arg) == 1) {
    sub("^--file=", "", script_arg)#如果找到了一个以"--file="开头的变量，就用sub()函数把这个参数中的"--file="部分替换成空字符串，得到实际的文件路径。
  } else {
    NULL
  }

  # source() 运行文件时，尝试从调用环境取得当前文件路径；失败则返回 NULL。
  source_file <- tryCatch(#tryCatch是干啥用的，这里是为了尝试获取通过source()运行的文件路径，如果失败了就返回NULL。
    sys.frames()[[1]]$ofile,#sys.frames()函数返回当前调用栈的所有环境(听不懂啥叫栈，是不是就是当前有多少个环境？，不要拽这种高端词汇，[[1]]表示取第一个环境，$ofile表示尝试获取这个环境中的ofile变量，这个变量通常在通过source()运行文件时会被设置为当前文件的路径（太多字分两行）。
    error = function(error) NULL #如果在尝试获取ofile时发生错误，就执行这个函数(哪个函数），返回NULL。
  )

  # 汇总可能的项目目录，并删除重复路径。
  candidate_dirs <- unique(c(
    if (!is.null(rscript_file)) dirname(rscript_file),#dirname()函数返回文件路径中的目录部分.
    if (!is.null(source_file)) dirname(source_file),
    getwd(),
    file.path(getwd(), "DataScience_Shiny")#这个和getwd区别啥？
  ))

  # 删除 NA 和空字符串。
  candidate_dirs <- candidate_dirs[!is.na(candidate_dirs) & nzchar(candidate_dirs)]#nzchar()函数检查字符串是否非空.

  # 将所有候选目录转换为格式统一的绝对路径(说的太呆板了，就是把路径都变成标准的绝对路径）。
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
      normalizePath(getwd(), winslash = "/", mustWork = FALSE),#normalizePath()函数将路径转换为绝对路径，winslash="/"表示在Windows系统中使用正斜杠作为路径分隔符，mustWork=FALSE表示即使路径不存在也不会报错(分成两行把）。
      call. = FALSE#call. = FALSE表示在报错信息中不显示调用栈信息(啥意思，举例一下），这样错误信息会更简洁。
    )
  }

  # 返回第一个有效项目目录。
  app_dirs[[1]]
}

# 找到项目目录，并加载 package 管理函数。
project_dir <- find_project_dir()
source(file.path(project_dir, "R", "packages.R"), encoding = "UTF-8")

# 使用项目自己的 package 目录，并安装真正缺少的 packages。
use_project_library(project_dir)#这个函数,来自哪里（来自上面source那个r文件）会设置R的library路径到项目目录下的一个特定文件夹（比如说project_dir/lib），这样安装的包就会放在这个文件夹里，不会影响全局的R环境。
install_missing_packages(required_packages, project_dir)#install_missing_packages()函数会检查required_packages列表中哪些包在项目的library路径下还没有安装，如果有缺失的包，就会安装它们。

# 在 console 显示本次运行使用的路径和 R 版本。
message("DataScience_Shiny project: ", project_dir)
message("R version: ", R.version.string)
message("Primary project library: ", project_library_path(project_dir))

# 启动网页；浏览器访问 http://127.0.0.1:7411。
shiny::runApp(project_dir, host = "127.0.0.1", port = 7411, launch.browser = FALSE)#shiny::runApp()函数启动Shiny应用，第一个参数是应用所在的目录，host和port参数指定了服务器的地址和端口，launch.browser=FALSE表示不自动打开浏览器（如果设置为TRUE，启动后会自动在默认浏览器中打开应用）（写的有点多了，可以考虑缩减一些，或者放成两行。
