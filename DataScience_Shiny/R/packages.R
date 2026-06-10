# ============================================================
# Package management
# ============================================================
# 整体功能：
# 这个文件统一管理网页需要的 R packages。网页启动时会先调用这里的函数：
# 1. 确定当前 R 版本对应的项目包目录；
# 2. 将项目包目录加入 .libPaths()；
# 3. 安装真正缺少的 package；
# 4. 验证 package 能被当前 R session 加载。

# required_packages 是网页运行所需 package 的唯一清单。
# 增加新的 package 时，应先在这里添加名称，避免在多个文件中重复维护。
required_packages <- c(
  "shiny",
  "bslib",
  "DT",
  "data.table",
  "ggplot2",
  "plotly",
  "visNetwork",
  "zoo"
)

# project_library_path()
# 功能：返回当前 R 版本专用的项目 package 目录。
# 参数：
# - project_dir：DataScience_Shiny 根目录；默认使用当前工作目录。
# 返回：
# - 字符串路径，例如 ".../DataScience_Shiny/R_library/R-4.5"。
# 为什么按版本分目录：
# - 不同 R 版本安装的二进制 package 可能不兼容，分开保存更稳定。
project_library_path <- function(project_dir = normalizePath(".", winslash = "/", mustWork = TRUE)) {
  # 使用当前 R 的主版本和次版本生成独立目录名，例如 R-4.5。
  r_version_dir <- paste0(
    "R-",
    R.version$major,
    ".",
    strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
  )

  # 返回项目根目录下当前 R 版本专用的 package 路径。
  file.path(
    project_dir,
    "R_library",
    r_version_dir
  )
}

# use_project_library()
# 功能：创建并启用项目自己的 package library。
# 参数：
# - project_dir：DataScience_Shiny 根目录。
# 返回：
# - invisibly 返回更新后的 .libPaths()；通常不直接打印。
# 关键函数：
# - dir.create(..., recursive = TRUE)：父目录不存在时一起创建。
# - .libPaths(...)：设置 R 查找 package 的目录顺序，越靠前优先级越高。
# - normalizePath(..., winslash = "/")：把 Windows 路径统一为正斜杠形式。
use_project_library <- function(project_dir = normalizePath(".", winslash = "/", mustWork = TRUE)) {
  # 取得当前 R 版本专用目录，并保留旧版公共目录作为兼容路径。
  version_library <- project_library_path(project_dir)
  legacy_library <- file.path(project_dir, "R_library")

  # 如果版本专用目录不存在，则连同父目录一起创建。
  dir.create(version_library, recursive = TRUE, showWarnings = FALSE)

  # 将项目 package 目录放到搜索路径最前面，优先使用项目内 packages。
  .libPaths(unique(c(
    normalizePath(version_library, winslash = "/", mustWork = TRUE),
    normalizePath(legacy_library, winslash = "/", mustWork = TRUE),
    .libPaths()
  )))

  # 返回新的 package 搜索路径，但不自动打印到 console。
  invisible(.libPaths())
}

# install_missing_packages()
# 功能：自动安装目录中不存在的 package，并确认所有 package 都可以加载。
# 参数：
# - packages：要检查的 package 名称向量，默认使用 required_packages。
# - project_dir：项目根目录，用于确定 package 安装位置。
# 返回：
# - 成功时 invisibly 返回 TRUE；失败时 stop() 并显示具体 package 错误。
# 注意：
# - 这里只在 package 文件夹不存在时安装，避免 VSCode 长期 session 反复下载。
# - dependencies 指定安装 package 正常运行所依赖的 Depends / Imports / LinkingTo。
install_missing_packages <- function(packages = required_packages, project_dir = normalizePath(".", winslash = "/", mustWork = TRUE)) {
  # 先启用项目 package 目录，确保后续检查和安装都指向正确位置。
  use_project_library(project_dir)

  # 根据 package 文件夹是否存在，找出真正尚未安装的 packages。
  target_library <- project_library_path(project_dir)
  package_directories <- file.path(target_library, packages)
  missing_packages <- packages[!dir.exists(package_directories)]

  # 只有确实缺少 package 文件夹时才联网安装，避免每次启动重复下载。
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    message("Target library: ", target_library)
    options(timeout = max(600, getOption("timeout")))
    install.packages(
      missing_packages,
      lib = target_library,
      repos = "https://cloud.r-project.org",
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }

  # 尝试加载每个 package 的 namespace，并保存具体错误文本。
  # vapply() 对每个 package 执行相同检查，并保证结果一定是 character。
  # loadNamespace() 只加载命名空间，不把函数直接放到当前搜索路径。
  load_errors <- vapply(
    packages,
    function(package) {
      tryCatch(
        {
          loadNamespace(package)
          ""
        },
        error = function(error) conditionMessage(error)
      )
    },
    character(1)
  )
  load_errors <- load_errors[nzchar(load_errors)]

  # 如果存在加载错误，整理 package 位置和错误原因，方便直接排查。
  if (length(load_errors) > 0) {
    package_details <- vapply(
      names(load_errors),
      function(package) {
        package_location <- find.package(package, quiet = TRUE)
        if (!nzchar(package_location)) {
          package_location <- "<not found in .libPaths()>"
        }
        paste0(
          "- ", package, "\n",
          "  Location: ", package_location, "\n",
          "  Error: ", load_errors[[package]]
        )
      },
      character(1)
    )

    stop(
      "Required packages cannot load:\n",
      paste(package_details, collapse = "\n"), "\n\n",
      "Project library: ", target_library, "\n",
      ".libPaths(): ", paste(.libPaths(), collapse = " | "),
      call. = FALSE
    )
  }

  # 所有 package 都可以加载时，安静地返回 TRUE。
  invisible(TRUE)
}

# load_required_packages()
# 功能：先确保 package 已安装，再使用 library() 将它们加载到当前 session。
# 参数：
# - packages：要加载的 package 名称。
# - project_dir：项目根目录。
# 返回：
# - invisibly 返回 library() 调用结果列表。
load_required_packages <- function(packages = required_packages, project_dir = normalizePath(".", winslash = "/", mustWork = TRUE)) {
  # 先确保所有 packages 已安装且可以加载。
  install_missing_packages(packages, project_dir)

  # 将每个 package 正式附加到当前 R session。
  invisible(lapply(packages, library, character.only = TRUE))
}
