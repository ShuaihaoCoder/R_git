# ============================================================
# UIimprove 图册 package 安装脚本
# ============================================================
# 整体作用：把原始参考脚本绘图所需 package 及其完整依赖链安装到项目自己的 R_library。
# 以后换电脑或 R 版本时，可以先运行本文件，再运行 generate_gallery.R。

project_dir <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(project_dir, "app.R"))) {
  project_dir <- normalizePath(file.path(getwd(), "DataScience_Shiny"), winslash = "/", mustWork = TRUE)
}

source(file.path(project_dir, "R", "packages.R"), encoding = "UTF-8")
use_project_library(project_dir)

# roots 是原始绘图代码直接使用的 package；package_dependencies() 再找出它们依赖的其他 package。
roots <- c(
  "PerformanceAnalytics", "quantmod", "ggfortify", "tidyverse", "reshape2",
  "viridis", "leaps", "olsrr", "multcomp", "HH", "car", "tseries",
  "forecast", "fUnitRoots", "pwr", "GPArotation", "psych", "lmtest", "vars",
  "sjPlot", "broom", "corrplot", "ggcorrplot", "qgraph", "ppcor", "vcd",
  "patchwork", "ggridges", "ggdist", "caret", "corpcor", "igraph", "ggraph",
  "ggExtra", "biotools", "ResourceSelection", "pscl", "pROC", "FinTS",
  "rugarch", "Metrics", "factoextra", "ggforce", "emmeans"
)

available <- utils::available.packages(repos = "https://cloud.r-project.org", type = "binary")
dependencies <- tools::package_dependencies(
  roots,
  db = available,
  which = c("Depends", "Imports", "LinkingTo"),
  recursive = TRUE
)
all_packages <- unique(c(roots, unlist(dependencies, use.names = FALSE)))
all_packages <- intersect(all_packages, rownames(available))

message("Installing gallery dependency closure: ", length(all_packages), " packages")
utils::install.packages(
  all_packages,
  lib = project_library_path(project_dir),
  repos = "https://cloud.r-project.org",
  type = "binary",
  dependencies = FALSE
)

installed_count <- sum(dir.exists(file.path(project_library_path(project_dir), all_packages)))
message("Project package directories: ", installed_count, "/", length(all_packages))
