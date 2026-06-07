# ============================================================
# Data Science Optimized Reference
# ============================================================
# 中文说明：
# 这个文件是从 R_Union/DataScience.R 整理出的新项目入口示例。
# 它不再使用 setwd()，所有数据都从本项目 data/ 文件夹读取。
# 原始完整脚本保存在 DataScience_original_reference.R，方便对照。

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
project_dir <- if (length(script_arg) == 1) {
  dirname(normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(project_dir, "R", "packages.R"))
use_project_library(project_dir)
source(file.path(project_dir, "R", "data_loader.R"))
source(file.path(project_dir, "R", "catalog.R"))
source(file.path(project_dir, "R", "examples.R"))

load_required_packages()

data_dir <- file.path(project_dir, "data")

# 中文说明：读取 WIDE_* 数据，并保留原数据库结构。
data_bundle <- load_wide_data(data_dir)

# 中文说明：为了兼容原来的分析习惯，这里保留清晰命名的数据对象。
rates_data <- data_bundle$rates
fx_data <- data_bundle$fx
vol_data <- data_bundle$vol
eco_data <- data_bundle$eco
cftc_data <- data_bundle$cftc
money_market_data <- data_bundle$money_market
equity_data <- data_bundle$equity
commodity_data <- data_bundle$commodity
credit_data <- data_bundle$credit
allx_data <- data_bundle$allx

# 中文说明：CAD 市场案例数据，后续回归、相关性、时间序列、PCA、Bayes 案例都复用它。
cad_market_data <- prepare_cad_market_data(data_bundle)

# 中文说明：运行一个示例，查看回归结果。
linear_regression_result <- run_example("linear_regression", data_bundle)
print(linear_regression_result$table)

# 中文说明：更多方法目录可通过 get_method_catalog() 查看。
method_catalog <- get_method_catalog()
print(method_catalog)
