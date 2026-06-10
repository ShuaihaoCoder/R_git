# ============================================================
# Data Science Optimized Reference
# ============================================================
# 中文说明：
# 这个文件是从 R_Union/DataScience.R 整理出的新项目入口示例。
# 它不再使用 setwd()，所有数据都从本项目 data/ 文件夹读取。
# 原始完整脚本保存在 DataScience_original_reference.R，方便对照。

# 整体功能：
# 这是“脚本式研究入口”，适合在 RStudio / VSCode 中直接运行和调试案例。
# 它与 app.R 共用 R/ 中的函数，但不会创建网页。
#
# commandArgs(trailingOnly = FALSE) 返回启动 R 时的完整参数。
# grep("^--file=", ...) 寻找 Rscript 提供的当前脚本文件路径。
# 如果通过 Rscript 运行，就使用脚本所在目录；否则使用当前工作目录。
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
project_dir <- if (length(script_arg) == 1) {
  # 通过 Rscript 运行时，使用当前脚本所在目录。
  dirname(normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE))
} else {
  # 交互式运行时，使用当前 working directory。
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

# source() 依次加载包管理、数据加载、目录定义和案例函数。
source(file.path(project_dir, "R", "packages.R"))
use_project_library(project_dir)
source(file.path(project_dir, "R", "data_loader.R"))
source(file.path(project_dir, "R", "catalog.R"))
source(file.path(project_dir, "R", "examples.R"))

# 自动检查并加载案例需要的 packages。
load_required_packages(project_dir = project_dir)

data_dir <- file.path(project_dir, "data")

# 中文说明：读取 WIDE_* 数据，并保留原数据库结构。
# data_bundle 是命名 list，例如 data_bundle$rates、data_bundle$fx。
data_bundle <- load_wide_data(data_dir)

# 中文说明：为了兼容原来的分析习惯，这里保留清晰命名的数据对象。
# 这些清晰命名对象便于在交互式研究中直接查看和使用。
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
# 生成回归、相关性、时间序列、PCA 和 Bayes 案例共用的 CAD 数据。
cad_market_data <- prepare_cad_market_data(data_bundle)

# 中文说明：运行一个示例，查看回归结果。
# run_example() 返回标准案例 list；$table 取其中的结果表。
linear_regression_result <- run_example("linear_regression", data_bundle)
print(linear_regression_result$table)

# 中文说明：更多方法目录可通过 get_method_catalog() 查看。
# method_catalog 列出网页中全部一级分类、具体方法和对应 example_id。
method_catalog <- get_method_catalog()
print(method_catalog)
