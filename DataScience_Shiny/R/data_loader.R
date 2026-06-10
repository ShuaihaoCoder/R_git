# ============================================================
# Data loading and shared data preparation
# ============================================================
# 整体功能：
# 这个文件把原来散落在 DataScience.R 中的数据读取和公共数据准备步骤集中起来。
# 网页案例不直接 readRDS()，而是统一从 load_wide_data() 返回的 data_bundle 取数据。

# 左侧名称是程序内部使用的短名称，右侧是 data/ 中的真实文件名。
wide_data_files <- c(
  rates = "WIDE_RATES",
  fx = "WIDE_FX",
  vol = "WIDE_VOL",
  eco = "WIDE_ECO",
  cftc = "WIDE_CFTC",
  money_market = "WIDE_MM",
  equity = "WIDE_EQ",
  commodity = "WIDE_COMM",
  credit = "WIDE_CREDIT",
  allx = "WIDE_ALLX"
)

# load_wide_data()
# 功能：一次读取全部 WIDE_* 文件，返回一个有名字的 list。
# 参数：
# - data_dir：存放 WIDE_* 文件的文件夹路径。
# 返回：
# - data_bundle：命名 list，例如 data_bundle$fx、data_bundle$rates。
# 关键函数：
# - unname(wide_data_files)：只取真实文件名，用于检查文件是否存在。
# - lapply()：对每个文件执行同一个 readRDS() 读取步骤。
# - data.table::as.data.table()：将读取结果转成 data.table，便于后续处理。
load_wide_data <- function(data_dir) {
  # 拼接全部预期数据文件路径，并找出不存在的文件。
  missing_files <- wide_data_files[
    !file.exists(file.path(data_dir, unname(wide_data_files)))
  ]

  # 缺少任何核心数据文件时立即停止，避免后续案例产生难理解的错误。
  if (length(missing_files) > 0) {
    stop(
      "Missing data files in ", data_dir, ": ",
      paste(unname(missing_files), collapse = ", "),
      call. = FALSE
    )
  }

  # 逐个读取 RDS 文件，并转成 data.table；list 名称来自 wide_data_files。
  data_bundle <- lapply(wide_data_files, function(file_name) {
    data.table::as.data.table(readRDS(file.path(data_dir, file_name)))
  })

  # 统一把 date 转为 Date，后续 merge 和时间序列处理会更稳定。
  data_bundle <- lapply(data_bundle, function(data_item) {
    if ("date" %in% names(data_item)) {
      data_item[, date := as.Date(date)]
    }
    data_item
  })

  data_bundle
}

# safe_grep_columns()
# 功能：根据列名规则寻找数据列，但找不到时返回空向量而不是报错。
# 参数：
# - data：要搜索列名的数据框或 data.table。
# - pattern：正则表达式，例如 "(?=.*CAD)(?=.*USD)" 表示同时包含 CAD 和 USD。
# - max_n：最多返回几个匹配列；Inf 表示不限数量。
# 返回：
# - 匹配到的列名 character 向量。
safe_grep_columns <- function(data, pattern, max_n = Inf) {
  # 在数据列名中执行正则搜索，并直接返回匹配到的列名。
  matches <- grep(pattern, names(data), value = TRUE, perl = TRUE)

  # 找不到时返回空字符向量；找到时最多保留前 max_n 个结果。
  if (length(matches) == 0) {
    character()
  } else {
    head(matches, max_n)
  }
}

# prepare_cad_market_data()
# 功能：准备多个网页案例共用的 CAD 市场分析表。
# 参数：
# - data_bundle：load_wide_data() 返回的命名 list。
# 返回：
# - 一个 data.table，包含 date、USDCAD、X10Y、TSX60、TSXC、CAD_ON，
#   以及计算后的 USDCAD_ret、TSXC_ret、delta10y。
# 处理逻辑：
# 1. 分别从 FX、Rates、Equity、Money Market 数据找需要的列；
# 2. 使用 date 作为键合并；
# 3. 统一列名；
# 4. 计算对数收益率和 10 年期收益率变化。
prepare_cad_market_data <- function(data_bundle) {
  # 从完整数据包中取出当前案例真正需要的四类数据库。
  fx_data <- data_bundle$fx
  rates_data <- data_bundle$rates
  equity_data <- data_bundle$equity
  money_market_data <- data_bundle$money_market

  # 按列名寻找 USDCAD、加拿大利率和加拿大股票指数列。
  fx_col <- safe_grep_columns(fx_data, "(?=.*CAD)(?=.*USD)", 1)
  rate_cols <- safe_grep_columns(rates_data, "CANADA", 3)
  equity_cols <- safe_grep_columns(equity_data, "Canada", 3)

  # 找不到足够的案例列时停止，避免位置选择引用错误列。
  if (length(fx_col) < 1 || length(rate_cols) < 3 || length(equity_cols) < 3) {
    stop("Cannot find the CAD market columns needed for examples.", call. = FALSE)
  }

  # merge(..., by = "date")：只保留双方都有的日期，相当于 inner join。
  cad_data <- merge(
    fx_data[, c("date", fx_col), with = FALSE],
    rates_data[, c("date", rate_cols), with = FALSE],
    by = "date"
  )
  cad_data <- merge(
    cad_data,
    equity_data[, c("date", equity_cols), with = FALSE],
    by = "date"
  )

  # 从合并表中保留原案例使用的列位置，并统一为容易理解的名称。
  selected_positions <- c(1, 2, 5, 6, 7)
  selected_positions <- selected_positions[selected_positions <= ncol(cad_data)]
  cad_data <- cad_data[, selected_positions, with = FALSE]
  setnames(
    cad_data,
    old = names(cad_data),
    new = c("date", "USDCAD", "X10Y", "TSX60", "TSXC")[seq_along(names(cad_data))]
  )

  # 如果存在 CAD 隔夜利率，则按日期左连接并向前填补缺失值。
  if ("CAD ON" %in% names(money_market_data)) {
    cad_data <- merge(
      cad_data,
      money_market_data[, .(date, CAD_ON = `CAD ON`)],
      by = "date",
      all.x = TRUE # 保留 cad_data 中全部日期，相当于 left join。
    )
    # na.locf() 使用上一个可用值填补缺失值；na.rm = FALSE 保留开头无法填补的 NA。
    cad_data[, CAD_ON := zoo::na.locf(CAD_ON, na.rm = FALSE)]
  } else {
    cad_data[, CAD_ON := NA_real_]
  }

  # 删除模型核心变量的缺失行，再计算收益率和利率变化。
  cad_data <- na.omit(cad_data)
  # shift() 取上一期数值；log(今天 / 昨天) 是常用的对数收益率。
  cad_data[, USDCAD_ret := log(USDCAD / data.table::shift(USDCAD))]
  cad_data[, TSXC_ret := log(TSXC / data.table::shift(TSXC))]
  cad_data[, delta10y := X10Y - data.table::shift(X10Y)]
  # shift() 会使第一行产生 NA，因此最后再次删除缺失行并返回结果。
  na.omit(cad_data)
}

# describe_variables()
# 功能：给 Shiny 的 Variable Meanings 表提供变量解释。
# 参数：
# - example_id：预留参数，未来可根据案例只返回相关变量；当前返回公共变量表。
# 返回：
# - 两列 data.frame：variable 和 meaning。
describe_variables <- function(example_id = NULL) {
  # 当前先返回所有案例共用变量；example_id 保留给未来按案例筛选。
  data.frame(
    variable = c("USDCAD", "USDCAD_ret", "TSXC", "TSXC_ret", "X10Y", "delta10y", "CAD_ON", "Risk", "ML1-ML3"),
    meaning = c(
      "USD/CAD exchange rate. Higher values mean USD is stronger against CAD.",
      "Log return of USDCAD, used as the FX movement target in several examples.",
      "Canadian equity index proxy selected from WIDE_EQ.",
      "Log return of TSXC, used as a risk sentiment / equity market input.",
      "Canada 10-year yield proxy selected from WIDE_RATES.",
      "Daily change in the 10-year yield; often used as a rate shock variable.",
      "CAD overnight money-market rate from WIDE_MM.",
      "Bloomberg country risk / macro risk score where available in WIDE_ALLX.",
      "Latent factor scores from exploratory factor analysis."
    ),
    stringsAsFactors = FALSE
  )
}
