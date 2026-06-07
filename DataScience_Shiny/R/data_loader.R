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

load_wide_data <- function(data_dir) {
  # 中文说明：集中读取原来的 WIDE_* 数据，保持数据库结构不变。
  missing_files <- wide_data_files[
    !file.exists(file.path(data_dir, unname(wide_data_files)))
  ]

  if (length(missing_files) > 0) {
    stop(
      "Missing data files in ", data_dir, ": ",
      paste(unname(missing_files), collapse = ", "),
      call. = FALSE
    )
  }

  data_bundle <- lapply(wide_data_files, function(file_name) {
    data.table::as.data.table(readRDS(file.path(data_dir, file_name)))
  })

  # 中文说明：统一把 date 转为 Date，后续 join / time-series 处理会更稳定。
  data_bundle <- lapply(data_bundle, function(data_item) {
    if ("date" %in% names(data_item)) {
      data_item[, date := as.Date(date)]
    }
    data_item
  })

  data_bundle
}

safe_grep_columns <- function(data, pattern, max_n = Inf) {
  matches <- grep(pattern, names(data), value = TRUE, perl = TRUE)
  if (length(matches) == 0) {
    character()
  } else {
    head(matches, max_n)
  }
}

prepare_cad_market_data <- function(data_bundle) {
  # 中文说明：这一步复用原脚本的 CAD / USDCAD / 利率 / 股指逻辑，
  # 但把列选择集中到一个函数里，避免每个案例重复写 grep/select/join。
  fx_data <- data_bundle$fx
  rates_data <- data_bundle$rates
  equity_data <- data_bundle$equity
  money_market_data <- data_bundle$money_market

  fx_col <- safe_grep_columns(fx_data, "(?=.*CAD)(?=.*USD)", 1)
  rate_cols <- safe_grep_columns(rates_data, "CANADA", 3)
  equity_cols <- safe_grep_columns(equity_data, "Canada", 3)

  if (length(fx_col) < 1 || length(rate_cols) < 3 || length(equity_cols) < 3) {
    stop("Cannot find the CAD market columns needed for examples.", call. = FALSE)
  }

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

  selected_positions <- c(1, 2, 5, 6, 7)
  selected_positions <- selected_positions[selected_positions <= ncol(cad_data)]
  cad_data <- cad_data[, selected_positions, with = FALSE]
  setnames(
    cad_data,
    old = names(cad_data),
    new = c("date", "USDCAD", "X10Y", "TSX60", "TSXC")[seq_along(names(cad_data))]
  )

  if ("CAD ON" %in% names(money_market_data)) {
    cad_data <- merge(
      cad_data,
      money_market_data[, .(date, CAD_ON = `CAD ON`)],
      by = "date",
      all.x = TRUE
    )
    cad_data[, CAD_ON := zoo::na.locf(CAD_ON, na.rm = FALSE)]
  } else {
    cad_data[, CAD_ON := NA_real_]
  }

  cad_data <- na.omit(cad_data)
  cad_data[, USDCAD_ret := log(USDCAD / data.table::shift(USDCAD))]
  cad_data[, TSXC_ret := log(TSXC / data.table::shift(TSXC))]
  cad_data[, delta10y := X10Y - data.table::shift(X10Y)]
  na.omit(cad_data)
}

describe_variables <- function(example_id = NULL) {
  # 中文说明：网页上每个案例都会展示变量含义，方便以后复盘。
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
