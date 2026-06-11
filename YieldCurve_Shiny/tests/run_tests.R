# ============================================================
# 核心计算、真实数据与 Shiny reactive 自动测试
# ============================================================
# 这个文件故意先加载真实网页 packages，再读取数据。
# 这样可以复现 data.table 等 package 已加载时的运行环境，避免测试环境误通过。

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
script_path <- if (length(file_arg)) file_arg[[1]] else file.path("YieldCurve_Shiny", "tests", "run_tests.R")
project_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(project_dir, "R", "packages.R"))
install_and_load_packages(project_dir)
source(file.path(project_dir, "R", "curve_engine.R"))
source(file.path(project_dir, "R", "data_loader.R"))

assert_true <- function(condition, message) {
  # 条件不是严格 TRUE 时立即停止，并显示该测试代表的业务含义。
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

assert_close <- function(actual, expected, tolerance = 1e-8, message = "Values differ") {
  # 数值计算允许一个很小的误差范围，适合拟合和浮点数结果。
  if (!isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance))) {
    stop(message, ": actual=", actual, ", expected=", expected, call. = FALSE)
  }
}

cat("Running YieldCurve_Shiny tests...\n")

# 第一组验证单位换算，专门防止原 YieldCurve.R 把 4.25 当成 425% 使用。
assert_close(market_percent_to_decimal(4.25), 0.0425, message = "Percent conversion failed")
assert_close(decimal_to_percent(0.0425), 4.25, message = "Decimal conversion failed")
assert_close(decimal_to_bp(0.0001), 1, message = "Basis-point conversion failed")

flat_tenor <- c(0.25, 0.5, 1, 2, 5, 10, 20, 30)
flat_rate <- rep(0.04, length(flat_tenor))
flat_curve <- fit_curve(flat_tenor, flat_rate, "spline", source = "test zero curve", proxy = FALSE)
flat_forward <- calculate_forward(flat_curve, 1, 5, "annual")
assert_close(flat_forward$forward_percent, 4, tolerance = 1e-6, message = "Flat-curve forward is incorrect")

receive <- calculate_carry_roll(flat_curve, 0, 5, 0.25, "Receive Fixed")
pay <- calculate_carry_roll(flat_curve, 0, 5, 0.25, "Pay Fixed")
assert_close(receive$total_bp, -pay$total_bp, tolerance = 1e-7, message = "Direction signs are not opposite")
assert_close(receive$roll_bp, 0, tolerance = 1e-6, message = "Flat-curve roll should be zero")
assert_close(receive$carry_bp, 0, tolerance = 1e-6, message = "Flat-curve net carry should be zero")
assert_close(calculate_dv01_pnl(2.5, 10000), 25000, message = "DV01 P&L failed")

ns_curve <- fit_curve(c(0.25, 0.5, 1, 2, 5, 10, 20, 30),
  c(0.02, 0.021, 0.022, 0.024, 0.027, 0.029, 0.03, 0.031),
  "nelson_siegel", source = "test", proxy = FALSE)
assert_true(is.finite(ns_curve$rmse_bp), "Nelson-Siegel RMSE is invalid")
assert_true(nrow(ns_curve$diagnostics) == 8, "Nelson-Siegel diagnostics are incomplete")

bad_fit <- try(fit_curve(c(1, 2), c(0.02, 0.03), "nelson_siegel"), silent = TRUE)
assert_true(inherits(bad_fit, "try-error"), "Insufficient curve points should fail")

market <- load_market_data(project_dir)
assert_true(nrow(market$wide_rates) > 0, "WIDE_RATES did not load")
assert_true(nrow(market$zero_curve) > 0, "ZERORATE_CURVE did not load")
assert_true(identical(class(market$wide_rates), "data.frame"), "WIDE_RATES must be normalized to data.frame")
zero_names <- zero_curve_names(market$zero_curve)
historical_names <- historical_curve_names(market$wide_rates)
assert_true(length(zero_names) > 0, "No zero curves found")
assert_true(length(historical_names) > 0, "No historical curves found")
zero_points <- extract_zero_curve(market$zero_curve, zero_names[[1]])
assert_true(nrow(zero_points) >= 4, "Zero curve extraction failed")

# 真实 RDS 原来带有 data.table class；本测试重新加回该 class，
# 确保动态列选择不再因为 data.table 的 j 规则报 matched_columns 错误。
raw_table <- readRDS(market$paths$wide_rates)
assert_true(inherits(raw_table, "data.table"), "Source WIDE_RATES should reproduce the historical data.table condition")
normalized_table <- as.data.frame(raw_table, check.names = FALSE)
normalized_table$date <- as.Date(normalized_table$date)
historical_points <- extract_historical_curve(normalized_table, "USD SOFR OIS", as.Date("2025-10-22"))
assert_true(nrow(historical_points) >= 20, "USD SOFR OIS extraction failed")
assert_true(identical(attr(historical_points, "effective_date"), as.Date("2025-10-22")),
  "USD SOFR OIS should use the requested latest date")

# EUR ESTR OIS 在最新日期没有足够报价，应自动回退而不是让页面报错。
fallback_points <- extract_historical_curve(normalized_table, "EUR ESTR OIS", as.Date("2025-10-22"))
assert_true(attr(fallback_points, "effective_date") < as.Date("2025-10-22"),
  "Missing historical date should fall back to an earlier effective date")

data_table_points <- extract_historical_curve(raw_table, "AUD COR OIS", as.Date("2025-10-22"))
data_frame_points <- extract_historical_curve(normalized_table, "AUD COR OIS", as.Date("2025-10-22"))
assert_true(isTRUE(all.equal(data_table_points, data_frame_points, check.attributes = FALSE)),
  "Historical extraction must be stable across normalized inputs")

# 多曲线、多日期 History 应返回所有组合，并让每条曲线分别与自己的 base date 比较。
history_multi <- build_history_comparison(normalized_table,
  c("USD SOFR OIS", "AUD COR OIS"),
  as.Date(c("2025-09-23", "2025-10-22")),
  as.Date("2025-09-23"))
assert_true(length(unique(history_multi$curve)) == 2, "Multi-curve History failed")
assert_true(length(unique(history_multi$requested_date)) == 2, "Multi-date History failed")
assert_close(max(abs(history_multi$change_bp[history_multi$requested_date == as.Date("2025-09-23")]), na.rm = TRUE),
  0, tolerance = 1e-8, message = "History base-date changes should be zero")

# Curve Trade 的默认 legs 必须满足已约定的 DV01-neutral 结构。
steepener_legs <- curve_trade_legs("steepener", 2, 5, 10, 10000)
assert_true(identical(as.numeric(steepener_legs$dv01), c(10000, 10000)), "Steepener DV01-neutral legs failed")
fly_legs <- curve_trade_legs("long_belly_fly", 2, 5, 10, 10000)
assert_close(fly_legs$dv01[[2]], fly_legs$dv01[[1]] + fly_legs$dv01[[3]],
  message = "Fly belly DV01 must equal wing DV01 total")
trade_calc <- calculate_curve_trade(ns_curve, fly_legs, 0.25, "annual", 10000)
assert_close(trade_calc$summary$total_pnl,
  sum(trade_calc$detail$total_pnl), message = "Curve Trade portfolio P&L failed")

# source(app.R) 后 server 函数可供 testServer() 使用。
# 这里直接设置浏览器输入，检查五个页面共同依赖的 reactive 结果。
old_directory <- getwd()
setwd(project_dir)
source(file.path(project_dir, "app.R"), local = .GlobalEnv)
shiny::testServer(server, {
  # session$setInputs() 相当于用户在浏览器里选择曲线并填写计算器。
  # flushReact() 等待这些输入触发的 reactive 全部完成后再检查结果。
  session$setInputs(
    source_mode = "zero",
    curve_name = "USD UNITED STATES OIS",
    fit_methods = c("nelson_siegel", "spline"),
    forward_source_mode = "zero",
    forward_curve_name = "EUR EUROZONE (vs. 6M EURIBOR)",
    forward_fit_method = "nelson_siegel",
    forward_start = 1,
    forward_end = 5,
    forward_compounding = "annual",
    carry_source_mode = "zero",
    carry_curve_name = "AUD AUSTRALIA (vs. 6M Bank Bills)",
    carry_fit_method = "nelson_siegel",
    carry_start = 0,
    carry_end = 5,
    carry_hold = "0.25",
    carry_direction = "Receive Fixed",
    dv01 = 10000,
    history_curves = c("USD SOFR OIS", "AUD COR OIS"),
    history_dates = c("2025-09-23", "2025-10-22"),
    history_base_date = "2025-09-23",
    trade_source_mode = "zero",
    trade_curve_name = "USD UNITED STATES OIS",
    trade_fit_method = "nelson_siegel",
    trade_structure = "steepener",
    trade_short_tenor = 2,
    trade_belly_tenor = 5,
    trade_long_tenor = 10,
    trade_hold = "0.25",
    trade_risk_budget = 10000,
    trade_short_dv01 = 10000,
    trade_belly_dv01 = 10000,
    trade_long_dv01 = 10000,
    calculate_curve_trade = 1
  )
  session$flushReact()
  assert_true(nrow(current_points()) >= 20, "Curve Explorer reactive failed")
  assert_true(length(current_fits()) == 2, "Curve fit reactive failed")
  assert_true(forward_curve()$curve_name == "EUR EUROZONE (vs. 6M EURIBOR)", "Forward independent curve failed")
  assert_true(carry_curve()$curve_name == "AUD AUSTRALIA (vs. 6M Bank Bills)", "Carry independent curve failed")
  assert_true(is.finite(forward_result()$forward_percent), "Forward page reactive failed")
  assert_true(is.finite(carry_result()$total_bp), "Carry page reactive failed")
  assert_true(nrow(carry_matrix_data()) > 0, "Carry matrix reactive failed")
  assert_true(nrow(history_data()) > 0, "History page reactive failed")
  assert_true(nrow(trade_result()$detail) == 2, "Steepener Curve Trade reactive failed")

  # 读取 output$... 会强制执行对应 render 函数，相当于逐页确认图、表和卡片能够生成。
  rendered_outputs <- list(
    curve_plot = output$curve_plot,
    fit_summary = output$fit_summary,
    ns_parameters = output$ns_parameters,
    history_absolute_plot = output$history_absolute_plot,
    history_change_plot = output$history_change_plot,
    history_comparison_table = output$history_comparison_table,
    forward_value = output$forward_value,
    forward_result = output$forward_result,
    forward_curve_plot = output$forward_curve_plot,
    carry_value = output$carry_value,
    roll_value = output$roll_value,
    total_value = output$total_value,
    carry_component_plot = output$carry_component_plot,
    carry_spot_plot = output$carry_spot_plot,
    carry_stacked_plot = output$carry_stacked_plot,
    carry_matrix = output$carry_matrix,
    carry_heatmap = output$carry_heatmap,
    trade_leg_table = output$trade_leg_table,
    trade_leg_pnl_plot = output$trade_leg_pnl_plot,
    trade_component_plot = output$trade_component_plot,
    diagnostics_table = output$diagnostics_table,
    input_points = output$input_points
  )
  assert_true(all(vapply(rendered_outputs, function(value) !is.null(value), logical(1))),
    "One or more page outputs failed to render")

  # Forward 与 Carry 分别切换到不同历史 Proxy，验证独立选择与日期回退。
  session$setInputs(
    forward_source_mode = "historical",
    forward_curve_name = "EUR ESTR OIS",
    forward_curve_date = "2025-10-22",
    carry_source_mode = "historical",
    carry_curve_name = "AUD COR OIS",
    carry_curve_date = "2025-10-22",
    carry_direction = "Pay Fixed",
    dv01 = 25000,
    trade_structure = "long_belly_fly",
    trade_short_dv01 = 5000,
    trade_belly_dv01 = 10000,
    trade_long_dv01 = 5000,
    calculate_curve_trade = 2
  )
  session$flushReact()
  assert_true(isTRUE(forward_curve()$proxy), "Historical Forward must be marked Proxy")
  assert_true(isTRUE(carry_curve()$proxy), "Historical Carry must be marked Proxy")
  assert_true(forward_curve()$effective_date < as.Date("2025-10-22"), "Forward date fallback failed")
  assert_true(is.finite(forward_result()$forward_percent), "Historical Forward page failed")
  assert_true(is.finite(carry_result()$total_bp), "Historical Carry page failed")
  assert_true(nrow(trade_result()$detail) == 3, "Fly Curve Trade reactive failed")

  # Refresh 按钮重新读取 RDS 后，当前页面仍应可以生成结果。
  session$setInputs(refresh_data = 1)
  session$flushReact()
  assert_true(nrow(current_points()) >= 3, "Page failed after Refresh local RDS")
})
setwd(old_directory)

cat("All tests passed.\n")
