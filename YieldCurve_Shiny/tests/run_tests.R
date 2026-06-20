# ============================================================
# Core analytics and applied-result Shiny regression tests
# ============================================================
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
script_path <- if (length(file_arg)) file_arg[[1]] else file.path("YieldCurve_Shiny", "tests", "run_tests.R")
project_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(project_dir, "R", "packages.R"))
install_and_load_packages(project_dir)
source(file.path(project_dir, "R", "curve_engine.R"))
source(file.path(project_dir, "R", "data_loader.R"))

assert_true <- function(condition, message) if (!isTRUE(condition)) stop(message, call. = FALSE)
assert_close <- function(actual, expected, tolerance = 1e-8, message = "Values differ") {
  if (!isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance))) {
    stop(message, ": actual=", actual, ", expected=", expected, call. = FALSE)
  }
}
cat("Running YieldCurve_Shiny tests...\n")

assert_close(market_percent_to_decimal(4.25), 0.0425, message = "Percent conversion failed")
assert_close(decimal_to_percent(0.0425), 4.25, message = "Decimal conversion failed")
assert_close(decimal_to_bp(0.0001), 1, message = "Basis-point conversion failed")

flat_tenor <- c(0.25, 0.5, 1, 2, 5, 10, 20, 30)
flat_curve <- fit_curve(flat_tenor, rep(0.04, length(flat_tenor)), "spline", source = "test", proxy = FALSE)
assert_close(calculate_forward(flat_curve, 1, 5, "annual")$forward_percent, 4, tolerance = 1e-6, message = "Flat forward failed")
receive <- calculate_carry_roll(flat_curve, 0, 5, 0.25, "Receive Fixed")
pay <- calculate_carry_roll(flat_curve, 0, 5, 0.25, "Pay Fixed")
assert_close(receive$total_bp, -pay$total_bp, tolerance = 1e-7, message = "Direction signs failed")
assert_close(receive$roll_bp, 0, tolerance = 1e-6, message = "Flat roll failed")
assert_close(receive$carry_bp, 0, tolerance = 1e-6, message = "Flat carry failed")

market <- load_market_data(project_dir)
assert_true(identical(class(market$wide_rates), "data.frame"), "WIDE_RATES must be data.frame")
raw_table <- readRDS(market$paths$wide_rates)
normalized <- as.data.frame(raw_table, check.names = FALSE)
normalized$date <- as.Date(normalized$date)
latest <- extract_historical_curve(normalized, "USD SOFR OIS", as.Date("2025-10-22"))
assert_true(identical(attr(latest, "effective_date"), as.Date("2025-10-22")), "Latest extraction failed")
fallback <- extract_historical_curve(normalized, "EUR ESTR OIS", as.Date("2025-10-22"))
assert_true(identical(attr(fallback, "effective_date"), as.Date("2025-10-21")), "Nearest date fallback failed")

nearest_fixture <- data.frame(
  date = as.Date(c("2025-01-01", "2025-01-05")),
  "TEST OIS 1 YR" = c(1, 2), "TEST OIS 2 YR" = c(1.1, 2.1), "TEST OIS 3 YR" = c(1.2, 2.2),
  check.names = FALSE
)
assert_true(identical(attr(extract_historical_curve(nearest_fixture, "TEST OIS", as.Date("2025-01-04")), "effective_date"), as.Date("2025-01-05")), "Future nearest failed")
assert_true(identical(attr(extract_historical_curve(nearest_fixture, "TEST OIS", as.Date("2025-01-03")), "effective_date"), as.Date("2025-01-01")), "Tie preference failed")

history_multi <- build_history_comparison(normalized, c("USD SOFR OIS", "AUD COR OIS"), as.Date(c("2025-09-23", "2025-10-22")), as.Date("2025-09-23"))
assert_true(length(unique(history_multi$curve)) == 2 && length(unique(history_multi$requested_date)) == 2, "Multi-history failed")
too_many_dates <- seq(as.Date("2025-09-01"), by = "day", length.out = 31)
history_31 <- try(build_history_comparison(normalized, "USD SOFR OIS", too_many_dates, too_many_dates[[1]], max_combinations = 30), silent = TRUE)
assert_true(inherits(history_31, "try-error"), "History should reject 31 combinations")

steepener <- curve_trade_legs("steepener", 2, 5, 10, 10000)
fly <- curve_trade_legs("long_belly_fly", 2, 5, 10, 10000)
assert_true(identical(as.numeric(steepener$dv01), c(10000, 10000)), "Steepener neutral legs failed")
assert_close(fly$dv01[[2]], fly$dv01[[1]] + fly$dv01[[3]], message = "Fly neutral legs failed")
started_trade <- calculate_curve_trade(flat_curve, steepener, 0.25, "annual", risk_budget = 10000, start = 1)
assert_true(all(started_trade$detail$start_years == 1), "Curve trade start tenor failed")
bad_started_trade <- try(calculate_curve_trade(flat_curve, steepener, 0.25, "annual", risk_budget = 10000, start = 2), silent = TRUE)
assert_true(inherits(bad_started_trade, "try-error"), "Curve trade start validation failed")

old_directory <- getwd()
setwd(project_dir)
source(file.path(project_dir, "app.R"), local = .GlobalEnv)
assert_true(as.Date(initial_history_date) == max(market$wide_rates$date), "History base date UI default should use latest database date")
shiny::testServer(server, {
  session$flushReact()
  session$setInputs(
    source_mode = "zero", curve_name = "USD UNITED STATES OIS", fit_methods = c("nelson_siegel", "spline"),
    forward_source_mode = "zero", forward_curve_name = "EUR EUROZONE (vs. 6M EURIBOR)",
    forward_fit_method = "nelson_siegel", forward_start = 1, forward_end = 5, forward_compounding = "annual",
    carry_source_mode = "zero", carry_curve_name = "AUD AUSTRALIA (vs. 6M Bank Bills)",
    carry_fit_method = "nelson_siegel", carry_start = 0, carry_end = 5, carry_hold = "0.25",
    carry_direction = "Receive Fixed", dv01 = 10000,
    history_curves = c("USD SOFR OIS", "AUD COR OIS"), history_base_date = "2025-09-23",
    history_compare_date = "2025-10-22", history_start_tenor = "0.0833333333333333",
    history_end_tenor = "30", add_history_date = 1,
    trade_structure = "steepener", trade_short_tenor = 2, trade_belly_tenor = 5, trade_long_tenor = 10,
    trade_start = 0, trade_hold = "0.25", trade_risk_budget = 10000, trade_short_dv01 = 10000,
    trade_belly_dv01 = 10000, trade_long_dv01 = 10000
  )
  session$flushReact()
  assert_true(is.null(applied_curve()) && is.null(applied_forward()) && is.null(applied_carry()), "Pages calculated before buttons")

  session$setInputs(apply_curve = 1, run_history = 1, calculate_forward = 1, calculate_carry = 1, calculate_curve_trade = 1)
  session$flushReact()
  assert_true(length(applied_curve()$fits) == 2, "Applied Curve failed")
  assert_true(applied_history()$combinations == 4, "Applied History failed")
  assert_close(applied_history()$tenor_range[[1]], 1/12, tolerance = 1e-8, message = "History tenor range start failed")
  assert_close(applied_history()$tenor_range[[2]], 30, tolerance = 1e-8, message = "History tenor range end failed")
  history_quote_table <- history_quote_details(applied_history()$data, applied_history()$tenor_range)
  assert_true(nrow(history_quote_table) > 0, "History quote details failed")
  assert_true(identical(names(history_quote_table)[1:5], c("Date", "Curve Name", "Tenor", "Rate (%)", "Change (bp)")), "History quote display columns failed")
  assert_true(all(c("date_span", "curve_span", "date_group_start", "curve_group_start") %in% names(history_quote_table)), "History quote span columns failed")
  assert_true(any(history_quote_table$date_span > history_quote_table$curve_span) && any(history_quote_table$curve_span > 1), "History quote span values failed")
  assert_true(all(history_quote_table$Tenor %in% c("2Y", "5Y", "10Y")), "History quote tenors failed")
  assert_true(all(c("USD SOFR OIS", "AUD COR OIS") %in% history_quote_table$`Curve Name`), "History quote selected curves failed")
  largest_move <- history_largest_move_info(applied_history()$data)
  assert_true(grepl("bp$", largest_move$value) && nzchar(largest_move$subtitle), "Largest move label failed")
  assert_true(largest_move$direction %in% c("positive", "negative", "neutral") && nzchar(largest_move$icon), "Largest move direction failed")
  assert_true(applied_forward()$bundle$curve_name == "EUR EUROZONE (vs. 6M EURIBOR)", "Applied Forward failed")
  assert_true(applied_carry()$bundle$curve_name == "AUD AUSTRALIA (vs. 6M Bank Bills)", "Applied Carry failed")
  assert_true(applied_trade()$bundle$curve_name == "AUD AUSTRALIA (vs. 6M Bank Bills)", "Applied Trade should use shared Carry curve")
  assert_true(nrow(applied_trade()$calculation$detail) == 2, "Applied Trade failed")
  assert_true(all(applied_trade()$calculation$detail$start_years == 0), "Default Trade start failed")
  session$setInputs(carry_workspace_mode = "trade")
  session$flushReact()
  assert_true(!is.null(output$carry_value_ui) && !grepl("NA", paste(output$carry_value_ui, collapse = "")), "Curve Trade KPI Carry failed")
  assert_true(grepl("DV01 Neutral", output$trade_mode_value), "Curve Trade KPI mode failed")
  session$setInputs(carry_workspace_mode = "single")
  session$flushReact()
  assert_true(grepl("is-disabled", paste(output$curve_trade_workspace, collapse = "")), "Single mode Curve Trade disabled panel failed")
  assert_true(all(vapply(c("curve", "history", "forward", "carry", "trade"), function(page) status[[page]]$type == "success", logical(1))), "Progress did not complete")
  assert_true(identical(levels(applied_carry()$matrix$hold_label), c("1M", "3M", "6M", "1Y")), "Hold order failed")
  assert_true(identical(levels(applied_carry()$matrix$tenor_label), c("1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "15Y", "20Y", "30Y")), "Tenor order failed")

  old_forward <- applied_forward()$result$forward_percent
  old_carry <- applied_carry()$single$total_bp
  session$setInputs(
    forward_source_mode = "historical", forward_curve_name = "EUR ESTR OIS", forward_curve_date = "2025-10-22",
    carry_source_mode = "historical", carry_curve_name = "AUD COR OIS", carry_curve_date = "2025-10-22",
    carry_direction = "Pay Fixed", dv01 = 25000,
    history_end_tenor = "5"
  )
  session$flushReact()
  assert_close(applied_forward()$result$forward_percent, old_forward, message = "Forward changed before Calculate")
  assert_close(applied_carry()$single$total_bp, old_carry, message = "Carry changed before Calculate")
  assert_true(status$forward$type == "pending" && status$carry$type == "pending", "Pending status failed")
  assert_close(applied_history()$tenor_range[[1]], 1/12, tolerance = 1e-8, message = "History start changed before Run")
  assert_close(applied_history()$tenor_range[[2]], 30, tolerance = 1e-8, message = "History end changed before Run")
  assert_true(status$history$type == "pending", "History tenor range pending failed")

  session$setInputs(calculate_forward = 2, calculate_carry = 2)
  session$flushReact()
  assert_true(isTRUE(applied_forward()$bundle$proxy), "Historical Forward Proxy failed")
  assert_true(applied_forward()$result$effective_date[[1]] == "2025-10-21", "Forward effective date failed")
  assert_true(isTRUE(applied_carry()$bundle$proxy), "Historical Carry Proxy failed")
  successful_forward <- applied_forward()$result$forward_percent
  session$setInputs(forward_start = 5, forward_end = 1, calculate_forward = 3)
  session$flushReact()
  assert_close(applied_forward()$result$forward_percent, successful_forward, message = "Failed calculation should retain old Forward")
  assert_true(status$forward$type == "error", "Failed calculation should show error status")
  session$setInputs(forward_start = 1, forward_end = 5, calculate_forward = 4)
  session$flushReact()

  session$setInputs(run_history = 2)
  session$flushReact()
  assert_close(applied_history()$tenor_range[[1]], 1/12, tolerance = 1e-8, message = "History range start did not apply")
  assert_close(applied_history()$tenor_range[[2]], 5, tolerance = 1e-8, message = "History range end did not apply")
  ranged_history_quote_table <- history_quote_details(applied_history()$data, applied_history()$tenor_range)
  assert_true(all(ranged_history_quote_table$Tenor %in% c("2Y", "5Y")), "History quote tenor range filter failed")

  session$setInputs(trade_structure = "long_belly_fly", trade_short_dv01 = 5000, trade_belly_dv01 = 10000, trade_long_dv01 = 5000)
  session$flushReact()
  assert_true(nrow(applied_trade()$calculation$detail) == 2, "Trade changed before Calculate")
  session$setInputs(calculate_curve_trade = 2)
  session$flushReact()
  assert_true(nrow(applied_trade()$calculation$detail) == 3, "Fly calculation failed")
  session$setInputs(trade_start = 1, calculate_curve_trade = 3)
  session$flushReact()
  assert_true(all(applied_trade()$calculation$detail$start_years == 1), "Trade start input failed")
  good_trade_total <- applied_trade()$calculation$summary$total_pnl
  session$setInputs(trade_start = 10, calculate_curve_trade = 4)
  session$flushReact()
  assert_close(applied_trade()$calculation$summary$total_pnl, good_trade_total, message = "Failed Trade should retain old result")
  assert_true(status$trade$type == "error", "Failed Trade should show error status")
  session$setInputs(trade_start = 0, calculate_curve_trade = 5)
  session$flushReact()

  rendered_outputs <- list(
    output$curve_plot, output$fit_summary, output$history_absolute_plot, output$history_change_plot,
    output$history_comparison_table, output$history_status_detail, output$history_header_subtitle,
    output$forward_result, output$forward_curve_plot,
    output$carry_value_ui, output$roll_value_ui, output$total_bp_ui, output$pnl_value_ui, output$curve_trade_workspace,
    output$carry_component_plot, output$carry_spot_plot, output$carry_stacked_plot, output$carry_heatmap,
    output$trade_leg_table, output$trade_leg_pnl_plot, output$trade_component_plot, output$diagnostics_table
  )
  assert_true(all(vapply(rendered_outputs, function(value) !is.null(value), logical(1))), "Output rendering failed")
  previous_carry_total <- applied_carry()$single$total_bp
  session$setInputs(open_carry_stacked_plot = 1)
  session$flushReact()
  assert_true(identical(large_plot_id(), "carry_stacked_plot"), "Large plot modal id failed")
  assert_true(identical(applied_carry()$single$total_bp, previous_carry_total), "Large plot changed applied result")
  assert_true(!is.null(output$large_plot), "Large plot rendering failed")
  session$setInputs(refresh_data = 1)
  session$flushReact()
  assert_true(!is.null(applied_curve()), "Refresh should retain results")
})
setwd(old_directory)

cat("All tests passed.\n")
