# ============================================================
# 真实市场数据验证矩阵
# ============================================================
# 这个脚本不启动 Shiny 页面，只验证页面背后的共享计算引擎。
# 它会打印具体曲线、日期和交易结构的结果，便于交易员快速人工复核。
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
script_path <- if (length(file_arg)) file_arg[[1]] else file.path("YieldCurve_Shiny", "tests", "validation_matrix.R")
project_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

source(file.path(project_dir, "R", "curve_engine.R"))
source(file.path(project_dir, "R", "data_loader.R"))
market <- load_market_data(project_dir)

cat("=== HISTORY: multiple curves and dates ===\n")
history <- build_history_comparison(
  market$wide_rates,
  c("USD SOFR OIS", "AUD COR OIS", "EUR ESTR OIS"),
  as.Date(c("2025-09-23", "2025-10-21", "2025-10-22")),
  as.Date("2025-09-23")
)
# 汇总表用平均利率和平均变化快速确认每个 curve/date 组合都已生成。
print(aggregate(
  cbind(rate_percent, change_bp) ~ curve + requested_date + effective_date,
  history,
  function(value) round(mean(value, na.rm = TRUE), 3)
))
cat(
  "history rows:", nrow(history),
  "| base-date maximum absolute change:", max(abs(history$change_bp[history$requested_date == as.Date("2025-09-23")]), na.rm = TRUE), "bp\n\n"
)

cat("=== FORWARD AND SINGLE-TRADE CARRY/ROLL ===\n")
# 五个案例同时覆盖正式 zero curve、历史 Proxy、NS、Spline 和日期回退。
curve_cases <- list(
  USD_ZERO_NS = prepare_curve_fit(market, "zero", "USD UNITED STATES OIS", method = "nelson_siegel"),
  EUR_ZERO_SPLINE = prepare_curve_fit(market, "zero", "EUR EUROZONE (vs. 6M EURIBOR)", method = "spline"),
  EUR_ESTR_PROXY = prepare_curve_fit(market, "historical", "EUR ESTR OIS", as.Date("2025-10-22"), "nelson_siegel"),
  AUD_ZERO_NS = prepare_curve_fit(market, "zero", "AUD AUSTRALIA (vs. 6M Bank Bills)", method = "nelson_siegel"),
  USD_SOFR_PROXY = prepare_curve_fit(market, "historical", "USD SOFR OIS", as.Date("2025-10-22"), "spline")
)
for (case_name in names(curve_cases)) {
  bundle <- curve_cases[[case_name]]
  forward <- calculate_forward(bundle$fit, 1, 5, "annual")
  carry_roll <- calculate_carry_roll(bundle$fit, 0, 5, 0.25, "Receive Fixed")
  cat(sprintf(
    "%s | effective=%s | proxy=%s | 1Y->5Y=%.4f%% | carry=%.2f bp | roll=%.2f bp | total=%.2f bp\n",
    case_name, as.character(bundle$effective_date), bundle$proxy, forward$forward_percent,
    carry_roll$carry_bp, carry_roll$roll_bp, carry_roll$total_bp
  ))
}

cat("\n=== CURVE TRADES: USD OIS, 3M hold, DV01-neutral ===\n")
usd_curve <- curve_cases$USD_ZERO_NS$fit
for (structure in c("steepener", "flattener", "long_belly_fly", "short_belly_fly")) {
  legs <- curve_trade_legs(structure, 2, 5, 10, 10000)
  result <- calculate_curve_trade(usd_curve, legs, 0.25, "annual", 10000)
  leg_text <- paste(
    paste(legs$direction, paste0(legs$tenor, "Y"), paste0("DV01=", legs$dv01)),
    collapse = " | "
  )
  cat(sprintf(
    "%s | %s | total P&L=%.0f | equivalent=%.3f bp\n",
    structure, leg_text, result$summary$total_pnl, result$summary$equivalent_total_bp
  ))
}

# 手动 DV01 案例证明用户改腿部风险后，组合结果会按新权重重新计算。
manual_legs <- curve_trade_legs("steepener", 2, 5, 10, 10000)
manual_legs$dv01 <- c(8000, 12000)
manual_result <- calculate_curve_trade(usd_curve, manual_legs, 0.25, "annual", 10000)
cat(sprintf(
  "manual steepener | DV01=8000/12000 | total P&L=%.0f | equivalent=%.3f bp\n",
  manual_result$summary$total_pnl, manual_result$summary$equivalent_total_bp
))
