# ============================================================
# 曲线计算引擎
# ============================================================
# 整体作用：这个文件只负责数学计算，不负责读取 RDS 或生成网页。
# app.R 把选中的期限和利率传进来，本文件返回拟合曲线、forward、carry/roll 和 P&L。

# 市场数据以“百分数”保存，例如 4.25 表示 4.25%。
# 计算时先转成 0.0425，输出到网页前再转回百分数或 bp，避免单位混用。
market_percent_to_decimal <- function(x) {
  suppressWarnings(as.numeric(x)) / 100
}

decimal_to_percent <- function(x) {
  as.numeric(x) * 100
}

decimal_to_bp <- function(x) {
  as.numeric(x) * 10000
}

clean_curve_points <- function(tenor, rates) {
  # 把期限和利率整理成两列，删除无效点，并将相同期限定价取平均。
  # 返回值是后面 NS 和 spline 两种拟合共同使用的标准输入。
  points <- data.frame(
    tenor = suppressWarnings(as.numeric(tenor)),
    rate = suppressWarnings(as.numeric(rates))
  )
  points <- points[is.finite(points$tenor) & is.finite(points$rate) & points$tenor > 0, ]
  if (nrow(points) == 0) return(points)
  points <- stats::aggregate(rate ~ tenor, data = points, FUN = mean)
  points[order(points$tenor), ]
}

ns_yield <- function(t, beta0, beta1, beta2, tau) {
  # Nelson-Siegel 用四个参数把离散报价变成一条平滑曲线。
  # pmax() 防止 t=0 时公式中的除法失效。
  t <- pmax(as.numeric(t), 1e-8)
  loading1 <- (1 - exp(-t / tau)) / (t / tau)
  beta0 + beta1 * loading1 + beta2 * (loading1 - exp(-t / tau))
}

fit_curve <- function(tenor, rates, method = c("nelson_siegel", "spline"),
                      source = "unknown", proxy = FALSE) {
  # method 决定使用可解释的 Nelson-Siegel，还是更贴近市场点的 spline。
  # source/proxy 不参与数学计算，只跟随结果传到网页，提醒用户数据口径。
  method <- match.arg(method)
  points <- clean_curve_points(tenor, rates)
  minimum_points <- if (method == "nelson_siegel") 4 else 3
  if (nrow(points) < minimum_points) {
    stop("Not enough valid curve points for ", method, ": need at least ", minimum_points, call. = FALSE)
  }

  if (method == "nelson_siegel") {
    # 用三组起点分别尝试 optim()，最后选择误差最小的一次，
    # 比只使用单一起点更不容易落入较差的局部结果。
    starts <- list(
      c(tail(points$rate, 1), head(points$rate, 1) - tail(points$rate, 1), 0, 2),
      c(mean(points$rate), -0.01, 0.01, 1),
      c(mean(points$rate), 0.01, -0.01, 5)
    )
    objective <- function(parameters) {
      fitted <- ns_yield(points$tenor, parameters[1], parameters[2], parameters[3], parameters[4])
      sum((points$rate - fitted)^2)
    }
    candidates <- lapply(starts, function(start) {
      try(stats::optim(
        start, objective, method = "L-BFGS-B",
        lower = c(-1, -1, -1, 0.01),
        upper = c(1, 1, 1, 30)
      ), silent = TRUE)
    })
    valid <- candidates[!vapply(candidates, inherits, logical(1), "try-error")]
    if (length(valid) == 0) stop("Nelson-Siegel optimization failed.", call. = FALSE)
    result <- valid[[which.min(vapply(valid, function(x) x$value, numeric(1)))]]
    parameters <- stats::setNames(result$par, c("beta0", "beta1", "beta2", "tau"))
    predict_function <- function(t) {
      ns_yield(t, parameters[["beta0"]], parameters[["beta1"]], parameters[["beta2"]], parameters[["tau"]])
    }
  } else {
    # smooth.spline() 直接穿过或靠近市场点；cv=TRUE 让 R 自动选择平滑程度。
    # 部分市场曲线会让自动搜索过程打印非致命的 spar 警告；结果仍有效，因此安静处理。
    spline_model <- suppressWarnings(stats::smooth.spline(points$tenor, points$rate, cv = TRUE))
    parameters <- c(df = spline_model$df, spar = spline_model$spar)
    predict_function <- function(t) {
      stats::predict(spline_model, x = pmax(as.numeric(t), min(points$tenor)))$y
    }
  }

  fitted_rates <- predict_function(points$tenor)
  residuals <- points$rate - fitted_rates
  structure(list(
    method = method,
    points = points,
    parameters = parameters,
    predict = predict_function,
    diagnostics = data.frame(
      tenor = points$tenor,
      observed_percent = decimal_to_percent(points$rate),
      fitted_percent = decimal_to_percent(fitted_rates),
      residual_bp = decimal_to_bp(residuals)
    ),
    rmse_bp = sqrt(mean(decimal_to_bp(residuals)^2)),
    source = source,
    proxy = isTRUE(proxy)
  ), class = "yield_curve_fit")
}

# curve_rate() 是网页和其他计算函数读取拟合曲线的统一入口。
# 输入一个或多个期限，返回内部小数单位的拟合利率。
curve_rate <- function(curve, tenor) {
  if (!inherits(curve, "yield_curve_fit")) stop("curve must be produced by fit_curve().", call. = FALSE)
  as.numeric(curve$predict(tenor))
}

discount_factor <- function(rate, tenor, compounding = c("annual", "continuous", "simple")) {
  # 根据用户选择的复利口径，把利率转成折现因子。
  compounding <- match.arg(compounding)
  tenor <- as.numeric(tenor)
  rate <- as.numeric(rate)
  if (compounding == "annual") return((1 + rate)^(-tenor))
  if (compounding == "continuous") return(exp(-rate * tenor))
  1 / (1 + rate * tenor)
}

calculate_forward <- function(curve, start, end,
                              compounding = c("annual", "continuous", "simple")) {
  # 用起点和终点折现因子计算 start -> end 的远期利率。
  # 返回数据框是为了让同一个结果可以直接放进 Forward Calculator 表格。
  compounding <- match.arg(compounding)
  start <- as.numeric(start)
  end <- as.numeric(end)
  if (!is.finite(start) || !is.finite(end) || start < 0 || end <= start) {
    stop("Forward end must be greater than start, and start cannot be negative.", call. = FALSE)
  }
  start_rate <- if (start == 0) curve_rate(curve, max(1e-6, min(curve$points$tenor))) else curve_rate(curve, start)
  end_rate <- curve_rate(curve, end)
  df_start <- if (start == 0) 1 else discount_factor(start_rate, start, compounding)
  df_end <- discount_factor(end_rate, end, compounding)
  period <- end - start
  forward <- switch(
    compounding,
    annual = (df_start / df_end)^(1 / period) - 1,
    continuous = log(df_start / df_end) / period,
    simple = (df_start / df_end - 1) / period
  )
  data.frame(
    start_years = start,
    end_years = end,
    forward_percent = decimal_to_percent(forward),
    compounding = compounding,
    source = curve$source,
    proxy = curve$proxy,
    stringsAsFactors = FALSE
  )
}

direction_sign <- function(direction) {
  # Receive Fixed 使用 +1，Pay Fixed 使用 -1。
  # 后续所有 bp 与 P&L 都乘这个符号，所以两个方向结果应正好相反。
  normalized <- tolower(trimws(as.character(direction)))
  if (normalized %in% c("receive", "receive fixed", "receiver", "r", "1")) return(1)
  if (normalized %in% c("pay", "pay fixed", "payer", "p", "-1")) return(-1)
  stop("Direction must be Receive Fixed or Pay Fixed.", call. = FALSE)
}

calculate_carry_roll <- function(curve, start, end, hold,
                                 direction = "Receive Fixed",
                                 compounding = c("annual", "continuous", "simple")) {
  # 把当前 forward 与持有期结束后的 rolled forward 比较，得到 roll；
  # carry 使用交易 forward 减去短端资金成本，是交易员快速查看的近似口径。
  compounding <- match.arg(compounding)
  start <- as.numeric(start)
  end <- as.numeric(end)
  hold <- as.numeric(hold)
  if (!is.finite(hold) || hold <= 0 || hold >= end) {
    stop("Hold must be positive and shorter than the trade end tenor.", call. = FALSE)
  }
  sign <- direction_sign(direction)
  current_forward <- calculate_forward(curve, start, end, compounding)$forward_percent / 100
  rolled_start <- max(0, start - hold)
  rolled_end <- end - hold
  rolled_forward <- calculate_forward(curve, rolled_start, rolled_end, compounding)$forward_percent / 100
  funding_end <- max(hold, min(curve$points$tenor))
  funding_rate <- calculate_forward(curve, 0, funding_end, compounding)$forward_percent / 100

  roll_decimal <- (current_forward - rolled_forward) * sign
  carry_decimal <- current_forward * hold * sign
  funding_decimal <- funding_rate * hold * sign
  net_carry_decimal <- carry_decimal - funding_decimal

  data.frame(
    start_years = start,
    end_years = end,
    hold_years = hold,
    direction = if (sign == 1) "Receive Fixed" else "Pay Fixed",
    carry_bp = decimal_to_bp(net_carry_decimal),
    roll_bp = decimal_to_bp(roll_decimal),
    total_bp = decimal_to_bp(net_carry_decimal + roll_decimal),
    source = curve$source,
    proxy = curve$proxy,
    stringsAsFactors = FALSE
  )
}

calculate_dv01_pnl <- function(total_bp, dv01) {
  # DV01 表示利率移动 1 bp 对应的金额，因此 total bp * DV01 得到估算 P&L。
  total_bp <- as.numeric(total_bp)
  dv01 <- as.numeric(dv01)
  if (!is.finite(total_bp) || !is.finite(dv01) || dv01 < 0) {
    stop("total_bp and dv01 must be finite; dv01 cannot be negative.", call. = FALSE)
  }
  total_bp * dv01
}

build_carry_matrix <- function(curve, tenors, holds, direction, dv01 = 0,
                               compounding = "annual") {
  # 外层循环遍历交易期限，内层循环遍历持有期。
  # 返回的每一行后来同时用于 Carry 页面表格和热力图。
  rows <- list()
  for (tenor in as.numeric(tenors)) {
    for (hold in as.numeric(holds)) {
      if (hold >= tenor) next
      result <- calculate_carry_roll(curve, 0, tenor, hold, direction, compounding)
      result$tenor_label <- paste0(format(tenor, trim = TRUE), "Y")
      result$hold_label <- if (hold < 1) paste0(round(hold * 12), "M") else paste0(format(hold, trim = TRUE), "Y")
      result$pnl <- calculate_dv01_pnl(result$total_bp, dv01)
      rows[[length(rows) + 1]] <- result
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

# curve_trade_legs()
# 根据交易结构生成每条腿的方向和默认 DV01-neutral 风险。
# risk_budget 是组合的基准 DV01：两腿交易每腿使用该 DV01；fly 的 belly 使用该
# DV01，两翼各使用一半，因此 belly DV01 等于两翼 DV01 总和。
curve_trade_legs <- function(structure = c("steepener", "flattener", "long_belly_fly", "short_belly_fly"),
                             short_tenor = 2, belly_tenor = 5, long_tenor = 10,
                             risk_budget = 10000) {
  structure <- match.arg(structure)
  risk_budget <- as.numeric(risk_budget)
  tenors <- c(short = as.numeric(short_tenor), belly = as.numeric(belly_tenor), long = as.numeric(long_tenor))
  if (!is.finite(risk_budget) || risk_budget <= 0) stop("Risk budget must be positive.", call. = FALSE)
  if (!all(is.finite(tenors)) || any(tenors <= 0)) stop("Curve-trade tenors must be positive.", call. = FALSE)
  if (structure %in% c("long_belly_fly", "short_belly_fly") && !(tenors["short"] < tenors["belly"] && tenors["belly"] < tenors["long"])) {
    stop("Fly tenors must satisfy short < belly < long.", call. = FALSE)
  }
  if (structure %in% c("steepener", "flattener") && !(tenors["short"] < tenors["long"])) {
    stop("Curve tenors must satisfy short < long.", call. = FALSE)
  }

  legs <- switch(
    structure,
    steepener = data.frame(
      leg = c("Short", "Long"), tenor = tenors[c("short", "long")],
      direction = c("Receive Fixed", "Pay Fixed"), dv01 = risk_budget
    ),
    flattener = data.frame(
      leg = c("Short", "Long"), tenor = tenors[c("short", "long")],
      direction = c("Pay Fixed", "Receive Fixed"), dv01 = risk_budget
    ),
    long_belly_fly = data.frame(
      leg = c("Short Wing", "Belly", "Long Wing"), tenor = tenors,
      direction = c("Pay Fixed", "Receive Fixed", "Pay Fixed"),
      dv01 = c(risk_budget / 2, risk_budget, risk_budget / 2)
    ),
    short_belly_fly = data.frame(
      leg = c("Short Wing", "Belly", "Long Wing"), tenor = tenors,
      direction = c("Receive Fixed", "Pay Fixed", "Receive Fixed"),
      dv01 = c(risk_budget / 2, risk_budget, risk_budget / 2)
    )
  )
  legs$structure <- structure
  rownames(legs) <- NULL
  legs
}

# calculate_curve_trade()
# 每条腿复用单腿 calculate_carry_roll()，再用该腿绝对 DV01 转成 P&L 并汇总。
# 用户可把 curve_trade_legs() 返回的 dv01 列改成手动值后再传入本函数。
calculate_curve_trade <- function(curve, legs, hold, compounding = "annual",
                                  risk_budget = NULL) {
  required <- c("leg", "tenor", "direction", "dv01")
  if (!all(required %in% names(legs))) stop("legs must contain: ", paste(required, collapse = ", "), call. = FALSE)
  rows <- lapply(seq_len(nrow(legs)), function(index) {
    leg <- legs[index, , drop = FALSE]
    result <- calculate_carry_roll(curve, 0, leg$tenor, hold, leg$direction, compounding)
    result$leg <- leg$leg
    result$tenor <- leg$tenor
    result$dv01 <- as.numeric(leg$dv01)
    result$carry_pnl <- calculate_dv01_pnl(result$carry_bp, result$dv01)
    result$roll_pnl <- calculate_dv01_pnl(result$roll_bp, result$dv01)
    result$total_pnl <- calculate_dv01_pnl(result$total_bp, result$dv01)
    result
  })
  detail <- do.call(rbind, rows)
  risk_budget <- if (is.null(risk_budget)) max(detail$dv01) else as.numeric(risk_budget)
  summary <- data.frame(
    carry_pnl = sum(detail$carry_pnl),
    roll_pnl = sum(detail$roll_pnl),
    total_pnl = sum(detail$total_pnl),
    equivalent_total_bp = sum(detail$total_pnl) / risk_budget,
    risk_budget = risk_budget,
    source = curve$source,
    proxy = curve$proxy,
    stringsAsFactors = FALSE
  )
  list(detail = detail, summary = summary)
}
