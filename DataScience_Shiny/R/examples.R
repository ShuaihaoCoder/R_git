run_example <- function(example_id, data_bundle) {
  switch(
    example_id,
    linear_regression = run_linear_regression_example(data_bundle),
    polynomial_regression = run_polynomial_regression_example(data_bundle),
    correlation = run_correlation_example(data_bundle),
    partial_correlation = run_partial_correlation_example(data_bundle),
    logistic_regression = run_logistic_example(data_bundle),
    arima = run_arima_example(data_bundle),
    pca = run_pca_example(data_bundle),
    bayesian_scenario = run_bayesian_scenario_example(data_bundle),
    method_placeholder(example_id)
  )
}

method_placeholder <- function(example_id) {
  list(
    title = "Reference Notes",
    background = "This method is included in the encyclopedia index. The first version keeps the explanation and reusable code pattern ready, while the full live example can be expanded later from DataScience_original_reference.R.",
    code = "# Add a live example here by following the original DataScience.R logic.",
    table = data.frame(status = "Documented method page; live example pending.", stringsAsFactors = FALSE),
    plot = NULL,
    model_summary = "No live model is run for this method yet."
  )
}

run_linear_regression_example <- function(data_bundle) {
  cad_data <- prepare_cad_market_data(data_bundle)
  fit <- lm(USDCAD_ret ~ TSXC_ret + delta10y, data = cad_data)

  plot_object <- ggplot2::ggplot(cad_data, ggplot2::aes(TSXC_ret, USDCAD_ret)) +
    ggplot2::geom_point(alpha = 0.35, color = "#335C67") +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#D95D39") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(
      title = "USDCAD Return vs Canadian Equity Return",
      x = "TSXC log return",
      y = "USDCAD log return"
    )

  list(
    title = "Linear Regression: USDCAD returns explained by equity and rate shocks",
    background = "The case asks whether Canadian equity risk sentiment and 10-year yield changes help explain daily USDCAD movements.",
    code = paste(
      "cad_data <- prepare_cad_market_data(data_bundle)",
      "fit <- lm(USDCAD_ret ~ TSXC_ret + delta10y, data = cad_data)",
      "summary(fit)",
      sep = "\n"
    ),
    table = broom_like_coefficients(fit),
    plot = plot_object,
    model_summary = capture.output(summary(fit))
  )
}

run_polynomial_regression_example <- function(data_bundle) {
  cad_data <- prepare_cad_market_data(data_bundle)
  fit_linear <- lm(USDCAD_ret ~ TSXC_ret, data = cad_data)
  fit_poly <- lm(USDCAD_ret ~ TSXC_ret + I(TSXC_ret^2), data = cad_data)

  plot_object <- ggplot2::ggplot(cad_data, ggplot2::aes(TSXC_ret, USDCAD_ret)) +
    ggplot2::geom_point(alpha = 0.3, color = "#445E93") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#D95D39") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "#2A9D8F") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(title = "Linear vs Polynomial Fit", x = "TSXC return", y = "USDCAD return")

  list(
    title = "Polynomial Regression: testing curved market relationships",
    background = "This checks whether the FX/equity relationship becomes non-linear during larger market moves.",
    code = "lm(USDCAD_ret ~ TSXC_ret + I(TSXC_ret^2), data = cad_data)",
    table = data.frame(
      model = c("Linear", "Polynomial"),
      AIC = c(AIC(fit_linear), AIC(fit_poly)),
      BIC = c(BIC(fit_linear), BIC(fit_poly))
    ),
    plot = plot_object,
    model_summary = capture.output(summary(fit_poly))
  )
}

run_correlation_example <- function(data_bundle) {
  cad_data <- prepare_cad_market_data(data_bundle)
  corr_data <- cad_data[, .(USDCAD_ret, TSXC_ret, delta10y, CAD_ON)]
  corr_matrix <- round(cor(corr_data, use = "pairwise.complete.obs"), 3)
  corr_long <- as.data.frame(as.table(corr_matrix))
  names(corr_long) <- c("x", "y", "correlation")

  plot_object <- ggplot2::ggplot(corr_long, ggplot2::aes(x, y, fill = correlation)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = correlation), size = 4) +
    ggplot2::scale_fill_gradient2(low = "#2B6CB0", mid = "white", high = "#C2410C", limits = c(-1, 1)) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(title = "Correlation Heatmap", x = NULL, y = NULL)

  list(
    title = "Correlation: market variables moving together",
    background = "The case measures pairwise association among FX returns, equity returns, yield changes, and overnight CAD rates.",
    code = "cor(cad_data[, .(USDCAD_ret, TSXC_ret, delta10y, CAD_ON)], use = 'pairwise.complete.obs')",
    table = as.data.frame(corr_matrix),
    plot = plot_object,
    model_summary = capture.output(cor.test(cad_data$USDCAD_ret, cad_data$TSXC_ret))
  )
}

run_partial_correlation_example <- function(data_bundle) {
  cad_data <- prepare_cad_market_data(data_bundle)
  corr_data <- na.omit(cad_data[, .(USDCAD_ret, TSXC_ret, delta10y, CAD_ON)])
  inv_cov <- solve(cov(corr_data))
  pcor <- -cov2cor(inv_cov)
  diag(pcor) <- 1

  pcor_long <- as.data.frame(as.table(round(pcor, 3)))
  names(pcor_long) <- c("x", "y", "partial_correlation")

  plot_object <- ggplot2::ggplot(pcor_long, ggplot2::aes(x, y, fill = partial_correlation)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = partial_correlation), size = 4) +
    ggplot2::scale_fill_gradient2(low = "#355070", mid = "white", high = "#B56576", limits = c(-1, 1)) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(title = "Partial Correlation Matrix", x = NULL, y = NULL)

  list(
    title = "Partial Correlation: relationship after controls",
    background = "The case asks which market relationships remain after controlling for the rest of the CAD market feature set.",
    code = "inv_cov <- solve(cov(corr_data)); pcor <- -cov2cor(inv_cov); diag(pcor) <- 1",
    table = as.data.frame(round(pcor, 3)),
    plot = plot_object,
    model_summary = "Partial correlations are computed from the inverse covariance matrix."
  )
}

run_logistic_example <- function(data_bundle) {
  cad_data <- prepare_cad_market_data(data_bundle)
  cad_data[, direction_up := factor(ifelse(data.table::shift(USDCAD_ret, type = "lead") > 0, 1, 0))]
  model_data <- na.omit(cad_data[, .(direction_up, USDCAD_ret, TSXC_ret, delta10y, CAD_ON)])
  fit <- glm(direction_up ~ USDCAD_ret + TSXC_ret + delta10y + CAD_ON, data = model_data, family = binomial)
  model_data[, probability := predict(fit, type = "response")]
  model_data[, predicted := factor(ifelse(probability > 0.5, 1, 0), levels = levels(direction_up))]

  confusion <- as.data.frame.matrix(table(Predicted = model_data$predicted, Actual = model_data$direction_up))

  plot_object <- ggplot2::ggplot(model_data, ggplot2::aes(probability, fill = direction_up)) +
    ggplot2::geom_density(alpha = 0.35) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(title = "Predicted Probability Distribution", x = "Probability of next-day USDCAD up", y = "Density")

  list(
    title = "Logistic Regression: next-day FX direction",
    background = "The case translates market features into a probability that USDCAD will rise the next day.",
    code = "glm(direction_up ~ USDCAD_ret + TSXC_ret + delta10y + CAD_ON, family = binomial, data = model_data)",
    table = confusion,
    plot = plot_object,
    model_summary = capture.output(summary(fit))
  )
}

run_arima_example <- function(data_bundle) {
  cad_data <- prepare_cad_market_data(data_bundle)
  series <- na.omit(cad_data$delta10y)
  fit <- stats::arima(series, order = c(1, 0, 1))
  fitted_values <- series - residuals(fit)
  plot_data <- data.frame(index = seq_along(series), actual = series, fitted = as.numeric(fitted_values))

  plot_object <- ggplot2::ggplot(plot_data, ggplot2::aes(index)) +
    ggplot2::geom_line(ggplot2::aes(y = actual, color = "Actual"), alpha = 0.75) +
    ggplot2::geom_line(ggplot2::aes(y = fitted, color = "ARIMA fitted"), alpha = 0.9) +
    ggplot2::scale_color_manual(values = c("Actual" = "#264653", "ARIMA fitted" = "#E76F51")) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(title = "ARIMA(1,0,1) on Canada 10Y Yield Changes", x = "Time index", y = "delta10y", color = NULL)

  list(
    title = "ARIMA: univariate rate-change dynamics",
    background = "The case models the persistence and shock correction pattern of daily Canada 10-year yield changes.",
    code = "fit <- arima(na.omit(cad_data$delta10y), order = c(1, 0, 1))",
    table = data.frame(parameter = names(fit$coef), estimate = as.numeric(fit$coef)),
    plot = plot_object,
    model_summary = capture.output(fit)
  )
}

run_pca_example <- function(data_bundle) {
  cad_data <- prepare_cad_market_data(data_bundle)
  pca_data <- na.omit(cad_data[, .(USDCAD_ret, TSXC_ret, delta10y, CAD_ON)])
  pca_fit <- prcomp(pca_data, scale. = TRUE)
  score_data <- as.data.frame(pca_fit$x[, 1:2])
  score_data$date <- cad_data$date[seq_len(nrow(score_data))]
  variance <- round(100 * summary(pca_fit)$importance[2, 1:2], 1)

  plot_object <- ggplot2::ggplot(score_data, ggplot2::aes(PC1, PC2)) +
    ggplot2::geom_point(alpha = 0.35, color = "#386641") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(
      title = "PCA Score Map",
      x = paste0("PC1 (", variance[1], "%)"),
      y = paste0("PC2 (", variance[2], "%)")
    )

  list(
    title = "PCA: compressing CAD market features",
    background = "The case reduces several correlated market variables into orthogonal components that summarize common movement.",
    code = "pca_fit <- prcomp(pca_data, scale. = TRUE)",
    table = as.data.frame(round(pca_fit$rotation[, 1:2], 3)),
    plot = plot_object,
    model_summary = capture.output(summary(pca_fit))
  )
}

run_bayesian_scenario_example <- function(data_bundle) {
  cad_data <- prepare_cad_market_data(data_bundle)
  dt <- data.table::copy(cad_data)
  dt[, usdcad_next := data.table::shift(USDCAD_ret, type = "lead")]
  dt <- na.omit(dt)
  dt[, scenario := data.table::fifelse(
    TSXC_ret < -0.005 & delta10y <= 0, "RiskOff",
    data.table::fifelse(TSXC_ret > 0.005 & delta10y >= 0, "RiskOn", "Neutral")
  )]

  fx_stats <- dt[, .(mu = mean(usdcad_next), sigma = sd(usdcad_next), n = .N), by = scenario]
  dist_stats <- dt[, .(
    tsx_mu = mean(TSXC_ret),
    tsx_sd = sd(TSXC_ret),
    rate_mu = mean(delta10y),
    rate_sd = sd(delta10y),
    prior = .N / nrow(dt)
  ), by = scenario]

  calc_posterior <- function(tsx, rate) {
    tmp <- data.table::copy(dist_stats)
    tmp[, likelihood := dnorm(tsx, tsx_mu, pmax(tsx_sd, 1e-8)) * dnorm(rate, rate_mu, pmax(rate_sd, 1e-8))]
    tmp[, post_raw := likelihood * prior]
    tmp[, posterior := post_raw / sum(post_raw)]
    tmp[, .(scenario, posterior)]
  }

  posterior_list <- dt[, calc_posterior(TSXC_ret, delta10y), by = date]
  posterior_ev <- merge(posterior_list, fx_stats[, .(scenario, mu)], by = "scenario", all.x = TRUE)
  ev_daily <- posterior_ev[, .(EV = sum(posterior * mu)), by = date]
  final_table <- merge(ev_daily, dt[, .(date, usdcad_next)], by = "date")

  plot_object <- ggplot2::ggplot(posterior_list, ggplot2::aes(date, posterior, fill = scenario)) +
    ggplot2::geom_area(alpha = 0.8) +
    ggplot2::scale_fill_manual(values = c(Neutral = "#E9C46A", RiskOff = "#E76F51", RiskOn = "#2A9D8F")) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(title = "Posterior Macro Scenario Probability", x = NULL, y = "Probability")

  list(
    title = "Bayesian Scenario Analysis: market regime probabilities",
    background = "The case classifies daily CAD market conditions into RiskOn, RiskOff, or Neutral regimes and converts those probabilities into expected USDCAD return.",
    code = "posterior <- likelihood(TSXC_ret, delta10y | scenario) * prior(scenario)",
    table = head(final_table, 20),
    plot = plot_object,
    model_summary = capture.output(print(fx_stats))
  )
}

broom_like_coefficients <- function(fit) {
  coef_table <- as.data.frame(summary(fit)$coefficients)
  coef_table$term <- rownames(coef_table)
  rownames(coef_table) <- NULL
  coef_table <- coef_table[, c("term", setdiff(names(coef_table), "term"))]
  names(coef_table) <- c("term", "estimate", "std_error", "statistic", "p_value")
  coef_table
}
