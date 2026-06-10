# ============================================================
# Runnable examples used by the Shiny app
# ============================================================
# 整体功能：
# 每个 run_*_example() 负责一个具体案例，但都返回相同结构。
# app.R 只负责展示返回值，不需要知道每个模型内部如何计算。

# run_example()
# 功能：根据 example_id 把请求发送到对应案例函数。
# 参数：example_id 是案例 ID；data_bundle 是全部数据的命名 list。
# 返回：包含 title/background/code/table/plot/model_summary 的标准 list。
run_example <- function(example_id, data_bundle) {
  # 根据 example_id 选择对应案例；未实现的案例统一返回占位内容。
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

# 尚未实现实时计算的方法使用此占位函数，保证网页页面仍可正常展示。
# 参数 example_id 预留给未来扩展；返回标准案例 list，其中 plot = NULL。
method_placeholder <- function(example_id) {
  # 返回与真实案例相同的字段，使 app.R 不需要额外处理未完成页面。
  list(
    title = "Reference Notes",
    background = "This method is included in the encyclopedia index. The first version keeps the explanation and reusable code pattern ready, while the full live example can be expanded later from DataScience_original_reference.R.",
    code = "# Add a live example here by following the original DataScience.R logic.",
    table = data.frame(status = "Documented method page; live example pending.", stringsAsFactors = FALSE),
    plot = NULL,
    model_summary = "No live model is run for this method yet."
  )
}

# 功能：用加拿大股指收益率和 10 年期利率变化解释 USDCAD 收益率。
# 参数 data_bundle：load_wide_data() 返回的数据；返回回归表、图和模型摘要。
run_linear_regression_example <- function(data_bundle) {
  # 取得多个市场案例共同使用的 CAD 数据。
  cad_data <- prepare_cad_market_data(data_bundle)

  # lm(formula, data)：formula 左边是因变量，右边是解释变量。
  fit <- lm(USDCAD_ret ~ TSXC_ret + delta10y, data = cad_data)

  # 创建散点图，并叠加线性回归拟合线和置信区间。
  plot_object <- ggplot2::ggplot(cad_data, ggplot2::aes(TSXC_ret, USDCAD_ret)) +
    ggplot2::geom_point(alpha = 0.35, color = "#335C67") +
    # method = "lm" 添加线性拟合线；se = TRUE 显示置信区间。
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#D95D39") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(
      title = "USDCAD Return vs Canadian Equity Return",
      x = "TSXC log return",
      y = "USDCAD log return"
    )

  # 将模型结果整理成 app.R 可以统一展示的标准案例 list。
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

# 功能：比较线性关系和二次曲线关系。
# I(TSXC_ret^2) 把平方项作为解释变量；AIC/BIC 通常越小越好。
run_polynomial_regression_example <- function(data_bundle) {
  # 使用同一份数据分别拟合直线模型和包含平方项的曲线模型。
  cad_data <- prepare_cad_market_data(data_bundle)
  fit_linear <- lm(USDCAD_ret ~ TSXC_ret, data = cad_data)
  fit_poly <- lm(USDCAD_ret ~ TSXC_ret + I(TSXC_ret^2), data = cad_data)

  # 在同一张图中展示直线拟合和二次曲线拟合。
  plot_object <- ggplot2::ggplot(cad_data, ggplot2::aes(TSXC_ret, USDCAD_ret)) +
    ggplot2::geom_point(alpha = 0.3, color = "#445E93") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#D95D39") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "#2A9D8F") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(title = "Linear vs Polynomial Fit", x = "TSXC return", y = "USDCAD return")

  # 返回 AIC/BIC 比较表、曲线图和二次模型摘要。
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

# 功能：计算普通相关系数并生成热力图。
# use = "pairwise.complete.obs" 表示每对变量使用该对都有值的观测。
run_correlation_example <- function(data_bundle) {
  # 选择用于比较共同变化关系的四个连续变量。
  cad_data <- prepare_cad_market_data(data_bundle)
  corr_data <- cad_data[, .(USDCAD_ret, TSXC_ret, delta10y, CAD_ON)]
  # 计算相关矩阵，并转换成长表格式用于 ggplot 热力图。
  corr_matrix <- round(cor(corr_data, use = "pairwise.complete.obs"), 3)
  corr_long <- as.data.frame(as.table(corr_matrix))
  names(corr_long) <- c("x", "y", "correlation")

  # 使用颜色和数字同时表达相关性的方向与强度。
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

# 功能：计算控制其他变量后，两变量之间剩余的相关性。
# solve(cov(...)) 求协方差矩阵的逆；-cov2cor() 将其转换为偏相关矩阵。
run_partial_correlation_example <- function(data_bundle) {
  # 删除缺失值，确保协方差矩阵可以计算和求逆。
  cad_data <- prepare_cad_market_data(data_bundle)
  corr_data <- na.omit(cad_data[, .(USDCAD_ret, TSXC_ret, delta10y, CAD_ON)])
  # 从逆协方差矩阵计算控制其他变量后的偏相关矩阵。
  inv_cov <- solve(cov(corr_data))
  pcor <- -cov2cor(inv_cov)
  diag(pcor) <- 1

  # 将偏相关矩阵转成长表格式，供 ggplot 绘图。
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

# 功能：预测下一日 USDCAD 收益率方向。
# glm(..., family = binomial) 拟合二分类模型；type = "response" 返回 0-1 概率。
run_logistic_example <- function(data_bundle) {
  # 使用下一日 USDCAD 收益率方向创建二分类目标变量。
  cad_data <- prepare_cad_market_data(data_bundle)
  cad_data[, direction_up := factor(ifelse(data.table::shift(USDCAD_ret, type = "lead") > 0, 1, 0))]
  model_data <- na.omit(cad_data[, .(direction_up, USDCAD_ret, TSXC_ret, delta10y, CAD_ON)])
  # 拟合 Logistic Regression，并取得每行上涨概率。
  fit <- glm(direction_up ~ USDCAD_ret + TSXC_ret + delta10y + CAD_ON, data = model_data, family = binomial)
  model_data[, probability := predict(fit, type = "response")]
  model_data[, predicted := factor(ifelse(probability > 0.5, 1, 0), levels = levels(direction_up))]

  # 比较预测分类和真实分类，生成 confusion matrix。
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

# 功能：用 ARIMA(1,0,1) 描述加拿大 10 年期收益率日变化。
# order = c(p,d,q)：p 是 AR 阶数，d 是差分次数，q 是 MA 阶数。
run_arima_example <- function(data_bundle) {
  # 取出加拿大 10 年期收益率日变化作为单变量时间序列。
  cad_data <- prepare_cad_market_data(data_bundle)
  series <- na.omit(cad_data$delta10y)
  # 拟合 ARIMA(1,0,1)，并利用残差反推出样本内拟合值。
  fit <- stats::arima(series, order = c(1, 0, 1))
  fitted_values <- series - residuals(fit)
  plot_data <- data.frame(index = seq_along(series), actual = series, fitted = as.numeric(fitted_values))

  # 比较真实收益率变化和 ARIMA 样本内拟合结果。
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

# 功能：把多个相关市场变量压缩为 PC1、PC2 等综合变量。
# scale. = TRUE 先标准化变量；rotation 是权重；x 是每个日期的主成分分数。
run_pca_example <- function(data_bundle) {
  # 选取多个市场变量，并删除无法参与 PCA 的缺失行。
  cad_data <- prepare_cad_market_data(data_bundle)
  pca_data <- na.omit(cad_data[, .(USDCAD_ret, TSXC_ret, delta10y, CAD_ON)])
  # 标准化变量后运行 PCA，并提取前两个主成分分数。
  pca_fit <- prcomp(pca_data, scale. = TRUE)
  score_data <- as.data.frame(pca_fit$x[, 1:2])
  score_data$date <- cad_data$date[seq_len(nrow(score_data))]
  # 计算 PC1 和 PC2 各自解释的总数据变化百分比。
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

# 功能：计算 RiskOn/RiskOff/Neutral 后验概率，再计算 USDCAD 期望收益。
run_bayesian_scenario_example <- function(data_bundle) {
  # 创建下一日 FX 收益率，并按股指和利率条件定义市场场景。
  cad_data <- prepare_cad_market_data(data_bundle)
  dt <- data.table::copy(cad_data)
  dt[, usdcad_next := data.table::shift(USDCAD_ret, type = "lead")]
  dt <- na.omit(dt)
  dt[, scenario := data.table::fifelse(
    TSXC_ret < -0.005 & delta10y <= 0, "RiskOff",
    data.table::fifelse(TSXC_ret > 0.005 & delta10y >= 0, "RiskOn", "Neutral")
  )]

  # 计算各场景下的 FX 收益统计，以及场景特征分布和先验概率。
  fx_stats <- dt[, .(mu = mean(usdcad_next), sigma = sd(usdcad_next), n = .N), by = scenario]
  dist_stats <- dt[, .(
    tsx_mu = mean(TSXC_ret),
    tsx_sd = sd(TSXC_ret),
    rate_mu = mean(delta10y),
    rate_sd = sd(delta10y),
    prior = .N / nrow(dt)
  ), by = scenario]

  # calc_posterior(tsx, rate)：
  # 输入某天的股指收益率和利率变化，返回三个场景的 posterior 概率。
  # posterior = likelihood * prior，再归一化使概率总和为 1。
  calc_posterior <- function(tsx, rate) {
    tmp <- data.table::copy(dist_stats)
    tmp[, likelihood := dnorm(tsx, tsx_mu, pmax(tsx_sd, 1e-8)) * dnorm(rate, rate_mu, pmax(rate_sd, 1e-8))]
    tmp[, post_raw := likelihood * prior]
    tmp[, posterior := post_raw / sum(post_raw)]
    tmp[, .(scenario, posterior)]
  }

  # 对每个日期计算场景概率，并用场景收益均值计算期望收益。
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

# 把 summary(fit)$coefficients 矩阵整理成网页易展示的数据框。
# 参数 fit 是 lm() 等模型对象；返回系数、标准误、统计量和 p-value。
broom_like_coefficients <- function(fit) {
  # 从模型摘要中取出系数矩阵，并将行名转换为普通 term 列。
  coef_table <- as.data.frame(summary(fit)$coefficients)
  coef_table$term <- rownames(coef_table)
  rownames(coef_table) <- NULL
  coef_table <- coef_table[, c("term", setdiff(names(coef_table), "term"))]
  # 使用统一列名，让 Shiny 结果表更容易理解和复用。
  names(coef_table) <- c("term", "estimate", "std_error", "statistic", "p_value")
  coef_table
}
