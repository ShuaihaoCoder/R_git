# ============================================================
# Complete runnable case studies
# ============================================================
# 整体作用：为目录中的全部 24 个方法提供真实案例，不再返回空图或 placeholder。
# 每个案例保留原 DataScience.R 的核心思路，并返回多图、多表、检验和教学步骤。

# run_example()
# 功能：全部案例的统一入口；app.R 只需要提供 example_id 和 data_bundle。
# 返回值由 new_case() 统一整理，因此网页可以用相同方式展示所有方法。
run_example <- function(example_id, data_bundle) {
  # switch() 根据 example_id 只运行一个对应案例；如果 ID 不存在，stop() 会直接指出错误名称。
  switch(
    example_id,
    independence_test = case_independence(),
    correlation = case_correlation(data_bundle),
    partial_correlation = case_partial_correlation(data_bundle),
    linear_regression = case_regression(data_bundle, polynomial = FALSE),
    polynomial_regression = case_regression(data_bundle, polynomial = TRUE),
    subset_regression = case_subset_regression(data_bundle),
    anova = case_anova(data_bundle),
    ancova = case_ancova(data_bundle),
    manova = case_manova(data_bundle),
    poisson_glm = case_poisson(),
    logistic_regression = case_logistic(data_bundle, view = "model"),
    confusion_matrix = case_logistic(data_bundle, view = "confusion"),
    roc = case_logistic(data_bundle, view = "roc"),
    arima = case_arima(data_bundle, seasonal = FALSE),
    sarima = case_arima(data_bundle, seasonal = TRUE),
    garch = case_garch(data_bundle),
    var = case_var(data_bundle, view = "var"),
    granger = case_var(data_bundle, view = "granger"),
    efa = case_efa(data_bundle),
    pca = case_pca(data_bundle),
    rolling_pca = case_rolling_pca(data_bundle),
    cluster = case_cluster(data_bundle),
    power_analysis = case_power(),
    bayesian_scenario = case_bayesian(data_bundle),
    stop("Unknown example_id: ", example_id, call. = FALSE)
  )
}

# base_steps()
# 功能：生成大多数案例共用的教学顺序，具体案例可补充额外步骤。
base_steps <- function(method, diagnostic) {
  # teaching_steps() 来自 R/case_helpers.R；每个 c() 分别提供步骤标题和英文教学说明。
  teaching_steps(
    c("1. Frame the question", paste("Define what", method, "should answer before fitting it.")),
    c("2. Prepare the data", "Select the variables, align observations, and handle missing values."),
    c("3. Fit or calculate", paste("Run", method, "and collect the central estimates.")),
    c("4. Diagnose", diagnostic),
    c("5. Interpret", "Translate the statistical result back into the financial or macro question.")
  )
}

# case_independence()
# 功能：复现原脚本的分类变量独立性案例，并同时展示 Chi-square、Fisher 和 CMH。
case_independence <- function() {
  # HairEyeColor 是 R 自带的三维分类表，因此不需要为此案例安装额外 package。
  hair_eye <- as.data.frame(HairEyeColor)
  # xtabs() 按 Hair 和 Eye 汇总人数，得到独立性检验使用的二维列联表。
  treatment_table <- xtabs(Freq ~ Hair + Eye, data = hair_eye)
  # 三个检验回答相近但不完全相同的问题：普通关联、精确概率、控制 Sex 后的关联。
  chi <- stats::chisq.test(treatment_table)
  # 大型列联表的精确枚举非常慢；simulate.p.value 用 Monte Carlo 得到可复现的近似 p-value。
  set.seed(123)
  fisher <- stats::fisher.test(treatment_table, simulate.p.value = TRUE, B = 5000)
  cmh <- stats::mantelhaen.test(HairEyeColor)

  plot_data <- as.data.frame(treatment_table)
  names(plot_data) <- c("Hair", "Eye", "Count")
  # ave() 在每种 Hair 颜色内部计算比例，使第二张图不受各组总人数大小影响。
  proportion_data <- transform(plot_data, Proportion = ave(Count, Hair, FUN = function(x) x / sum(x)))

  count_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(Hair, Count, fill = Eye)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(title = "Observed Arthritis Outcomes", y = "Patients") +
    standard_theme()
  proportion_plot <- ggplot2::ggplot(proportion_data, ggplot2::aes(Hair, Proportion, fill = Eye)) +
    ggplot2::geom_col() +
    ggplot2::labs(title = "Outcome Composition Within Treatment", y = "Proportion") +
    standard_theme()

  # new_case() 将图、表、检验和解释包装成 app.R 可以统一展示的结构。
  new_case(
    "Independence Test: hair color and eye color",
    "The HairEyeColor table asks whether two visible categorical traits are independent while sex provides a possible control layer.",
    "Does the distribution of eye color change across hair-color groups?",
    "Learn when Chi-square, Fisher exact, and stratified CMH tests answer related categorical questions.",
    variable_rows(
      Hair = "Hair-color category.",
      Eye = "Eye-color category.",
      Sex = "Sex category, used as a control layer in the CMH test."
    ),
    base_steps("independence testing", "Compare observed counts with counts expected under independence."),
    list("Observed counts" = count_plot, "Within-group proportions" = proportion_plot),
    list("Contingency table" = as.data.frame.matrix(treatment_table), "Expected counts under H0" = as.data.frame(chi$expected)),
    bind_tests(
      test_result("Chi-square independence", "Hair color and eye color are independent.", chi$statistic, chi$p.value),
      test_result("Fisher exact", "Hair color and eye color are independent.", NA, fisher$p.value),
      test_result("Cochran-Mantel-Haenszel", "Hair color and eye color are independent within sex strata.", cmh$statistic, cmh$p.value)
    ),
    capture.output(chi),
    "chisq.test(HairEyeColor); fisher.test(...); mantelhaen.test(HairEyeColor)",
    "Use the p-values together with the proportion chart: significance tells us an association exists, while the chart shows its direction."
  )
}

# case_correlation()
# 功能：展示普通相关矩阵、散点关系和显著性检验。
case_correlation <- function(data_bundle) {
  # prepare_cad_market_data() 来自 R/data_loader.R，返回多个 CAD 案例共用的市场数据。
  cad <- prepare_cad_market_data(data_bundle)
  data <- as.data.frame(cad[, .(USDCAD_ret, TSXC_ret, delta10y, CAD_ON)])
  # cor() 计算完整相关矩阵；cor.test() 对最重要的一组关系额外计算显著性。
  correlation <- stats::cor(data, use = "pairwise.complete.obs")
  test <- stats::cor.test(data$USDCAD_ret, data$TSXC_ret)

  scatter <- ggplot2::ggplot(data, ggplot2::aes(TSXC_ret, USDCAD_ret)) +
    ggplot2::geom_point(alpha = 0.3, color = "#335C67") +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#D95D39") +
    ggplot2::labs(title = "FX vs Equity Return", x = "TSXC return", y = "USDCAD return") +
    standard_theme()

  # 同时返回热力图和散点图，让学生分别看到整体关系与一组具体关系。
  new_case(
    "Correlation: CAD market variables moving together",
    "This case measures which CAD market variables usually rise and fall together.",
    "How strongly are FX returns, equity returns, rate shocks, and overnight rates associated?",
    "Separate the strength and direction of association from causal interpretation.",
    describe_variables(),
    base_steps("correlation", "Check scatterplots, outliers, and statistical significance before interpreting coefficients."),
    list("Correlation heatmap" = matrix_heatmap(correlation, "CAD Market Correlation Matrix"), "Key pair scatterplot" = scatter),
    list("Correlation matrix" = round(correlation, 4)),
    bind_tests(test_result("Pearson correlation", "USDCAD and TSXC returns have zero linear correlation.", test$statistic, test$p.value)),
    capture.output(test),
    "cor(cad_variables, use = 'pairwise.complete.obs'); cor.test(USDCAD_ret, TSXC_ret)",
    "Correlation summarizes co-movement. A strong coefficient is useful for monitoring and modeling, but it does not prove one market causes the other."
  )
}

# case_partial_correlation()
# 功能：比较普通相关与控制其他变量后的偏相关。
case_partial_correlation <- function(data_bundle) {
  data <- prepare_factor_data(data_bundle)[, 1:4]
  correlation <- stats::cor(data)
  # solve() 求协方差矩阵的逆；再用 cov2cor() 转换成控制其他变量后的偏相关。
  inverse_covariance <- solve(stats::cov(data))
  partial <- -stats::cov2cor(inverse_covariance)
  diag(partial) <- 1
  comparison <- data.frame(
    pair = paste(row(correlation), col(correlation), sep = "-"),
    correlation = as.numeric(correlation),
    partial_correlation = as.numeric(partial)
  )

  # 点离虚线越远，说明控制其他变量后，这一对变量的关系变化越明显。
  comparison_plot <- ggplot2::ggplot(comparison, ggplot2::aes(correlation, partial_correlation)) +
    ggplot2::geom_point(alpha = 0.65, color = "#2B6CB0") +
    ggplot2::geom_abline(linetype = "dashed", color = "#C2410C") +
    ggplot2::labs(title = "Correlation vs Partial Correlation", x = "Ordinary correlation", y = "Partial correlation") +
    standard_theme()

  new_case(
    "Partial Correlation: relationships after controls",
    "Ordinary correlation can be driven by shared exposure to other variables. Partial correlation removes those linear controls.",
    "Which CAD market relationships remain after controlling for the rest of the feature set?",
    "Recognize relationships that weaken after shared market drivers are removed.",
    variable_rows(
      USDCAD_ret = "Daily log return of USDCAD.",
      TSXC_ret = "Daily log return of the Canadian equity proxy.",
      delta10y = "Daily change in Canada 10-year yield.",
      CAD_ON = "CAD overnight money-market rate."
    ),
    base_steps("partial correlation", "Compare the ordinary and partial matrices; large changes suggest shared-variable effects."),
    list("Partial correlation heatmap" = matrix_heatmap(partial, "Partial Correlation Matrix"), "Ordinary vs partial" = comparison_plot),
    list("Partial correlation matrix" = round(partial, 4), "Ordinary correlation matrix" = round(correlation, 4)),
    bind_tests(),
    "Partial correlations are calculated from the inverse covariance matrix.",
    "inverse_covariance <- solve(cov(data)); partial <- -cov2cor(inverse_covariance)",
    "Pairs far from the diagonal in the comparison plot are the clearest examples where ordinary correlation overstated or understated the direct relationship."
  )
}

# case_regression()
# 功能：展示线性或二次回归，并提供拟合、残差和模型检验。
case_regression <- function(data_bundle, polynomial = FALSE) {
  # polynomial 决定使用直线模型，还是额外加入 TSXC_ret 的平方项。
  cad <- as.data.frame(prepare_cad_market_data(data_bundle))
  formula <- if (polynomial) USDCAD_ret ~ TSXC_ret + I(TSXC_ret^2) + delta10y else USDCAD_ret ~ TSXC_ret + delta10y
  fit <- stats::lm(formula, data = cad)
  # baseline 始终是直线模型，用来判断二次项是否真正改善 AIC、BIC 和调整后 R²。
  baseline <- stats::lm(USDCAD_ret ~ TSXC_ret + delta10y, data = cad)
  fit_data <- data.frame(fitted = stats::fitted(fit), residual = stats::residuals(fit))

  relationship <- ggplot2::ggplot(cad, ggplot2::aes(TSXC_ret, USDCAD_ret)) +
    ggplot2::geom_point(alpha = 0.25, color = "#335C67") +
    ggplot2::geom_smooth(method = "lm", formula = if (polynomial) y ~ x + I(x^2) else y ~ x, color = "#D95D39") +
    ggplot2::labs(title = if (polynomial) "Quadratic FX-Equity Relationship" else "Linear FX-Equity Relationship") +
    standard_theme()
  residual_plot <- ggplot2::ggplot(fit_data, ggplot2::aes(fitted, residual)) +
    ggplot2::geom_point(alpha = 0.35, color = "#386641") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_smooth(se = FALSE, color = "#C2410C") +
    ggplot2::labs(title = "Residuals vs Fitted", x = "Fitted return", y = "Residual") +
    standard_theme()

  comparison <- data.frame(
    model = c("Linear baseline", if (polynomial) "Polynomial" else "Selected linear"),
    AIC = c(stats::AIC(baseline), stats::AIC(fit)),
    BIC = c(stats::BIC(baseline), stats::BIC(fit)),
    adjusted_R2 = c(summary(baseline)$adj.r.squared, summary(fit)$adj.r.squared)
  )
  f_test <- summary(fit)$fstatistic
  # pf(..., lower.tail = FALSE) 将整体 F 统计量转换为模型整体显著性的 p-value。
  model_p <- stats::pf(f_test[1], f_test[2], f_test[3], lower.tail = FALSE)

  new_case(
    if (polynomial) "Polynomial Regression: curved market relationships" else "Linear Regression: explaining USDCAD returns",
    "The case explains daily USDCAD returns using Canadian equity returns and interest-rate shocks.",
    if (polynomial) "Does a curved equity-FX relationship improve the model?" else "Do equity and rate shocks explain daily FX movements?",
    "Read coefficients, model fit, residual diagnostics, and model-comparison metrics as one analysis.",
    describe_variables(),
    base_steps(if (polynomial) "polynomial regression" else "linear regression", "Residuals should show no clear remaining pattern."),
    list("Observed relationship" = relationship, "Residual diagnostics" = residual_plot),
    list("Coefficient estimates" = broom_like_coefficients(fit), "Model comparison" = comparison),
    bind_tests(test_result("Overall regression F-test", "All slope coefficients are zero.", f_test[1], model_p)),
    capture.output(summary(fit)),
    paste("fit <- lm(", deparse(formula), ", data = cad_data)", sep = ""),
    "A useful regression requires both meaningful coefficients and acceptable residual behavior; model fit alone is not enough."
  )
}

# case_subset_regression()
# 功能：枚举候选变量组合，以 AIC/BIC/调整后 R² 选择较简洁的回归模型。
case_subset_regression <- function(data_bundle) {
  cad <- as.data.frame(prepare_cad_market_data(data_bundle))
  predictors <- c("TSXC_ret", "delta10y", "CAD_ON", "X10Y")
  # combn() 生成所有不同大小的变量组合；unlist(..., recursive = FALSE) 保留每个组合为独立向量。
  combinations <- unlist(lapply(seq_along(predictors), function(size) combn(predictors, size, simplify = FALSE)), recursive = FALSE)
  # 对每个候选组合拟合一次回归，并保存用于比较复杂度和拟合效果的指标。
  rows <- lapply(combinations, function(items) {
    fit <- stats::lm(stats::reformulate(items, "USDCAD_ret"), data = cad)
    data.frame(model = paste(items, collapse = " + "), variables = length(items), AIC = AIC(fit), BIC = BIC(fit), adjusted_R2 = summary(fit)$adj.r.squared)
  })
  comparison <- do.call(rbind, rows)
  comparison <- comparison[order(comparison$BIC), ]
  # strsplit() 把最佳模型文字拆回变量名，再由 reformulate() 重新生成回归公式。
  best_fit <- stats::lm(stats::reformulate(strsplit(comparison$model[1], " \\+ ")[[1]], "USDCAD_ret"), data = cad)

  metric_plot <- ggplot2::ggplot(comparison, ggplot2::aes(BIC, adjusted_R2, color = factor(variables), label = model)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(data = head(comparison, 3), hjust = 0, nudge_x = 2, size = 3) +
    ggplot2::labs(title = "Subset Model Trade-off", color = "Variables") +
    standard_theme()
  coefficient_plot <- ggplot2::ggplot(broom_like_coefficients(best_fit)[-1, ], ggplot2::aes(estimate, reorder(term, estimate))) +
    ggplot2::geom_col(fill = "#335C67") +
    ggplot2::labs(title = "Best-BIC Model Coefficients", x = "Estimate", y = NULL) +
    standard_theme()

  new_case(
    "Subset Regression: balancing fit and simplicity",
    "The original script used exhaustive subset selection. This case compares every combination of four CAD predictors.",
    "Which smaller predictor set explains USDCAD returns without unnecessary complexity?",
    "Use BIC, AIC, and adjusted R-squared together instead of selecting the largest model automatically.",
    describe_variables(),
    base_steps("subset regression", "Check whether the selected model remains interpretable and stable."),
    list("Model trade-off" = metric_plot, "Selected coefficients" = coefficient_plot),
    list("All candidate models" = comparison, "Best model coefficients" = broom_like_coefficients(best_fit)),
    bind_tests(),
    capture.output(summary(best_fit)),
    "fit every predictor subset; order candidate models by BIC",
    "The best-BIC model is the most economical candidate under a stronger complexity penalty; it is a starting point, not proof of a final causal model."
  )
}

# group_case_data()
# 功能：为 ANOVA、ANCOVA 和 MANOVA 返回同一份宏观分组数据。
group_case_data <- function(data_bundle) {
  # 为了让网页保持响应速度，最多取 3000 行参与分组模型；原数据库对象不会被修改。
  data <- prepare_macro_group_data(data_bundle)
  as.data.frame(data[seq_len(min(nrow(data), 3000))])
}

# case_anova()
# 功能：检验不同 NFP 分组的美国 10 年期利率变化均值是否相同。
case_anova <- function(data_bundle) {
  data <- group_case_data(data_bundle)
  # aov() 先做整体均值比较；TukeyHSD() 再检查具体哪些 NFP 组之间存在差异。
  fit <- stats::aov(change_10y ~ NFP_cat, data = data)
  anova_table <- as.data.frame(summary(fit)[[1]])
  tukey <- as.data.frame(stats::TukeyHSD(fit)$NFP_cat)
  # TukeyHSD() 默认把组名放在 rownames；这里转成普通列，方便网页表格和绘图。
  tukey$comparison <- rownames(tukey)

  distribution <- ggplot2::ggplot(data, ggplot2::aes(NFP_cat, change_10y, fill = NFP_cat)) +
    ggplot2::geom_violin(alpha = 0.65) +
    ggplot2::geom_boxplot(width = 0.15, outlier.shape = NA) +
    ggplot2::labs(title = "10Y Yield Changes Across NFP Regimes", x = "NFP quartile", y = "Change in US 10Y") +
    standard_theme()
  tukey_plot <- ggplot2::ggplot(tukey, ggplot2::aes(diff, reorder(comparison, diff))) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = lwr, xmax = upr), width = 0.2) +
    ggplot2::geom_point(size = 3, color = "#335C67") +
    ggplot2::labs(title = "Tukey Pairwise Differences", x = "Mean difference", y = NULL) +
    standard_theme()
  f_value <- anova_table$`F value`[1]
  p_value <- anova_table$`Pr(>F)`[1]

  new_case(
    "ANOVA: yield changes across NFP regimes",
    "NFP observations are divided into quartiles to test whether rate-change means differ across labor-market regimes.",
    "Are average US 10-year yield changes equal across NFP groups?",
    "Move from the overall F-test to pairwise Tukey comparisons and distribution plots.",
    variable_rows(NFP = "US nonfarm payroll monthly net change.", NFP_cat = "NFP quartile group.", change_10y = "Daily change in the US OIS 10-year rate."),
    base_steps("one-way ANOVA", "Inspect group distributions and residual assumptions before trusting the F-test."),
    list("Group distributions" = distribution, "Tukey comparisons" = tukey_plot),
    list("ANOVA table" = anova_table, "Tukey HSD" = tukey),
    bind_tests(test_result("ANOVA F-test", "All NFP groups have the same mean yield change.", f_value, p_value)),
    capture.output(summary(fit)),
    "aov(change_10y ~ NFP_cat, data = macro_data); TukeyHSD(fit)",
    "The F-test answers whether any group differs; Tukey intervals identify which pairwise differences are responsible."
  )
}

# case_ancova()
# 功能：控制 Risk 后比较不同 NFP 分组的美国 10 年期利率水平。
case_ancova <- function(data_bundle) {
  data <- group_case_data(data_bundle)
  # fit 假设各组 Risk 斜率相同；interaction_fit 允许每个 NFP 组拥有不同斜率。
  fit <- stats::lm(US10Y ~ Risk + NFP_cat, data = data)
  interaction_fit <- stats::lm(US10Y ~ Risk * NFP_cat, data = data)
  anova_table <- as.data.frame(stats::anova(fit))
  interaction_table <- as.data.frame(stats::anova(fit, interaction_fit))

  slope_plot <- ggplot2::ggplot(data, ggplot2::aes(Risk, US10Y, color = NFP_cat)) +
    ggplot2::geom_point(alpha = 0.15) +
    ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggplot2::labs(title = "Parallel-Slope Assumption", y = "US OIS 10Y") +
    standard_theme()
  residual_data <- data.frame(fitted = fitted(fit), residual = residuals(fit))
  residual_plot <- ggplot2::ggplot(residual_data, ggplot2::aes(fitted, residual)) +
    ggplot2::geom_point(alpha = 0.3, color = "#335C67") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = "ANCOVA Residuals", x = "Fitted yield", y = "Residual") +
    standard_theme()
  interaction_p <- interaction_table$`Pr(>F)`[2]
  # interaction_p 很小时，说明“平行斜率”假设不理想，需要谨慎解释 ANCOVA 调整均值。

  new_case(
    "ANCOVA: NFP regimes after controlling for risk",
    "ANCOVA compares NFP groups while holding the continuous Risk score constant.",
    "Do NFP regimes still relate to 10-year yields after controlling for market risk?",
    "Understand adjusted group comparisons and the important parallel-slope assumption.",
    variable_rows(NFP_cat = "NFP quartile group.", Risk = "Bloomberg US economic risk score.", US10Y = "US OIS 10-year rate."),
    base_steps("ANCOVA", "Check whether Risk has approximately parallel relationships with yield across NFP groups."),
    list("Slope assumption" = slope_plot, "Residual diagnostics" = residual_plot),
    list("ANCOVA table" = anova_table, "Interaction comparison" = interaction_table, "Coefficients" = broom_like_coefficients(fit)),
    bind_tests(test_result("Parallel slopes interaction test", "Risk slopes are equal across NFP groups.", interaction_table$F[2], interaction_p)),
    capture.output(summary(fit)),
    "lm(US10Y ~ Risk + NFP_cat, data = macro_data); lm(US10Y ~ Risk * NFP_cat, ...)",
    "ANCOVA adjusts group comparisons for Risk. A significant interaction warns that one common adjusted group effect may be too simple."
  )
}

# case_manova()
# 功能：同时比较各 NFP 组在 US10Y 和 Risk 两个结果变量上的联合差异。
case_manova <- function(data_bundle) {
  data <- group_case_data(data_bundle)
  # cbind(US10Y, Risk) 表示把两个结果变量作为一个联合结果，一次比较各 NFP 组。
  fit <- stats::manova(cbind(US10Y, Risk) ~ NFP_cat, data = data)
  wilks <- summary(fit, test = "Wilks")$stats
  # mahalanobis() 衡量每个观测点离二维总体中心有多远，用于识别多变量异常值。
  distances <- stats::mahalanobis(data[, c("US10Y", "Risk")], colMeans(data[, c("US10Y", "Risk")]), stats::cov(data[, c("US10Y", "Risk")]))
  # qchisq(ppoints(...)) 生成理论距离，之后与真实 Mahalanobis 距离画 QQ 图。
  qq_data <- data.frame(theoretical = stats::qchisq(stats::ppoints(length(distances)), df = 2), observed = sort(distances))

  joint_plot <- ggplot2::ggplot(data, ggplot2::aes(Risk, US10Y, color = NFP_cat)) +
    ggplot2::geom_point(alpha = 0.25) +
    ggplot2::stat_ellipse(type = "norm") +
    ggplot2::labs(title = "Joint Risk-Yield Distribution by NFP Regime") +
    standard_theme()
  qq_plot <- ggplot2::ggplot(qq_data, ggplot2::aes(theoretical, observed)) +
    ggplot2::geom_point(alpha = 0.45, color = "#335C67") +
    ggplot2::geom_abline(linetype = "dashed", color = "#C2410C") +
    ggplot2::labs(title = "Mahalanobis Distance QQ Plot", x = "Theoretical chi-square", y = "Observed distance") +
    standard_theme()

  new_case(
    "MANOVA: joint macro group differences",
    "MANOVA tests NFP-regime differences in the joint Risk and US10Y outcome space rather than testing each outcome separately.",
    "Do NFP regimes have different multivariate centers for yield and risk?",
    "Interpret a joint Wilks test alongside group geometry and multivariate outlier diagnostics.",
    variable_rows(NFP_cat = "NFP quartile group.", Risk = "Bloomberg US economic risk score.", US10Y = "US OIS 10-year rate."),
    base_steps("MANOVA", "Inspect multivariate outliers and group covariance patterns."),
    list("Joint distribution" = joint_plot, "Multivariate outliers" = qq_plot),
    list("Wilks MANOVA table" = as.data.frame(wilks), "Univariate follow-ups" = as.data.frame(summary.aov(fit)[[1]])),
    bind_tests(test_result("Wilks MANOVA", "NFP groups share the same multivariate center.", wilks[1, "approx F"], wilks[1, "Pr(>F)"])),
    capture.output(summary(fit, test = "Wilks")),
    "manova(cbind(US10Y, Risk) ~ NFP_cat, data = macro_data)",
    "The joint test asks whether groups differ somewhere in the combined outcome space; follow-up models explain which outcome contributes most."
  )
}

# case_poisson()
# 功能：复现原脚本的癫痫发作计数 Poisson GLM，并检查过度离散与残差。
case_poisson <- function() {
  # MASS::epil 是 R package 自带的癫痫发作次数数据，适合展示计数型结果。
  data <- MASS::epil
  # family = poisson() 告诉 glm()：目标是事件次数，使用 Poisson 分布和 log link。
  fit <- stats::glm(y ~ trt + base + age + V4, data = data, family = stats::poisson())
  data$predicted <- fitted(fit)
  data$pearson <- residuals(fit, type = "pearson")
  dispersion <- sum(data$pearson^2) / fit$df.residual
  # dispersion 明显大于 1 表示真实波动大于 Poisson 假设，可能需要负二项模型。

  predicted_plot <- ggplot2::ggplot(data, ggplot2::aes(predicted, y, color = trt)) +
    ggplot2::geom_point(alpha = 0.45) +
    ggplot2::geom_abline(linetype = "dashed") +
    ggplot2::labs(title = "Observed vs Predicted Counts", x = "Predicted seizure count", y = "Observed count") +
    standard_theme()
  residual_plot <- ggplot2::ggplot(data, ggplot2::aes(predicted, pearson)) +
    ggplot2::geom_point(alpha = 0.45, color = "#335C67") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = "Pearson Residuals", x = "Predicted count", y = "Pearson residual") +
    standard_theme()

  new_case(
    "Poisson GLM: modeling event counts",
    "The epilepsy case models repeated seizure counts using treatment and patient characteristics.",
    "How do treatment and baseline conditions relate to the expected event count?",
    "Interpret coefficients as multiplicative count effects and diagnose overdispersion.",
    variable_rows(y = "Number of seizures during a follow-up period.", trt = "Treatment group.", base = "Baseline seizure count.", age = "Patient age.", V4 = "Indicator for the fourth visit."),
    base_steps("Poisson GLM", "Check residual patterns and whether variance is much larger than the Poisson mean."),
    list("Observed vs predicted" = predicted_plot, "Residual diagnostics" = residual_plot),
    list("Coefficient estimates" = broom_like_coefficients(fit), "Exponentiated effects" = data.frame(term = names(coef(fit)), rate_ratio = exp(coef(fit)))),
    bind_tests(test_result("Overdispersion diagnostic", "Dispersion is approximately one.", dispersion, NA, paste("Pearson dispersion =", round(dispersion, 3), "; values well above 1 suggest overdispersion."))),
    capture.output(summary(fit)),
    "glm(y ~ trt + base + age + V4, family = poisson(), data = MASS::epil)",
    "Exponentiated coefficients are rate ratios. Large dispersion means a negative-binomial model may be more realistic than Poisson."
  )
}

# logistic_components()
# 功能：一次准备 Logistic、Confusion Matrix 和 ROC 共用的模型结果。
logistic_components <- function(data_bundle) {
  cad <- as.data.frame(prepare_cad_market_data(data_bundle))
  # 用下一行 USDCAD_ret 的正负创建预测目标；最后一行没有下一期，因此会产生 NA。
  cad$direction_up <- as.integer(c(cad$USDCAD_ret[-1] > 0, NA))
  data <- na.omit(cad[, c("direction_up", "USDCAD_ret", "TSXC_ret", "delta10y", "CAD_ON")])
  fit <- stats::glm(direction_up ~ USDCAD_ret + TSXC_ret + delta10y + CAD_ON, family = stats::binomial(), data = data)
  # type = "response" 返回 0 到 1 的上涨概率，再使用 0.5 转成具体分类。
  data$probability <- predict(fit, type = "response")
  data$predicted <- as.integer(data$probability >= 0.5)
  confusion <- table(Predicted = data$predicted, Actual = data$direction_up)
  roc <- manual_roc(data$direction_up, data$probability)
  # 返回共享对象，避免三个分类页面分别重复写一套准备逻辑。
  list(data = data, fit = fit, confusion = confusion, roc = roc)
}

# case_logistic()
# 功能：根据 view 分别突出 Logistic 模型、混淆矩阵或 ROC，但保留完整分析框架。
case_logistic <- function(data_bundle, view = "model") {
  # view 只改变页面重点；模型、混淆矩阵和 ROC 结果仍会一起返回，形成完整分析链路。
  result <- logistic_components(data_bundle)
  data <- result$data
  confusion_data <- as.data.frame(result$confusion)
  names(confusion_data) <- c("Predicted", "Actual", "Count")

  probability_plot <- ggplot2::ggplot(data, ggplot2::aes(probability, fill = factor(direction_up))) +
    ggplot2::geom_density(alpha = 0.4) +
    ggplot2::labs(title = "Predicted Probability by Actual Direction", fill = "Actual") +
    standard_theme()
  confusion_plot <- ggplot2::ggplot(confusion_data, ggplot2::aes(Actual, Predicted, fill = Count)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = Count), size = 5) +
    ggplot2::labs(title = "Confusion Matrix") +
    standard_theme()
  roc_plot <- ggplot2::ggplot(result$roc$data, ggplot2::aes(FPR, TPR)) +
    ggplot2::geom_line(linewidth = 1.2, color = "#335C67") +
    ggplot2::geom_abline(linetype = "dashed") +
    ggplot2::coord_equal() +
    ggplot2::labs(title = paste0("ROC Curve; AUC = ", round(result$roc$auc, 3))) +
    standard_theme()
  likelihood <- stats::anova(result$fit, test = "Chisq")
  # 最后一行是完整模型相对空模型带来的改进，其 p-value 用于整体模型检验。
  likelihood_p <- tail(likelihood$`Pr(>Chi)`, 1)

  titles <- c(model = "Logistic Regression: next-day FX direction", confusion = "Confusion Matrix: classification errors", roc = "ROC: performance across thresholds")
  questions <- c(model = "Which CAD market variables change the probability of an upward FX move?", confusion = "At a 0.5 threshold, which errors does the classifier make?", roc = "How well does the model rank positive days across every threshold?")
  new_case(
    titles[[view]],
    "The model converts CAD market features into a probability that the next USDCAD return is positive.",
    questions[[view]],
    "Connect probability modeling, a chosen decision threshold, and threshold-free ranking performance.",
    describe_variables(),
    base_steps("binary classification", "Compare probability separation, threshold errors, and ROC ranking ability."),
    list("Probability separation" = probability_plot, "Confusion matrix" = confusion_plot, "ROC curve" = roc_plot),
    list("Coefficient estimates" = broom_like_coefficients(result$fit), "Confusion matrix" = as.data.frame.matrix(result$confusion), "ROC coordinates" = head(result$roc$data, 30)),
    bind_tests(test_result("Likelihood-ratio model test", "Predictors do not improve the intercept-only model.", tail(likelihood$Deviance, 1), likelihood_p)),
    capture.output(summary(result$fit)),
    "glm(direction_up ~ USDCAD_ret + TSXC_ret + delta10y + CAD_ON, family = binomial())",
    paste("At the selected threshold, inspect the confusion matrix; across all thresholds, the model's AUC is", round(result$roc$auc, 3), ".")
  )
}

# time_series_components()
# 功能：准备 ARIMA、SARIMA 和 GARCH 共用的加拿大 10 年期利率变化序列。
time_series_components <- function(data_bundle, seasonal = FALSE) {
  # tail(..., 1800) 只保留最近 1800 个观测，兼顾代表性和网页运行速度。
  series <- tail(prepare_cad_market_data(data_bundle)$delta10y, 1800)
  order <- if (seasonal) c(2, 0, 1) else c(1, 0, 1)
  # seasonal = TRUE 时加入 5 个交易日周期；否则拟合普通 ARIMA(1,0,1)。
  fit <- if (seasonal) {
    stats::arima(series, order = order, seasonal = list(order = c(1, 0, 0), period = 5))
  } else {
    stats::arima(series, order = order)
  }
  residual <- as.numeric(residuals(fit))
  # 实际值减去残差可以还原样本内拟合值；predict() 计算未来 40 步预测和标准误。
  fitted <- as.numeric(series) - residual
  forecast <- stats::predict(fit, n.ahead = 40)
  list(series = as.numeric(series), fit = fit, residual = residual, fitted = fitted, forecast = forecast)
}

# case_arima()
# 功能：展示时间序列、ACF/PACF、拟合、残差白噪声和预测。
case_arima <- function(data_bundle, seasonal = FALSE) {
  result <- time_series_components(data_bundle, seasonal)
  # 分别整理历史拟合和未来预测数据，避免在 ggplot2 中直接操作复杂模型对象。
  fit_data <- data.frame(index = seq_along(result$series), actual = result$series, fitted = result$fitted)
  forecast_data <- data.frame(
    horizon = seq_along(result$forecast$pred),
    mean = as.numeric(result$forecast$pred),
    lower = as.numeric(result$forecast$pred - 1.96 * result$forecast$se),
    upper = as.numeric(result$forecast$pred + 1.96 * result$forecast$se)
  )
  fit_plot <- ggplot2::ggplot(fit_data, ggplot2::aes(index)) +
    ggplot2::geom_line(ggplot2::aes(y = actual, color = "Actual"), alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = fitted, color = "Fitted")) +
    ggplot2::labs(title = "Observed Series and In-Sample Fit", y = "delta10y", color = NULL) +
    standard_theme()
  forecast_plot <- ggplot2::ggplot(forecast_data, ggplot2::aes(horizon, mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.25, fill = "#335C67") +
    ggplot2::geom_line(color = "#C2410C") +
    ggplot2::labs(title = "40-Step Forecast with 95% Interval", y = "Forecast delta10y") +
    standard_theme()
  box <- stats::Box.test(result$residual, lag = 20, type = "Ljung-Box")
  # Ljung-Box 的 H0 是多个残差自相关同时为零；p-value 大通常更符合白噪声要求。

  new_case(
    if (seasonal) "SARIMA: short-cycle rate dynamics" else "ARIMA: univariate rate-change dynamics",
    "The case models daily changes in the Canada 10-year yield using its own history.",
    if (seasonal) "Does a five-business-day seasonal component improve short-cycle modeling?" else "Can recent rate changes explain and forecast the next movements?",
    "Use ACF/PACF to understand lag structure, then verify that model residuals resemble white noise.",
    variable_rows(delta10y = "Daily change in the Canada 10-year yield.", residual = "Observed value minus the model's fitted value."),
    base_steps(if (seasonal) "SARIMA" else "ARIMA", "A good time-series model leaves little autocorrelation in residuals."),
    list("Series and fit" = fit_plot, "Series ACF" = acf_plot(result$series, "ACF of Rate Changes"), "Residual PACF" = acf_plot(result$residual, "PACF of Residuals", "partial"), "Forecast" = forecast_plot),
    list("Model coefficients" = data.frame(term = names(result$fit$coef), estimate = as.numeric(result$fit$coef)), "Forecast values" = forecast_data),
    bind_tests(test_result("Ljung-Box residual test", "Residual autocorrelations are jointly zero.", box$statistic, box$p.value)),
    capture.output(result$fit),
    if (seasonal) "arima(series, order = c(2,0,1), seasonal = list(order = c(1,0,0), period = 5))" else "arima(series, order = c(1,0,1))",
    "Forecast intervals widen because future uncertainty accumulates. Residual autocorrelation indicates which time structure remains unexplained."
  )
}

# case_garch()
# 功能：复现原脚本的 ARIMA 均值模型到 GARCH 波动模型分析链路。
case_garch <- function(data_bundle) {
  # 先由 ARIMA 解释条件均值，再把剩余残差交给 GARCH 解释随时间变化的波动。
  arima_result <- time_series_components(data_bundle, FALSE)
  garch <- fit_garch_manual(arima_result$residual)
  plot_data <- data.frame(index = seq_along(garch$sigma), residual = arima_result$residual, volatility = garch$sigma)
  volatility_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(index)) +
    ggplot2::geom_line(ggplot2::aes(y = abs(residual), color = "Absolute ARIMA residual"), alpha = 0.55) +
    ggplot2::geom_line(ggplot2::aes(y = volatility, color = "GARCH conditional volatility"), linewidth = 0.8) +
    ggplot2::labs(title = "Shocks and Estimated Conditional Volatility", y = NULL, color = NULL) +
    standard_theme()
  squared_before <- acf_plot(arima_result$residual^2, "ACF of Squared ARIMA Residuals")
  squared_after <- acf_plot(garch$standardized_residuals^2, "ACF After GARCH Standardization")
  before_test <- stats::Box.test(arima_result$residual^2, lag = 20, type = "Ljung-Box")
  after_test <- stats::Box.test(garch$standardized_residuals^2, lag = 20, type = "Ljung-Box")
  # 对平方残差做检验，是因为波动聚集主要表现在“冲击大小”而不是冲击正负方向。

  new_case(
    "ARCH / GARCH: modeling changing volatility",
    "ARIMA models the conditional mean; GARCH models clusters of large and small shocks remaining in the residuals.",
    "Does the uncertainty of rate changes vary over time, and can GARCH explain that clustering?",
    "Diagnose ARCH effects, estimate GARCH(1,1), and verify standardized residuals.",
    variable_rows(residual = "Shock left after the ARIMA mean model.", volatility = "GARCH estimate of time-varying standard deviation.", alpha1 = "Immediate shock effect.", beta1 = "Persistence of past volatility."),
    base_steps("GARCH(1,1)", "Squared standardized residuals should have less remaining autocorrelation."),
    list("Conditional volatility" = volatility_plot, "Squared residual ACF before GARCH" = squared_before, "Squared residual ACF after GARCH" = squared_after),
    list("GARCH coefficients" = data.frame(term = names(garch$coefficients), estimate = as.numeric(garch$coefficients)), "Recent volatility" = tail(plot_data, 30)),
    bind_tests(
      test_result("ARCH-effect proxy before GARCH", "Squared residuals have no autocorrelation.", before_test$statistic, before_test$p.value),
      test_result("Remaining ARCH effect after GARCH", "Squared standardized residuals have no autocorrelation.", after_test$statistic, after_test$p.value)
    ),
    paste(capture.output(garch$coefficients), collapse = "\n"),
    "fit ARIMA mean; estimate GARCH(1,1) variance by maximum likelihood; test squared standardized residuals",
    "A high alpha reacts quickly to shocks; a high beta means volatility fades slowly. The after-GARCH test shows whether clustering remains."
  )
}

# case_var()
# 功能：展示多变量动态系统，并在 Granger 页面突出信息贡献关系。
case_var <- function(data_bundle, view = "var") {
  # prepare_var_data() 生成平稳化后的 delta10y 与三个市场因子，再拟合两阶 VAR。
  data <- prepare_var_data(data_bundle)
  fit <- fit_simple_var(data, lag = 2)
  # granger_table() 比较完整和删减后的 VAR 方程，检查哪些因子增加预测信息。
  granger <- granger_table(fit)
  coefficient_table <- do.call(rbind, lapply(names(fit$equations), function(response) {
    output <- broom_like_coefficients(fit$equations[[response]])
    output$response <- response
    output
  }))
  fitted_data <- data.frame(actual = fit$response[, "delta10y"], fitted = fit$fitted[, "delta10y"])
  fit_plot <- ggplot2::ggplot(fitted_data, ggplot2::aes(actual, fitted)) +
    ggplot2::geom_point(alpha = 0.35, color = "#335C67") +
    ggplot2::geom_abline(linetype = "dashed", color = "#C2410C") +
    ggplot2::labs(title = "VAR Fitted vs Actual: delta10y") +
    standard_theme()
  granger_plot <- ggplot2::ggplot(granger, ggplot2::aes(strength, reorder(cause, strength), fill = p_value < 0.05)) +
    ggplot2::geom_col() +
    ggplot2::geom_vline(xintercept = -log10(0.05), linetype = "dashed") +
    ggplot2::labs(title = "Granger Information Contribution", x = "-log10(p-value)", y = NULL, fill = "p < 0.05") +
    standard_theme()
  residual_correlation <- stats::cor(fit$residuals)
  # 残差仍然高度相关时，说明系统中可能还有未建模的共同冲击。
  titles <- c(var = "VAR: a dynamic multi-variable system", granger = "Granger Causality: predictive information flow")

  new_case(
    titles[[view]],
    "The case combines delta10y with three PCA-derived market factors so every equation can depend on the recent history of the full system.",
    if (view == "var") "How do rate changes and market factors evolve together over time?" else "Which market factors add predictive information for future rate changes?",
    "Read a VAR as a system of linked regressions, then use nested-model tests for Granger causality.",
    variable_rows(delta10y = "Daily change in Canada 10-year yield.", ML1 = "First market component.", ML2 = "Second market component.", ML3 = "Third market component."),
    base_steps(if (view == "var") "VAR" else "Granger causality", "Check residual relationships and avoid interpreting predictive causality as structural causality."),
    list("VAR fitted vs actual" = fit_plot, "Granger strength" = granger_plot, "Residual correlation" = matrix_heatmap(residual_correlation, "VAR Residual Correlation")),
    list("VAR coefficients" = coefficient_table, "Granger tests" = granger),
    bind_tests(test_result("Granger joint test: strongest factor", paste0(granger$cause[which.min(granger$p_value)], " lags do not improve delta10y prediction."), max(granger$F_statistic, na.rm = TRUE), min(granger$p_value, na.rm = TRUE))),
    paste(capture.output(summary(fit$equations$delta10y)), collapse = "\n"),
    "fit each VAR equation with two lags of every system variable; compare reduced and full equations",
    "VAR describes joint dynamics. Granger significance means past values improve prediction after other lags are included; it is not proof of economic causation."
  )
}

# case_efa()
# 功能：使用 factanal() 提取共同因子，并展示载荷和因子得分。
case_efa <- function(data_bundle) {
  data <- prepare_factor_data(data_bundle)
  # factanal() 假设观测变量由少数共同因子和各自特有误差共同组成。
  # scores = "regression" 同时估计每条观测对应的因子得分，供散点图使用。
  fit <- stats::factanal(data, factors = 2, scores = "regression")
  loadings <- unclass(fit$loadings)
  # unclass() 把专用 loadings 对象转为普通矩阵，便于网页表格和热力图读取。
  scores <- as.data.frame(fit$scores)
  score_plot <- ggplot2::ggplot(scores, ggplot2::aes(Factor1, Factor2)) +
    ggplot2::geom_point(alpha = 0.3, color = "#386641") +
    ggplot2::labs(title = "EFA Factor Scores") +
    standard_theme()
  loadings_plot <- matrix_heatmap(loadings, "EFA Factor Loadings")

  new_case(
    "EFA: discovering latent market drivers",
    "Exploratory Factor Analysis asks whether observed CAD variables are driven by a smaller set of unobserved common factors.",
    "Can a few latent factors explain the shared variation among CAD market variables?",
    "Interpret factor loadings, uniqueness, and factor scores without treating factor names as automatic truths.",
    variable_rows(USDCAD_ret = "FX return.", TSXC_ret = "Equity return.", delta10y = "10-year yield change.", CAD_ON = "Overnight rate.", X10Y = "10-year yield level."),
    base_steps("exploratory factor analysis", "Check whether variables share enough correlation and whether loadings form interpretable groups."),
    list("Loading heatmap" = loadings_plot, "Factor scores" = score_plot, "Input correlation" = matrix_heatmap(cor(data), "Input Correlation for EFA")),
    list("Factor loadings" = round(loadings, 4), "Variable uniqueness" = data.frame(variable = names(fit$uniquenesses), uniqueness = fit$uniquenesses)),
    bind_tests(test_result("Factor model adequacy", "Two factors adequately reproduce the correlation structure.", fit$STATISTIC, fit$PVAL)),
    capture.output(fit),
    "factanal(market_variables, factors = 2, scores = 'regression')",
    "Large absolute loadings show which variables define each latent factor; uniqueness shows variation left specific to each variable."
  )
}

# case_pca()
# 功能：展示 PCA 的解释方差、载荷和样本得分。
case_pca <- function(data_bundle) {
  data <- prepare_factor_data(data_bundle)
  # scale. = TRUE 先把不同单位的变量标准化，避免利率水平仅因数值范围大而主导 PCA。
  fit <- stats::prcomp(data, scale. = TRUE)
  # 每个主成分的方差等于 sdev²；除以总和后得到解释方差比例。
  variance <- fit$sdev^2 / sum(fit$sdev^2)
  variance_data <- data.frame(component = seq_along(variance), variance = variance, cumulative = cumsum(variance))
  scores <- as.data.frame(fit$x[, 1:2])
  score_plot <- ggplot2::ggplot(scores, ggplot2::aes(PC1, PC2)) +
    ggplot2::geom_point(alpha = 0.3, color = "#335C67") +
    ggplot2::labs(title = "PCA Score Map") +
    standard_theme()
  scree <- ggplot2::ggplot(variance_data, ggplot2::aes(component, variance)) +
    ggplot2::geom_col(fill = "#335C67") +
    ggplot2::geom_line(ggplot2::aes(y = cumulative), color = "#C2410C") +
    ggplot2::geom_point(ggplot2::aes(y = cumulative), color = "#C2410C") +
    ggplot2::labs(title = "Explained and Cumulative Variance", y = "Share") +
    standard_theme()

  new_case(
    "PCA: compressing correlated market variables",
    "PCA creates orthogonal components that preserve as much total variation as possible.",
    "How many components summarize the CAD market feature set?",
    "Use explained variance and loadings to decide whether compression remains interpretable.",
    variable_rows(PC1 = "First component; direction with the most variance.", PC2 = "Second orthogonal component.", loading = "Weight connecting an original variable to a component."),
    base_steps("PCA", "Inspect explained variance and loadings before using components downstream."),
    list("Scree and cumulative variance" = scree, "PCA score map" = score_plot, "Loading heatmap" = matrix_heatmap(fit$rotation[, 1:3], "PCA Loadings")),
    list("Explained variance" = variance_data, "Loadings" = round(fit$rotation, 4)),
    bind_tests(),
    capture.output(summary(fit)),
    "prcomp(market_variables, center = TRUE, scale. = TRUE)",
    "PCA is strongest when a small number of components preserve much of the variation and their loadings can be explained economically."
  )
}

# case_rolling_pca()
# 功能：用滚动窗口重复 PCA，展示市场结构随时间变化。
case_rolling_pca <- function(data_bundle) {
  data <- prepare_factor_data(data_bundle)
  window <- 250
  # endpoints 每隔 50 个观测建立一次窗口；每个窗口都使用最近 250 条数据。
  endpoints <- seq(window, nrow(data), by = 50)
  rows <- lapply(endpoints, function(endpoint) {
    window_data <- data[(endpoint - window + 1):endpoint, , drop = FALSE]
    # 某些短窗口内 CAD_ON 不变化；PCA 前删除这些零方差列，避免标准化除以零。
    variable_columns <- vapply(window_data, stats::sd, numeric(1)) > 0
    fit <- stats::prcomp(window_data[, variable_columns, drop = FALSE], scale. = TRUE)
    variance <- fit$sdev^2 / sum(fit$sdev^2)
    usdcad_loading <- if ("USDCAD_ret" %in% rownames(fit$rotation)) fit$rotation["USDCAD_ret", 1] else NA_real_
    data.frame(index = endpoint, PC1_variance = variance[1], PC2_variance = variance[2], USDCAD_loading = usdcad_loading)
  })
  rolling <- do.call(rbind, rows)
  # do.call(rbind, rows) 把每个窗口产生的一行指标合并成完整时间序列表。
  variance_plot <- ggplot2::ggplot(rolling, ggplot2::aes(index)) +
    ggplot2::geom_line(ggplot2::aes(y = PC1_variance, color = "PC1")) +
    ggplot2::geom_line(ggplot2::aes(y = PC2_variance, color = "PC2")) +
    ggplot2::labs(title = "Rolling Explained Variance", y = "Variance share", color = NULL) +
    standard_theme()
  loading_plot <- ggplot2::ggplot(rolling, ggplot2::aes(index, USDCAD_loading)) +
    ggplot2::geom_line(color = "#C2410C") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = "Rolling USDCAD Loading on PC1", y = "Loading") +
    standard_theme()

  new_case(
    "Rolling PCA: detecting changing market structure",
    "A single full-sample PCA assumes relationships are stable. Rolling PCA repeats the calculation through time.",
    "Does the dominant CAD market component change across regimes?",
    "Track both the strength and composition of the first component over moving windows.",
    variable_rows(PC1_variance = "Variance share explained by PC1 in each rolling window.", USDCAD_loading = "USDCAD return weight in rolling PC1.", window = "250 observations used for each local PCA."),
    base_steps("rolling PCA", "Watch for changes in explained variance and loading signs across windows."),
    list("Rolling variance" = variance_plot, "Rolling loading" = loading_plot),
    list("Rolling PCA metrics" = rolling),
    bind_tests(),
    paste("Rolling windows:", nrow(rolling), "; window size:", window),
    "repeat prcomp() over 250-observation windows, advancing 50 observations each time",
    "Large shifts in PC1 variance or loadings indicate that a fixed component interpretation may no longer describe the current regime."
  )
}

# case_cluster()
# 功能：新增教学案例，在 PCA 得分上运行 k-means，展示市场状态聚类。
case_cluster <- function(data_bundle) {
  data <- prepare_factor_data(data_bundle)
  # 先将原变量压缩到 PC1/PC2，再在二维得分空间内寻找相似市场状态。
  pca <- stats::prcomp(data, scale. = TRUE)
  scores <- as.data.frame(pca$x[, 1:2])
  set.seed(123)
  # nstart = 20 从 20 组随机中心尝试，降低 k-means 落入较差局部结果的概率。
  fit <- stats::kmeans(scores, centers = 3, nstart = 20)
  scores$cluster <- factor(fit$cluster)
  cluster_plot <- ggplot2::ggplot(scores, ggplot2::aes(PC1, PC2, color = cluster)) +
    ggplot2::geom_point(alpha = 0.4) +
    ggplot2::labs(title = "K-Means Clusters in PCA Space") +
    standard_theme()
  size_plot <- ggplot2::ggplot(data.frame(cluster = factor(seq_along(fit$size)), observations = fit$size), ggplot2::aes(cluster, observations, fill = cluster)) +
    ggplot2::geom_col() +
    ggplot2::labs(title = "Cluster Sizes") +
    standard_theme()

  new_case(
    "Cluster-related Example: market-state grouping",
    "This teaching extension groups PCA scores into three recurring CAD market states. It is added for the encyclopedia and is not a direct copied block from the original script.",
    "Which observations look similar after the market variables are compressed into PCA space?",
    "Understand k-means as descriptive grouping, then profile rather than over-label the clusters.",
    variable_rows(PC1 = "First PCA score.", PC2 = "Second PCA score.", cluster = "Nearest k-means center among three groups."),
    base_steps("k-means clustering", "Compare cluster separation, size, and economic profiles; clusters are not automatically real regimes."),
    list("Clusters in PCA space" = cluster_plot, "Cluster sizes" = size_plot),
    list("Cluster centers" = fit$centers, "Cluster sizes" = data.frame(cluster = seq_along(fit$size), observations = fit$size)),
    bind_tests(),
    paste("Within-cluster sum of squares:", round(fit$tot.withinss, 3)),
    "pca <- prcomp(data, scale. = TRUE); kmeans(pca$x[, 1:2], centers = 3, nstart = 20)",
    "The clusters summarize similarity, not causality. Their usefulness depends on whether later profiling reveals stable and meaningful market states."
  )
}

# case_power()
# 功能：展示样本量、效应大小和统计功效之间的关系。
case_power <- function() {
  sample_sizes <- seq(20, 500, by = 10)
  effect_sizes <- c(0.1, 0.25, 0.4)
  # expand.grid() 建立“样本量 × 效应大小”的所有组合，之后逐一计算 power。
  grid <- expand.grid(n = sample_sizes, effect_size = effect_sizes)
  # mapply() 同时逐行传入 n 和 effect；非中心 F 分布表示真实效应存在时的统计量分布。
  grid$power <- mapply(function(n, effect) {
    critical <- stats::qf(0.95, df1 = 3, df2 = n - 4)
    1 - stats::pf(critical, df1 = 3, df2 = n - 4, ncp = effect^2 * n)
  }, grid$n, grid$effect_size)
  required <- do.call(rbind, lapply(effect_sizes, function(effect) {
    # 对每个效应大小，找出第一条达到 80% power 的样本量。
    subset <- grid[grid$effect_size == effect & grid$power >= 0.8, ]
    head(subset, 1)
  }))
  power_plot <- ggplot2::ggplot(grid, ggplot2::aes(n, power, color = factor(effect_size))) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed") +
    ggplot2::labs(title = "Power by Sample Size and Effect Size", color = "Effect size") +
    standard_theme()

  new_case(
    "Power Analysis: planning a study before modeling",
    "Power analysis links sample size, effect size, significance threshold, and the chance of detecting a real relationship.",
    "How many observations are needed to reach 80% power for different effect sizes?",
    "See why small effects require much larger samples and why power should be planned before analysis.",
    variable_rows(n = "Total sample size.", effect_size = "Standardized strength of the relationship.", power = "Probability of rejecting H0 when the assumed effect is real."),
    base_steps("power analysis", "Run sensitivity scenarios because the true effect size is uncertain."),
    list("Power curves" = power_plot),
    list("Required sample approximations" = required, "Power grid" = grid),
    bind_tests(),
    "Power calculated from the non-central F distribution for a regression with three predictors.",
    "calculate non-central F power across sample sizes and assumed effect sizes",
    "Planning for smaller realistic effects produces more conservative sample requirements than assuming a large effect."
  )
}

# case_bayesian()
# 功能：保留原脚本的场景后验概率和 USDCAD 期望收益分析，并增加结果评价。
case_bayesian <- function(data_bundle) {
  cad <- data.table::copy(prepare_cad_market_data(data_bundle))
  # copy() 建立独立数据副本，后续新增列不会改变其他案例共享的数据。
  cad[, next_return := data.table::shift(USDCAD_ret, type = "lead")]
  cad <- na.omit(cad)
  cad[, scenario := data.table::fifelse(TSXC_ret < -0.005 & delta10y <= 0, "RiskOff", data.table::fifelse(TSXC_ret > 0.005 & delta10y >= 0, "RiskOn", "Neutral"))]
  stats <- cad[, .(prior = .N / nrow(cad), tsx_mean = mean(TSXC_ret), tsx_sd = sd(TSXC_ret), rate_mean = mean(delta10y), rate_sd = sd(delta10y), return_mean = mean(next_return)), by = scenario]
  # stats 保存每个场景的先验概率、输入分布参数和未来收益均值。

  posterior_rows <- lapply(seq_len(nrow(cad)), function(index) {
    # dnorm() 计算当天股指和利率条件在每个场景下出现的可能程度，也就是 likelihood。
    likelihood <- stats::dnorm(cad$TSXC_ret[index], stats$tsx_mean, pmax(stats$tsx_sd, 1e-8)) *
      stats::dnorm(cad$delta10y[index], stats$rate_mean, pmax(stats$rate_sd, 1e-8))
    # posterior 与 likelihood × prior 成正比，再除以总和使三个场景概率加起来等于 1。
    posterior <- likelihood * stats$prior
    posterior <- posterior / sum(posterior)
    data.frame(index = index, scenario = stats$scenario, posterior = posterior, expected_component = posterior * stats$return_mean)
  })
  posterior <- data.table::as.data.table(do.call(rbind, posterior_rows))
  # 将每个日期的场景概率乘以场景平均收益，再相加得到当天的概率加权期望收益。
  expected <- posterior[, .(expected_return = sum(expected_component)), by = index]
  expected$actual_return <- cad$next_return[expected$index]
  expected$cumulative_strategy <- cumsum(sign(expected$expected_return) * expected$actual_return)

  probability_plot <- ggplot2::ggplot(posterior, ggplot2::aes(index, posterior, fill = scenario)) +
    ggplot2::geom_area() +
    ggplot2::labs(title = "Posterior Scenario Probabilities", y = "Probability") +
    standard_theme()
  scatter <- ggplot2::ggplot(expected, ggplot2::aes(expected_return, actual_return)) +
    ggplot2::geom_point(alpha = 0.25, color = "#335C67") +
    ggplot2::geom_smooth(method = "lm", color = "#C2410C") +
    ggplot2::labs(title = "Expected vs Realized Next Return") +
    standard_theme()
  cumulative <- ggplot2::ggplot(expected, ggplot2::aes(index, cumulative_strategy)) +
    ggplot2::geom_line(color = "#386641") +
    ggplot2::labs(title = "Illustrative Cumulative Signed Return") +
    standard_theme()
  correlation_test <- stats::cor.test(expected$expected_return, expected$actual_return)
  # 相关性检验检查期望收益是否至少与下一期真实收益方向保持统计关系。

  new_case(
    "Bayesian Scenario Analysis: market regime probabilities",
    "The case estimates RiskOn, RiskOff, and Neutral probabilities from equity and rate conditions, then converts them into expected USDCAD return.",
    "How should uncertain market scenarios be combined into one expected return?",
    "Move from priors and likelihoods to posterior probabilities, expected value, and outcome evaluation.",
    variable_rows(scenario = "RiskOn, RiskOff, or Neutral market state.", prior = "Historical frequency of a scenario.", likelihood = "How compatible today's inputs are with a scenario.", posterior = "Updated scenario probability.", expected_return = "Probability-weighted next USDCAD return."),
    base_steps("Bayesian scenario analysis", "Compare expected and realized returns; posterior confidence does not guarantee forecast accuracy."),
    list("Posterior probabilities" = probability_plot, "Expected vs realized" = scatter, "Illustrative cumulative result" = cumulative),
    list("Scenario parameters" = stats, "Recent expected returns" = tail(expected, 30)),
    bind_tests(test_result("Expected-realized correlation", "Expected and realized next returns have zero correlation.", correlation_test$statistic, correlation_test$p.value)),
    capture.output(correlation_test),
    "posterior = likelihood * prior / sum(likelihood * prior); expected_return = sum(posterior * scenario_mean)",
    "Bayesian probabilities organize uncertainty explicitly. Their practical value still depends on whether expected returns relate to later outcomes."
  )
}
