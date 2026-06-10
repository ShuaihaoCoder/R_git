# ============================================================
# Method catalog, notes, source mapping, and navigator network
# ============================================================
# 整体功能：
# 本文件不运行统计模型，而是描述网页有哪些方法、如何解释和如何导航。

# get_method_catalog()
# 功能：定义网页左侧两级目录和每个方法的唯一 ID。
# 参数：无。
# 返回：data.frame；category 是一级目录，method_id 是连接全项目的唯一键，
# method_name 是显示名称，example_id 是传给 run_example() 的案例 ID。
get_method_catalog <- function() {
  # 每行定义一个网页方法；相同 category 会在左侧选择器中组成同一组。
  data.frame(
    category = c(
      "Statistical Relationship", "Statistical Relationship", "Statistical Relationship",
      "Regression Models", "Regression Models", "Regression Models",
      "Group Comparison", "Group Comparison", "Group Comparison",
      "Generalized Models", "Generalized Models", "Generalized Models", "Generalized Models",
      "Time Series", "Time Series", "Time Series", "Time Series", "Time Series",
      "Dimension Reduction", "Dimension Reduction", "Dimension Reduction", "Dimension Reduction",
      "Decision & Probability", "Decision & Probability"
    ),
    method_id = c(
      "independence_test", "correlation", "partial_correlation",
      "linear_regression", "polynomial_regression", "subset_regression",
      "anova", "ancova", "manova",
      "poisson_glm", "logistic_regression", "confusion_matrix", "roc",
      "arima", "sarima", "garch", "var", "granger",
      "efa", "pca", "rolling_pca", "cluster",
      "power_analysis", "bayesian_scenario"
    ),
    method_name = c(
      "Independence Test", "Correlation", "Partial Correlation",
      "Linear Regression", "Polynomial Regression", "Subset Regression",
      "ANOVA", "ANCOVA", "MANOVA",
      "Poisson GLM", "Logistic Regression", "Confusion Matrix", "ROC",
      "ARIMA", "SARIMA", "ARCH / GARCH", "VAR", "Granger Causality",
      "EFA", "PCA", "Rolling PCA", "Cluster-related Examples",
      "Power Analysis", "Bayesian Scenario Analysis"
    ),
    example_id = c(
      "independence_test", "correlation", "partial_correlation",
      "linear_regression", "polynomial_regression", "subset_regression",
      "anova", "ancova", "manova",
      "poisson_glm", "logistic_regression", "confusion_matrix", "roc",
      "arima", "sarima", "garch", "var", "granger",
      "efa", "pca", "rolling_pca", "cluster",
      "power_analysis", "bayesian_scenario"
    ),
    stringsAsFactors = FALSE
  )
}

# get_source_method_map()
# 功能：把原 DataScience.R 中的具体代码段映射到 Shiny 方法页面。
# 参数：无。
# 返回：data.frame；记录源代码行号、原方法、目标 method_id 和映射理由。
get_source_method_map <- function() {
  # 中文说明：这里把原始 DataScience.R 的代码段映射到 Shiny 百科里的方法页面。
  # 目标是让网页能回答：原脚本里的每个 data science 方法，到底应该去哪个 tab 查。
  # 每行把一个原脚本代码段连接到一个现有 method_id。
  data.frame(
    source_lines = c(
      "64-147", "64-147", "64-147", "64-147", "64-147",
      "148-186", "148-186", "148-186", "148-186",
      "187-254", "198-302", "303-374",
      "375-628", "375-628", "375-628",
      "629-711", "712-796",
      "797-884", "797-884", "885-937", "938-970", "971-1055",
      "1056-1065",
      "1066-1113", "1114-1166", "1167-1198",
      "1199-1258", "1259-1380",
      "1381-1500", "1500-1703",
      "1704-1774", "1775-1901",
      "1902-1997", "1998-2044", "2045-2153",
      "2154-2158", "2159-2396"
    ),
    source_section = c(
      "Regression", "Regression", "Regression", "Regression", "Regression",
      "Independent Test", "Independent Test", "Independent Test", "Independent Test",
      "Correlation Analysis", "Partial Correlation", "Correlation Test Matrix",
      "ANOVA", "ANOVA diagnostics", "Post-hoc comparison",
      "ANCOVA", "MANOVA",
      "Generalized Linear Model", "Poisson diagnostics", "Logistic Regression", "Confusion Matrix", "ROC",
      "Power Analysis",
      "Time Series", "SARIMA", "auto.arima",
      "ARCH/GARCH", "ARIMA + GARCH integration",
      "EFA", "Factor-score modeling",
      "VAR", "Granger matrix / impulse workflow",
      "PCA", "Rolling PCA", "PCA projection / biplot",
      "Granger Causality", "Bayesian Naive / Gaussian Scenario"
    ),
    source_method = c(
      "Simple linear regression: lm(USDCAD ~ TSXC)",
      "Polynomial regression: lm(... + I(x^2))",
      "Return regression: lm(USDCAD_ret ~ TSXC_ret)",
      "Interaction regression: lm(... + X * Z)",
      "Model selection: stepAIC() and regsubsets()",
      "Chi-square independence test: chisq.test()",
      "Fisher exact test: fisher.test()",
      "Cochran-Mantel-Haenszel test: mantelhaen.test()",
      "Mosaic visualization for contingency tables",
      "Correlation matrix / corrplot / ggcorrplot / qgraph",
      "Partial correlation matrix and network graph",
      "psych::corr.test() p-value matrix",
      "One-way ANOVA: aov() / anova()",
      "ANOVA assumption checks and group visualization",
      "Tukey HSD pairwise comparison",
      "ANCOVA with covariate controls",
      "MANOVA with Mahalanobis distance and Box's M",
      "Poisson GLM: glm(..., family = poisson)",
      "Poisson residual diagnostics and overdispersion",
      "Binary logistic regression: glm(..., family = binomial)",
      "Classification evaluation: confusion matrix",
      "ROC curve and AUC",
      "Power analysis with pwr-style workflow",
      "ARIMA time-series modeling",
      "Seasonal ARIMA / SARIMA",
      "Automatic ARIMA selection",
      "ARCH effect test and GARCH volatility model",
      "Combined ARIMA mean model + GARCH volatility model",
      "Exploratory Factor Analysis: fa(), loadings, scores",
      "Regression/logistic models using factor scores",
      "Vector Autoregression: VARselect(), VAR()",
      "Granger matrix, causality(), IRF-style workflow",
      "Principal Component Analysis: prcomp()",
      "Rolling PCA for regime detection",
      "PCA projection and biplot workflow",
      "Standalone Granger causality note",
      "Naive Bayes posterior scenario probability and expected value"
    ),
    category = c(
      "Regression Models", "Regression Models", "Regression Models", "Regression Models", "Regression Models",
      "Statistical Relationship", "Statistical Relationship", "Statistical Relationship", "Statistical Relationship",
      "Statistical Relationship", "Statistical Relationship", "Statistical Relationship",
      "Group Comparison", "Group Comparison", "Group Comparison",
      "Group Comparison", "Group Comparison",
      "Generalized Models", "Generalized Models", "Generalized Models", "Generalized Models", "Generalized Models",
      "Decision & Probability",
      "Time Series", "Time Series", "Time Series",
      "Time Series", "Time Series",
      "Dimension Reduction", "Dimension Reduction",
      "Time Series", "Time Series",
      "Dimension Reduction", "Dimension Reduction", "Dimension Reduction",
      "Time Series", "Decision & Probability"
    ),
    method_id = c(
      "linear_regression", "polynomial_regression", "linear_regression", "linear_regression", "subset_regression",
      "independence_test", "independence_test", "independence_test", "independence_test",
      "correlation", "partial_correlation", "correlation",
      "anova", "anova", "anova",
      "ancova", "manova",
      "poisson_glm", "poisson_glm", "logistic_regression", "confusion_matrix", "roc",
      "power_analysis",
      "arima", "sarima", "arima",
      "garch", "garch",
      "efa", "efa",
      "var", "granger",
      "pca", "rolling_pca", "pca",
      "granger", "bayesian_scenario"
    ),
    method_name = c(
      "Linear Regression", "Polynomial Regression", "Linear Regression", "Linear Regression", "Subset Regression",
      "Independence Test", "Independence Test", "Independence Test", "Independence Test",
      "Correlation", "Partial Correlation", "Correlation",
      "ANOVA", "ANOVA", "ANOVA",
      "ANCOVA", "MANOVA",
      "Poisson GLM", "Poisson GLM", "Logistic Regression", "Confusion Matrix", "ROC",
      "Power Analysis",
      "ARIMA", "SARIMA", "ARIMA",
      "ARCH / GARCH", "ARCH / GARCH",
      "EFA", "EFA",
      "VAR", "Granger Causality",
      "PCA", "Rolling PCA", "PCA",
      "Granger Causality", "Bayesian Scenario Analysis"
    ),
    mapping_note = c(
      "Core example page includes CAD market regression background and variable meaning.",
      "Mapped to the polynomial regression detail page with AIC/BIC comparison.",
      "Mapped to linear regression because return transformation is a target construction choice.",
      "Mapped to linear regression; interaction terms are covered as model specification variants.",
      "Mapped to subset regression because the goal is variable selection.",
      "Same method page covers chi-square, Fisher, and CMH decision rules.",
      "Same method page covers sparse contingency-table cases.",
      "Same method page covers stratified categorical relationships.",
      "Visualization belongs with categorical independence workflow.",
      "Mapped to correlation page and network navigator continuous-vs-continuous path.",
      "Separate method page because controls change interpretation.",
      "Mapped to correlation page as p-value extension.",
      "Mapped to group comparison ANOVA.",
      "Diagnostics are explained under the ANOVA method page.",
      "Pairwise post-hoc comparison is part of ANOVA workflow.",
      "Mapped to ANCOVA because continuous covariates adjust group effects.",
      "Mapped to MANOVA because multiple outcomes are tested jointly.",
      "Mapped to Poisson GLM count-model page.",
      "Diagnostics remain inside Poisson GLM workflow.",
      "Mapped to binary outcome modeling.",
      "Mapped to classification evaluation page.",
      "Mapped to threshold-free classifier evaluation.",
      "Mapped to Decision & Probability section.",
      "Mapped to univariate time-series model page.",
      "Seasonal extension under Time Series.",
      "Mapped to ARIMA page as selection automation.",
      "Mapped to volatility modeling page.",
      "Mapped to GARCH page because volatility modeling is the distinctive step.",
      "Mapped to latent-factor discovery.",
      "Mapped to EFA because factor scores become downstream predictors.",
      "Mapped to multivariate time-series dynamics.",
      "Mapped to Granger causality page, with VAR as upstream model context.",
      "Mapped to PCA dimensionality reduction.",
      "Mapped to rolling PCA regime workflow.",
      "Mapped to PCA visualization/projection workflow.",
      "Mapped to Granger method page.",
      "Mapped to Bayesian scenario probability and EV workflow."
    ),
    stringsAsFactors = FALSE
  )
}

# get_method_notes()
# 功能：返回方法页面顶部的英文学习说明。
# 参数 method_id：get_method_catalog() 中定义的唯一方法 ID。
# 返回：包含 when/assumptions/inputs/outputs/interpretation 的命名字符向量。
get_method_notes <- function(method_id) {
  # notes 使用 method_id 作为名称，保存每个方法的五类学习说明。
  notes <- list(
    independence_test = c(
      when = "Use when both variables are categorical and you want to know whether their distributions are independent.",
      assumptions = "Expected cell counts should be large enough for chi-square; Fisher is safer for sparse tables.",
      inputs = "Two categorical variables, optionally stratified by a third categorical variable.",
      outputs = "Test statistic, p-value, contingency table, and mosaic-style interpretation.",
      interpretation = "A small p-value suggests the variables are not independent."
    ),
    correlation = c(
      when = "Use when both variables are continuous and the question is about association strength.",
      assumptions = "Pearson focuses on linear association; rank-based alternatives are better for monotonic non-linear relationships.",
      inputs = "Continuous columns such as FX returns, equity returns, yields, and macro indicators.",
      outputs = "Correlation matrix, heatmap, and pairwise test results.",
      interpretation = "Positive values move together; negative values move in opposite directions."
    ),
    partial_correlation = c(
      when = "Use when you want the association between two variables after controlling for other variables.",
      assumptions = "The control set should be economically meaningful and not purely mechanical.",
      inputs = "A covariance or correlation matrix across continuous variables.",
      outputs = "Partial correlation matrix and network graph.",
      interpretation = "Edges represent residual relationships after accounting for the rest of the system."
    ),
    linear_regression = c(
      when = "Use when you want to explain or predict one continuous variable with one or more predictors.",
      assumptions = "Linearity, independent errors, reasonable residual behavior, and stable relationship over the sample.",
      inputs = "USDCAD returns as target, TSX returns and rate changes as predictors.",
      outputs = "Coefficients, fitted values, residuals, confidence intervals, and scatter plot.",
      interpretation = "Coefficient signs show direction; p-values and intervals show statistical uncertainty."
    ),
    polynomial_regression = c(
      when = "Use when a continuous relationship may be curved rather than straight.",
      assumptions = "The polynomial term should have a plausible interpretation and not simply overfit noise.",
      inputs = "Continuous target and predictor with possible non-linearity.",
      outputs = "Linear and curved fitted lines, AIC/BIC comparison.",
      interpretation = "Use only if the curved model improves fit and remains interpretable."
    ),
    subset_regression = c(
      when = "Use when many candidate predictors exist and you want a compact model.",
      assumptions = "Candidate variables should be chosen before searching; validation is important.",
      inputs = "Target variable plus candidate predictors.",
      outputs = "Best subsets by adjusted R-squared, BIC, or Cp.",
      interpretation = "Lower BIC and simpler models are often preferred."
    ),
    anova = c(
      when = "Use when comparing a continuous outcome across groups.",
      assumptions = "Independent observations, roughly normal residuals, and similar group variances.",
      inputs = "Continuous response and categorical group variable.",
      outputs = "F-test, group means, and post-hoc comparisons.",
      interpretation = "A small p-value means at least one group mean differs."
    ),
    ancova = c(
      when = "Use when comparing groups while controlling for a continuous covariate.",
      assumptions = "Group effect and covariate effect should be meaningful together.",
      inputs = "Outcome, group variable, and covariate.",
      outputs = "Adjusted group effect and model diagnostics.",
      interpretation = "Shows whether group differences remain after risk or macro controls."
    ),
    manova = c(
      when = "Use when groups may differ across multiple continuous outcomes jointly.",
      assumptions = "Multivariate residual assumptions and enough observations per group.",
      inputs = "Multiple outcomes and one or more grouping variables.",
      outputs = "Multivariate test statistics and follow-up univariate tests.",
      interpretation = "Tests whether groups differ in the joint outcome space."
    ),
    poisson_glm = c(
      when = "Use for count outcomes such as events per period.",
      assumptions = "Mean and variance are close unless overdispersion is handled.",
      inputs = "Count target and explanatory variables.",
      outputs = "Rate ratios, residual diagnostics, and overdispersion check.",
      interpretation = "Exponentiated coefficients describe multiplicative changes in expected count."
    ),
    logistic_regression = c(
      when = "Use when the outcome is binary, such as up/down direction.",
      assumptions = "Independent observations and a log-odds relationship.",
      inputs = "Binary target with market and macro predictors.",
      outputs = "Odds ratios, predicted probability, confusion matrix, ROC.",
      interpretation = "Odds ratios above 1 increase the event odds."
    ),
    confusion_matrix = c(
      when = "Use to evaluate classification decisions.",
      assumptions = "Class threshold should match the business decision.",
      inputs = "Actual class and predicted class.",
      outputs = "Accuracy, sensitivity, specificity, and class errors.",
      interpretation = "Look at both correct calls and the type of mistakes."
    ),
    roc = c(
      when = "Use to evaluate probability scores across thresholds.",
      assumptions = "The score should rank positives above negatives.",
      inputs = "Binary target and predicted probability.",
      outputs = "ROC curve and AUC.",
      interpretation = "Higher AUC means better ranking ability."
    ),
    arima = c(
      when = "Use for univariate time series forecasting with autocorrelation.",
      assumptions = "Stationarity after differencing and stable residual behavior.",
      inputs = "One ordered numeric series, such as yield changes.",
      outputs = "AR/MA coefficients, fitted values, residual checks.",
      interpretation = "AR terms capture persistence; MA terms capture shock correction."
    ),
    sarima = c(
      when = "Use when a time series has seasonal structure.",
      assumptions = "Seasonality should be visible or economically justified.",
      inputs = "One ordered numeric series with seasonal frequency.",
      outputs = "Seasonal and non-seasonal ARIMA parameters.",
      interpretation = "Seasonal terms describe repeating calendar patterns."
    ),
    garch = c(
      when = "Use when volatility clusters over time.",
      assumptions = "Mean dynamics and volatility dynamics should be separated.",
      inputs = "Return series or model residuals.",
      outputs = "Conditional volatility and volatility forecasts.",
      interpretation = "High conditional sigma means the market is in a higher-risk regime."
    ),
    var = c(
      when = "Use when multiple time series influence each other dynamically.",
      assumptions = "Series should be stationary or transformed to stationarity.",
      inputs = "Several time-aligned series.",
      outputs = "Lag coefficients, impulse responses, and forecast error variance.",
      interpretation = "Shows how shocks propagate across variables."
    ),
    granger = c(
      when = "Use to test whether past values of one series improve prediction of another.",
      assumptions = "Time order matters and both series are aligned.",
      inputs = "Two stationary time series and lag order.",
      outputs = "F-test and p-value.",
      interpretation = "Small p-value means past X adds predictive information for Y."
    ),
    efa = c(
      when = "Use to discover latent factors behind many observed variables.",
      assumptions = "Observed variables share common underlying drivers.",
      inputs = "A matrix of correlated numeric variables.",
      outputs = "Factor loadings, scores, and variance explained.",
      interpretation = "Loadings help name latent macro factors."
    ),
    pca = c(
      when = "Use to compress many correlated numeric variables into orthogonal components.",
      assumptions = "Linear combinations are useful summaries of the data.",
      inputs = "Scaled numeric variables.",
      outputs = "Principal components, loadings, and explained variance.",
      interpretation = "PC1 explains the largest common variation."
    ),
    rolling_pca = c(
      when = "Use when factor structure may change across market regimes.",
      assumptions = "Window length should balance stability and responsiveness.",
      inputs = "Time-indexed numeric features.",
      outputs = "Rolling scores, loadings, and regime labels.",
      interpretation = "Changing scores can indicate changing macro regimes."
    ),
    cluster = c(
      when = "Use to group similar observations or assets.",
      assumptions = "Distance metric should match the problem.",
      inputs = "Scaled numeric features.",
      outputs = "Cluster labels and distance visualization.",
      interpretation = "Clusters reveal similar behavior patterns."
    ),
    power_analysis = c(
      when = "Use before or after a test to understand sample size and detectable effect.",
      assumptions = "Requires an assumed effect size, alpha, and power target.",
      inputs = "Effect size, significance level, sample size or desired power.",
      outputs = "Required sample size or achieved power.",
      interpretation = "Low power means a non-significant result may be inconclusive."
    ),
    bayesian_scenario = c(
      when = "Use when you want probabilistic regime classification and expected value.",
      assumptions = "Likelihood distributions should be checked and priors should be reasonable.",
      inputs = "TSX return, yield change, scenario labels, and next-day FX return.",
      outputs = "Posterior scenario probabilities, expected return, and backtest table.",
      interpretation = "Posterior probabilities translate market inputs into regime weights."
    )
  )

  # 返回当前方法说明；找不到 method_id 时使用 Linear Regression 作为默认值。
  notes[[method_id]] %||% notes$linear_regression
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# get_method_network()
# 功能：定义 Method Navigator 的节点和箭头。
# 参数：无。
# 返回：list(nodes, edges)。
# nodes 中 id 是节点唯一 ID，label 是显示文字，group 控制样式，
# method_id 决定方法节点点击后跳转到哪个页面；非方法节点的 method_id 为 NA。
# edges 中 from/to 是箭头起点和终点，arrows = "to" 表示箭头方向。
get_method_network <- function() {
  # nodes 定义网络图中可以看到和点击的节点。
  nodes <- data.frame(
    id = c(
      "start", "categorical_pair", "continuous_pair", "time_ordered", "many_variables", "binary_target",
      "group_question", "count_target", "forecasting", "volatility", "dynamic_system",
      "independence_test", "correlation", "partial_correlation", "linear_regression",
      "logistic_regression", "poisson_glm", "anova", "ancova", "manova", "arima",
      "sarima", "garch", "var", "granger", "pca", "efa", "rolling_pca",
      "power_analysis", "bayesian_scenario"
    ),
    label = c(
      "Data Question", "Categorical vs Categorical", "Continuous vs Continuous", "Time Ordered Data",
      "Many Correlated Variables", "Binary Target", "Group Comparison", "Count Target",
      "Forecasting", "Volatility Clustering", "Dynamic System",
      "Independence Test", "Correlation", "Partial Correlation", "Linear Regression",
      "Logistic Regression", "Poisson GLM", "ANOVA", "ANCOVA", "MANOVA", "ARIMA",
      "SARIMA", "ARCH / GARCH", "VAR", "Granger", "PCA", "EFA", "Rolling PCA",
      "Power Analysis", "Bayesian Scenario"
    ),
    group = c(
      "question", rep("data_type", 6), rep("goal", 4), rep("method", 19)
    ),
    method_id = c(rep(NA, 11), "independence_test", "correlation", "partial_correlation",
                  "linear_regression", "logistic_regression", "poisson_glm", "anova",
                  "ancova", "manova", "arima", "sarima", "garch", "var", "granger",
                  "pca", "efa", "rolling_pca", "power_analysis", "bayesian_scenario"),
    stringsAsFactors = FALSE
  )

  # edges 使用节点 id 定义箭头连接关系。
  edges <- data.frame(
    from = c(
      "start", "start", "start", "start", "start", "start", "start",
      "categorical_pair", "continuous_pair", "continuous_pair", "continuous_pair",
      "binary_target", "count_target", "group_question", "group_question", "group_question",
      "time_ordered", "time_ordered", "time_ordered", "time_ordered",
      "forecasting", "forecasting", "volatility", "dynamic_system", "dynamic_system",
      "many_variables", "many_variables", "many_variables", "start"
    ),
    to = c(
      "categorical_pair", "continuous_pair", "time_ordered", "many_variables", "binary_target",
      "group_question", "count_target",
      "independence_test", "correlation", "partial_correlation", "linear_regression",
      "logistic_regression", "poisson_glm", "anova", "ancova", "manova",
      "forecasting", "volatility", "dynamic_system", "granger",
      "arima", "sarima", "garch", "var", "bayesian_scenario",
      "pca", "efa", "rolling_pca", "power_analysis"
    ),
    arrows = "to",
    stringsAsFactors = FALSE
  )

  # 返回 visNetwork() 需要的节点表和连接表。
  list(nodes = nodes, edges = edges)
}
