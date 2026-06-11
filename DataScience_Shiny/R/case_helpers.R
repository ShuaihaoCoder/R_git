# ============================================================
# Complete case-study helpers
# ============================================================
# 整体作用：统一每个方法页面返回的数据结构，并提供常用绘图、检验和数据准备工具。
# 所有案例都使用同一套结构，app.R 才能循环展示多张图、多张表和教学步骤。

# new_case()
# 功能：把一个统计方法的背景、步骤、图、表和检验整理成统一案例。
# plots/tables 使用命名 list；名称会直接成为网页中对应结果的标题。
new_case <- function(title, background, question, objective, variables, steps,
                     plots, tables, tests, model_summary, code, conclusion,
                     plot_notes = NULL) {
  # 没有手工提供说明时，根据图名和实际图层生成专用说明；说明名称必须和 plots 完全对应。
  if (is.null(plot_notes)) plot_notes <- build_plot_notes(plots)
  if (!identical(names(plot_notes), names(plots)) || any(!nzchar(plot_notes))) {
    stop("Every case plot must have a non-empty, same-named plot note.", call. = FALSE)
  }
  # list() 保留不同类型的结果对象；app.R 会按字段名称分别放入对应网页区域。
  list(
    title = title,
    background = background,
    question = question,
    objective = objective,
    variables = variables,
    steps = steps,
    plots = plots,
    plot_notes = plot_notes,
    tables = tables,
    tests = tests,
    model_summary = model_summary,
    code = code,
    conclusion = conclusion
  )
}

# teaching_steps()
# 功能：把每个案例的分析顺序和英文解释整理成网页表格。
teaching_steps <- function(...) {
  # ... 接收任意数量的 c("阶段", "说明")；list(...) 将这些步骤先集中保存。
  items <- list(...)
  # vapply() 从每个步骤取固定位置的文字，并确保每次都返回一个字符串。
  data.frame(
    step = seq_along(items),
    stage = vapply(items, `[[`, character(1), 1),
    explanation = vapply(items, `[[`, character(1), 2),
    stringsAsFactors = FALSE
  )
}

# test_result()
# 功能：把不同统计检验整理为统一列，方便学生横向比较原假设和结论。
test_result <- function(test, null_hypothesis, statistic, p_value, interpretation = NULL) {
  # [1] 只保留第一个统计量和 p-value，避免命名向量影响网页表格。
  p_value <- as.numeric(p_value)[1]
  # 使用 5% 显著性水平自动生成统一结论；NA 表示该项只是诊断指标而非正式 p-value。
  conclusion <- if (is.na(p_value)) {
    "Review the reported statistic."
  } else if (p_value < 0.05) {
    "Reject H0 at the 5% level."
  } else {
    "Do not reject H0 at the 5% level."
  }
  data.frame(
    test = test,
    null_hypothesis = null_hypothesis,
    statistic = round(as.numeric(statistic)[1], 4),
    p_value = round(p_value, 6),
    conclusion = conclusion,
    interpretation = if (is.null(interpretation)) conclusion else interpretation,
    stringsAsFactors = FALSE
  )
}

# bind_tests()
# 功能：合并多个检验结果；没有检验时仍返回可以直接展示的表格。
bind_tests <- function(...) {
  # Filter(Negate(is.null), ...) 删除没有成功生成的可选检验。
  tests <- Filter(Negate(is.null), list(...))
  if (length(tests) == 0) {
    return(data.frame(
      test = "Descriptive workflow",
      null_hypothesis = "Not applicable",
      statistic = NA_real_,
      p_value = NA_real_,
      conclusion = "This method is descriptive rather than hypothesis-driven.",
      interpretation = "Read the plots and reported metrics together.",
      stringsAsFactors = FALSE
    ))
  }
  # do.call() 把 tests list 中的多个表依次传给 rbind()，合并成一张检验表。
  do.call(rbind, tests)
}

# variable_rows()
# 功能：只返回当前案例真正用到的变量解释。
variable_rows <- function(...) {
  # names(values) 是变量名；unlist() 取出对应解释并转换为普通字符列。
  values <- list(...)
  data.frame(
    variable = names(values),
    meaning = unlist(values, use.names = FALSE),
    stringsAsFactors = FALSE
  )
}

# standard_theme()
# 功能：统一全部案例图的字体、网格和标题样式，让多方法页面保持一致。
standard_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

# plot_teaching_note()
# 功能：根据图名说明图表回答的问题，再结合实际图层解释点、线、柱、颜色等视觉元素。
plot_teaching_note <- function(plot_title, plot_object = NULL) {
  title <- tolower(plot_title)
  visual_note <- describe_plot_marks(plot_object)
  if (grepl("residual|acf|pacf", title)) {
    return(paste("This chart checks whether meaningful structure remains after modeling.", visual_note, "Patterns or bars beyond reference limits suggest the model has not explained all dependence."))
  }
  if (grepl("forecast", title)) {
    return(paste("This chart asks how the series may develop beyond the observed sample.", visual_note, "Read the central path as the forecast and any surrounding band as uncertainty."))
  }
  if (grepl("correlation|loading|heatmap", title)) {
    return(paste("This chart compares the direction and strength of relationships across variables.", visual_note, "Stronger colors or larger absolute values indicate stronger relationships."))
  }
  if (grepl("confusion", title)) {
    return(paste("This chart asks where the classifier is correct and where it makes mistakes.", visual_note, "Diagonal cells are correct classifications; off-diagonal cells are the two error types."))
  }
  if (grepl("roc", title)) {
    return(paste("This chart compares classification quality across all probability thresholds.", visual_note, "A curve closer to the upper-left corner means better ranking with fewer false positives."))
  }
  if (grepl("volatility|garch|shock", title)) {
    return(paste("This chart asks when market uncertainty rises and how long it remains elevated.", visual_note, "Slow declines after large shocks indicate persistent volatility."))
  }
  if (grepl("cluster|score|joint|distribution", title)) {
    return(paste("This chart asks whether observations form distinct groups or overlapping regimes.", visual_note, "Separation supports different group labels; overlap means the distinction is weaker."))
  }
  if (grepl("tukey|group|regime|slope", title)) {
    return(paste("This chart compares groups, regimes, or fitted relationships.", visual_note, "Compare centers and uncertainty; visible overlap means the evidence is weaker."))
  }
  if (grepl("fit|observed|actual|predicted", title)) {
    return(paste("This chart asks how closely model results follow the observed data.", visual_note, "Closer agreement indicates better fit, while repeated gaps reveal model weakness."))
  }
  paste("This chart provides visual evidence for the current case-study step.", visual_note, "Use it together with the related result table and statistical tests.")
}

# describe_plot_marks()
# 功能：读取 ggplot 中实际使用的图层，简短解释点、线、柱、色块和阴影分别代表什么。
describe_plot_marks <- function(plot_object) {
  if (!inherits(plot_object, "ggplot")) {
    return("Read the axes, legend, and reference marks together when comparing the displayed values.")
  }
  geom_names <- unique(vapply(plot_object$layers, function(layer) class(layer$geom)[1], character(1)))
  marks <- character()
  if (any(grepl("Point", geom_names))) marks <- c(marks, "points represent individual observations")
  if (any(grepl("Line|Path|Smooth", geom_names))) marks <- c(marks, "lines show trends, fitted paths, or reference levels")
  if (any(grepl("Col|Bar", geom_names))) marks <- c(marks, "bar height shows the size of each value")
  if (any(grepl("Boxplot|Violin|Density|Ridgeline", geom_names))) marks <- c(marks, "the shapes summarize each distribution and its spread")
  if (any(grepl("Tile|Raster", geom_names))) marks <- c(marks, "colored cells encode the value at each row-column combination")
  if (any(grepl("Ribbon|Area", geom_names))) marks <- c(marks, "the shaded area shows a range, contribution, or uncertainty")
  if (length(marks) == 0) marks <- "the plotted marks encode the values shown on the axes"
  paste0("In this figure, ", paste(marks, collapse = "; "), ".")
}

# build_plot_notes()
# 功能：为案例的每张图建立同名说明，供 App 和 UIimprove 图册直接读取。
build_plot_notes <- function(plots) {
  if (length(plots) == 0 || is.null(names(plots)) || any(!nzchar(names(plots)))) {
    stop("Case plots must be a named, non-empty list.", call. = FALSE)
  }
  notes <- vapply(seq_along(plots), function(index) {
    plot_teaching_note(names(plots)[index], plots[[index]])
  }, character(1))
  names(notes) <- names(plots)
  notes
}

# broom_like_coefficients()
# 功能：把 lm()、glm() 等模型摘要中的系数矩阵整理成网页容易阅读的统一表格。
broom_like_coefficients <- function(fit) {
  # summary(fit)$coefficients 返回矩阵；as.data.frame() 将它变成普通结果表。
  coefficient_table <- as.data.frame(summary(fit)$coefficients)
  coefficient_table$term <- rownames(coefficient_table)
  rownames(coefficient_table) <- NULL

  # 将 term 移到第一列，并统一列名，避免不同模型页面使用不同叫法。
  coefficient_table <- coefficient_table[, c("term", setdiff(names(coefficient_table), "term"))]
  names(coefficient_table) <- c("term", "estimate", "std_error", "statistic", "p_value")
  coefficient_table
}

# matrix_long()
# 功能：把相关矩阵等宽表转换成长表，方便 ggplot2 绘制热力图。
matrix_long <- function(matrix_object, value_name = "value") {
  # as.table() 将矩阵的行、列和值拆开；as.data.frame() 再变成 ggplot2 可读取的长表。
  result <- as.data.frame(as.table(matrix_object), stringsAsFactors = FALSE)
  names(result) <- c("x", "y", value_name)
  result
}

# matrix_heatmap()
# 功能：绘制带数值标签的矩阵热力图，适合相关、载荷和混淆矩阵。
matrix_heatmap <- function(matrix_object, title, low = "#2B6CB0", high = "#C2410C") {
  # round(..., 3) 控制图中文字长度，避免相关矩阵标签过密。
  plot_data <- matrix_long(round(matrix_object, 3))
  ggplot2::ggplot(plot_data, ggplot2::aes(x, y, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = value), size = 3.5) +
    ggplot2::scale_fill_gradient2(low = low, mid = "white", high = high) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title, x = NULL, y = NULL, fill = NULL) +
    standard_theme()
}

# prepare_macro_group_data()
# 功能：沿用原 DataScience.R 的 NFP、美国 10 年期利率和 Risk 案例数据逻辑。
# 找不到某个长列名时会立即报出缺少的部分，避免静默使用错误变量。
prepare_macro_group_data <- function(data_bundle) {
  allx_data <- data_bundle$allx
  rates_data <- data_bundle$rates

  nfp_col <- safe_grep_columns(allx_data, "(?i)Nonfarm Payrolls Total MoM Net Change", 1)
  risk_col <- safe_grep_columns(allx_data, "(?i)UNITED STATES Bloomberg Country Risk Economic Score", 1)
  rate_col <- safe_grep_columns(rates_data, "(?i)US OIS 10Y", 1)

  if (length(nfp_col) == 0 || length(risk_col) == 0 || length(rate_col) == 0) {
    stop("Cannot find NFP, US OIS 10Y, or US Risk columns for group examples.", call. = FALSE)
  }

  # merge(..., all = FALSE) 只保留宏观数据库和利率数据库都有记录的日期。
  macro_data <- merge(
    allx_data[, c("date", nfp_col, risk_col), with = FALSE],
    rates_data[, c("date", rate_col), with = FALSE],
    by = "date",
    all = FALSE
  )
  data.table::setnames(macro_data, names(macro_data), c("date", "NFP", "Risk", "US10Y"))

  # na.locf() 使用上一个可用值填补宏观数据发布日之间的空白日期。
  macro_data[, NFP := zoo::na.locf(NFP, na.rm = FALSE)]
  macro_data[, Risk := zoo::na.locf(Risk, na.rm = FALSE)]
  macro_data <- na.omit(macro_data)
  macro_data[, change_10y := US10Y - data.table::shift(US10Y)]
  macro_data <- na.omit(macro_data)

  # 用唯一分位点建立四组；duplicated() 防止重复分位点使 cut() 报错。
  nfp_breaks <- unique(as.numeric(stats::quantile(macro_data$NFP, probs = seq(0, 1, 0.25))))
  risk_breaks <- unique(as.numeric(stats::quantile(macro_data$Risk, probs = seq(0, 1, 0.25))))
  macro_data[, NFP_cat := cut(NFP, breaks = nfp_breaks, include.lowest = TRUE)]
  macro_data[, Risk_cat := cut(Risk, breaks = risk_breaks, include.lowest = TRUE)]
  na.omit(macro_data)
}

# prepare_factor_data()
# 功能：建立 EFA、PCA、VAR 和聚类共同使用的连续型 CAD 市场变量表。
prepare_factor_data <- function(data_bundle) {
  cad_data <- prepare_cad_market_data(data_bundle)
  # complete.cases() 只保留五个变量都有数值的观测，避免 PCA/EFA 因缺失值停止。
  result <- as.data.frame(cad_data[, .(USDCAD_ret, TSXC_ret, delta10y, CAD_ON, X10Y)])
  result <- result[stats::complete.cases(result), ]
  result[seq_len(min(nrow(result), 2500)), , drop = FALSE]
}

# prepare_var_data()
# 功能：先用 PCA 产生三个共同市场因子，再与利率变化组成 VAR 系统。
prepare_var_data <- function(data_bundle) {
  factor_data <- prepare_factor_data(data_bundle)
  # PCA 将五个相关市场变量压缩成 ML1-ML3，作为 VAR 中的共同市场因子。
  pca_fit <- stats::prcomp(factor_data, center = TRUE, scale. = TRUE)
  result <- data.frame(
    delta10y = factor_data$delta10y,
    ML1 = pca_fit$x[, 1],
    ML2 = pca_fit$x[, 2],
    ML3 = pca_fit$x[, 3]
  )
  # diff() 对所有序列做一阶差分，让 VAR 使用变化量而不是可能带趋势的水平值。
  as.data.frame(diff(as.matrix(result)))
}

# fit_simple_var()
# 功能：用每个变量自己的滞后值和其他变量的滞后值拟合一个简单 VAR。
# 返回每条方程、残差和拟合值，供 VAR、Granger、IRF 教学案例共同使用。
fit_simple_var <- function(data, lag = 2) {
  matrix_data <- as.matrix(data)
  variable_names <- colnames(matrix_data)
  response_index <- (lag + 1):nrow(matrix_data)

  lagged_parts <- lapply(seq_len(lag), function(current_lag) {
    # 每次向前移动 current_lag 行，并在列名后标记 L1、L2 等滞后阶数。
    part <- matrix_data[response_index - current_lag, , drop = FALSE]
    colnames(part) <- paste0(variable_names, "_L", current_lag)
    part
  })
  design <- as.data.frame(do.call(cbind, lagged_parts))
  # VAR 可以理解为多条回归方程：每个变量轮流作为 response，解释变量保持同一组滞后值。
  equations <- lapply(variable_names, function(variable) {
    equation_data <- data.frame(response_value = matrix_data[response_index, variable], design)
    stats::lm(response_value ~ ., data = equation_data)
  })
  names(equations) <- variable_names

  list(
    equations = equations,
    residuals = sapply(equations, stats::residuals),
    fitted = sapply(equations, stats::fitted),
    design = design,
    response = matrix_data[response_index, , drop = FALSE],
    lag = lag
  )
}

# granger_table()
# 功能：逐个删除某个变量的全部滞后项，并与完整 VAR 方程比较，得到 Granger 检验表。
granger_table <- function(var_fit, response = "delta10y") {
  full_fit <- var_fit$equations[[response]]
  causes <- setdiff(colnames(var_fit$response), response)
  rows <- lapply(causes, function(cause) {
    # grepl() 找到 cause 的全部滞后列；删掉这些列后拟合 reduced model。
    keep_columns <- !grepl(paste0("^", cause, "_L"), names(var_fit$design))
    reduced_data <- data.frame(response_value = var_fit$response[, response], var_fit$design[, keep_columns, drop = FALSE])
    reduced_fit <- stats::lm(response_value ~ ., data = reduced_data)
    comparison <- stats::anova(reduced_fit, full_fit)
    # 如果删掉 cause 后模型明显变差，说明 cause 的过去值增加了 response 的预测信息。
    data.frame(
      cause = cause,
      response = response,
      F_statistic = comparison$F[2],
      p_value = comparison$`Pr(>F)`[2],
      strength = -log10(comparison$`Pr(>F)`[2]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# acf_data()
# 功能：把 acf() 的结果转换为数据框，避免基础绘图无法直接嵌入多图页面。
acf_data <- function(series, lag_max = 30, type = "correlation") {
  # plot = FALSE 让 acf() 只返回计算结果；网页绘图由 ggplot2 统一完成。
  result <- stats::acf(series, lag.max = lag_max, plot = FALSE, type = type, na.action = na.pass)
  data.frame(lag = as.numeric(result$lag), value = as.numeric(result$acf))
}

# acf_plot()
# 功能：绘制 ACF 或 PACF，并显示约 95% 的白噪声界限。
acf_plot <- function(series, title, type = "correlation") {
  plot_data <- acf_data(series, type = type)
  # 1.96 / sqrt(n) 是白噪声情况下常用的约 95% 自相关参考界限。
  bound <- 1.96 / sqrt(sum(!is.na(series)))
  ggplot2::ggplot(plot_data, ggplot2::aes(lag, value)) +
    ggplot2::geom_hline(yintercept = c(-bound, bound), linetype = "dashed", color = "#C2410C") +
    ggplot2::geom_col(fill = "#335C67", width = 0.65) +
    ggplot2::labs(title = title, x = "Lag", y = "Correlation") +
    standard_theme()
}

# fit_garch_manual()
# 功能：使用最大似然估计简单 GARCH(1,1)，避免完整案例依赖额外的 rugarch 安装。
fit_garch_manual <- function(residual_series) {
  x <- as.numeric(residual_series)
  x <- x[is.finite(x)]
  variance_start <- stats::var(x)

  # objective()
  # 功能：给定一组候选参数，计算这组参数下的 GARCH 负对数似然；optim() 会反复调用它寻找最小值。
  objective <- function(raw_parameters) {
    # exp() 保证原始参数为正；分母转换保证 alpha + beta 小于 1，波动不会无限发散。
    transformed <- exp(raw_parameters)
    omega <- transformed[1]
    alpha <- transformed[2] / (1 + transformed[2] + transformed[3])
    beta <- transformed[3] / (1 + transformed[2] + transformed[3])
    conditional_variance <- numeric(length(x))
    conditional_variance[1] <- variance_start
    for (index in 2:length(x)) {
      # 今天的条件方差由基础波动、昨天冲击平方和昨天条件方差共同决定。
      conditional_variance[index] <- omega + alpha * x[index - 1]^2 + beta * conditional_variance[index - 1]
    }
    0.5 * sum(log(conditional_variance) + x^2 / conditional_variance)
  }

  # optim() 寻找使负对数似然最小的参数，也就是最符合这组残差的 GARCH 参数。
  fit <- stats::optim(log(c(variance_start * 0.05, 0.1, 0.8)), objective)
  transformed <- exp(fit$par)
  omega <- transformed[1]
  alpha <- transformed[2] / (1 + transformed[2] + transformed[3])
  beta <- transformed[3] / (1 + transformed[2] + transformed[3])
  conditional_variance <- numeric(length(x))
  conditional_variance[1] <- variance_start
  for (index in 2:length(x)) {
    conditional_variance[index] <- omega + alpha * x[index - 1]^2 + beta * conditional_variance[index - 1]
  }
  list(
    coefficients = c(omega = omega, alpha1 = alpha, beta1 = beta),
    sigma = sqrt(conditional_variance),
    standardized_residuals = x / sqrt(conditional_variance),
    convergence = fit$convergence
  )
}

# manual_roc()
# 功能：根据真实标签和预测概率计算 ROC 坐标和 AUC，不依赖额外 ROC package。
manual_roc <- function(actual, probability) {
  # 将每个预测概率都当作一次分类阈值，计算该阈值下的 TPR 和 FPR。
  thresholds <- sort(unique(c(Inf, probability, -Inf)), decreasing = TRUE)
  points <- lapply(thresholds, function(threshold) {
    predicted <- probability >= threshold
    tp <- sum(predicted & actual == 1)
    fp <- sum(predicted & actual == 0)
    fn <- sum(!predicted & actual == 1)
    tn <- sum(!predicted & actual == 0)
    data.frame(threshold = threshold, TPR = tp / (tp + fn), FPR = fp / (fp + tn))
  })
  roc_data <- do.call(rbind, points)
  roc_data <- roc_data[order(roc_data$FPR, roc_data$TPR), ]
  # 使用梯形面积近似 ROC 曲线下面积；AUC 越高表示排序能力越强。
  auc <- sum(diff(roc_data$FPR) * (head(roc_data$TPR, -1) + tail(roc_data$TPR, -1)) / 2)
  list(data = roc_data, auc = auc)
}
