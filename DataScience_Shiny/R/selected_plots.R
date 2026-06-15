# ============================================================
# Selected reference plots and runtime PNG cache
# ============================================================
# Overall role: read the user's gallery decisions, rebuild the selected plots
# from live project data, and prepare ready-to-display PNG files before Shiny opens.

# get_selected_plot_requests()
# Role: read the GB18030 CSV and return filled decisions through ANCOVA.
get_selected_plot_requests <- function(csv_path) {
  requests <- utils::read.csv(csv_path, fileEncoding = "GB18030", check.names = FALSE, stringsAsFactors = FALSE)
  names(requests)[tolower(names(requests)) == "comment"] <- "comment"
  requests <- requests[nzchar(trimws(requests$comment)), , drop = FALSE]
  requests$keep <- !grepl("\u4e0d\u4fdd\u7559|\u6ca1\u5fc5\u8981", requests$comment)
  requests
}

# selected_plot_notes()
# Role: provide the requested English teaching explanation for each rebuilt plot.
selected_plot_notes <- function() {
  c(
    "Original FX-equity relationship" = "This raw-data view asks whether USDCAD and TSXC returns move together. Each point is one aligned observation; the fitted line summarizes the average linear relationship.",
    "Original FX-rate relationship" = "This raw-data view compares USDCAD returns with daily CAD 10-year yield changes. Each point is one aligned observation and helps frame the continuous-variable relationship used on this page.",
    "Original correlation matrix" = "This matrix reports ordinary pairwise correlations before controlling for other variables. Cell color and the printed value show direction and strength; values near zero indicate weak linear co-movement.",
    "Correlation versus partial correlation" = "Each point compares one ordinary correlation on the horizontal axis with its partial correlation on the vertical axis. Distance from the dashed 45-degree line shows how much the relationship changes after controlling for the other variables.",
    "Original partial correlation matrix" = "This matrix shows relationships after linearly controlling for the remaining variables. Strong positive and negative cells identify relationships that remain after shared market exposure is removed.",
    "Significant partial correlations" = "This heatmap keeps partial correlations with p-values below 0.05 and colors nonsignificant cells grey. Red indicates positive controlled relationships, purple indicates negative relationships, and stronger color means a larger absolute effect.",
    "10Y yield timeline" = "This timeline provides the yield-level background before ANOVA grouping. The line follows the US 10-year rate through time, while color identifies the yield-rank regime.",
    "Log-transformed NFP timeline" = "NFP is log-transformed here to reduce the visual influence of extreme payroll observations. Bar height and color show the transformed labor-market signal through time.",
    "NFP quartile boxplot" = "NFP observations are divided at the 0%, 25%, 50%, 75%, and 100% quantiles. Each box summarizes the distribution of 10-year yield changes inside one NFP quartile.",
    "NFP quartile violin distribution" = "The violin width shows where yield-change observations are concentrated within each NFP quartile. The embedded boxplot summarizes the median and middle half of each distribution.",
    "NFP quartile density ridges" = "Each panel shows the yield-change density for one NFP quartile. Similar shapes imply comparable distributions; shifts in the peaks or tails suggest regime differences.",
    "Tukey HSD intervals" = "Tukey HSD tests the null hypothesis that each pair of NFP-group means is equal while controlling the family-wise error rate. A confidence interval crossing zero is not significant; an interval entirely on one side of zero identifies a significant pairwise difference.",
    "Raw NFP-risk interaction" = "This chart tests whether the relationship between NFP category and mean yield change differs across Risk categories. Points are observed cell means, lines connect the same Risk group, and error bars show one standard error; nonparallel lines indicate interaction.",
    "Adjusted NFP-risk interaction" = "This chart shows model-adjusted mean yield changes for each NFP-Risk combination. Unlike the raw interaction chart, the points come from the fitted additive model; the lines therefore emphasize estimated main effects after model adjustment.",
    "Yield-level interaction by NFP and Risk" = "This interaction plot asks whether NFP effects on the 10-year yield depend on Risk category. Each point is a group mean and each line follows one Risk category; clearly nonparallel lines warn that the effects interact.",
    "ANCOVA slope check with uncertainty" = "ANCOVA assumes a broadly comparable Risk-yield slope across NFP groups. Points are observations and fitted lines with confidence bands show each group relationship; visibly different slopes indicate that the parallel-slope assumption is weak.",
    "ANCOVA parallel-slope comparison" = "This focused slope comparison removes confidence bands so differences between fitted group slopes are easier to see. Approximately parallel lines support standard ANCOVA; crossings or strongly different slopes suggest an interaction model is more appropriate.",
    "ANCOVA variance distribution" = "This plot checks whether yield variability is reasonably similar across NFP groups. Similar violin widths and boxplot spreads support equal variance; noticeably different spreads indicate heteroskedasticity.",
    "ANCOVA adjusted means" = "These are NFP-group yield estimates evaluated at the same average Risk level. Differences that remain after this adjustment suggest NFP relates to 10-year yields through more than the Risk channel alone."
  )
}

# selected_visual_sections()
# Role: define the teaching order used by the dashboard for the covered methods.
selected_visual_sections <- function(method_id, plot_names) {
  section_map <- list(
    independence_test = list("Relationship Context" = "Original FX-rate relationship", "Sample-data Independence Example" = c("Observed counts", "Within-group proportions")),
    correlation = list("Correlation Overview" = c("Original correlation matrix", "Correlation heatmap"), "Focused Relationships" = c("Key pair scatterplot", "Correlation versus partial correlation")),
    partial_correlation = list("Controlled Relationships" = c("Original partial correlation matrix", "Partial correlation heatmap", "Significant partial correlations"), "Ordinary Versus Controlled" = "Ordinary vs partial"),
    linear_regression = list("Observed Relationship" = c("Observed relationship", "Original FX-equity relationship"), "Model Diagnostics" = "Residual diagnostics"),
    polynomial_regression = list("Observed Relationship" = "Observed relationship", "Model Diagnostics" = "Residual diagnostics"),
    subset_regression = list("Model Selection" = c("Model trade-off", "Selected coefficients")),
    anova = list("Data Preparation" = c("10Y yield timeline", "Log-transformed NFP timeline", "NFP quartile boxplot"), "Group Distributions" = c("Group distributions", "NFP quartile violin distribution", "NFP quartile density ridges"), "Pairwise Inference" = "Tukey HSD intervals", "NFP-Risk Interaction" = c("Raw NFP-risk interaction", "Adjusted NFP-risk interaction")),
    ancova = list("Interaction and Assumptions" = c("Yield-level interaction by NFP and Risk", "ANCOVA slope check with uncertainty", "ANCOVA parallel-slope comparison", "ANCOVA variance distribution", "Residual diagnostics"), "Adjusted Comparison" = "ANCOVA adjusted means")
  )
  sections <- section_map[[method_id]]
  if (is.null(sections)) return(list("Visual Analysis" = plot_names))
  lapply(sections, function(names_in_section) intersect(names_in_section, plot_names))
}

# build_selected_reference_plots()
# Role: rebuild only the selected Original Reference plots assigned to method_id.
build_selected_reference_plots <- function(method_id, data_bundle) {
  notes <- selected_plot_notes()
  plots <- list()

  if (method_id %in% c("linear_regression", "independence_test")) {
    cad <- as.data.frame(prepare_cad_market_data(data_bundle))
  }
  if (method_id %in% c("correlation", "partial_correlation")) {
    factor_data <- prepare_factor_data(data_bundle)
    correlation <- stats::cor(factor_data[, 1:4], use = "pairwise.complete.obs")
    inverse_covariance <- solve(stats::cov(factor_data[, 1:4]))
    partial <- -stats::cov2cor(inverse_covariance)
    diag(partial) <- 1
  }

  if (method_id == "linear_regression") {
    plots[["Original FX-equity relationship"]] <- ggplot2::ggplot(cad, ggplot2::aes(TSXC_ret, USDCAD_ret)) +
      ggplot2::geom_point(alpha = 0.25, color = "#335C67") +
      ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#D95D39") +
      ggplot2::labs(title = "Original USDCAD Return vs TSXC Return", x = "TSXC return", y = "USDCAD return") + standard_theme()
  }
  if (method_id == "independence_test") {
    plots[["Original FX-rate relationship"]] <- ggplot2::ggplot(cad, ggplot2::aes(delta10y, USDCAD_ret)) +
      ggplot2::geom_point(alpha = 0.25, color = "#335C67") +
      ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#D95D39") +
      ggplot2::labs(title = "Original USDCAD Return vs CAD 10Y Change", x = "CAD 10Y change", y = "USDCAD return") + standard_theme()
  }
  if (method_id == "correlation") {
    plots[["Original correlation matrix"]] <- matrix_heatmap(correlation, "Original Correlation Matrix", low = "#6A51A3", high = "#D7301F")
    comparison <- data.frame(correlation = as.numeric(correlation), partial_correlation = as.numeric(partial))
    plots[["Correlation versus partial correlation"]] <- ggplot2::ggplot(comparison, ggplot2::aes(correlation, partial_correlation)) +
      ggplot2::geom_point(alpha = 0.7, color = "#2B6CB0") + ggplot2::geom_abline(linetype = "dashed", color = "#C2410C") +
      ggplot2::labs(title = "Correlation vs Partial Correlation", x = "Correlation", y = "Partial correlation") + standard_theme()
  }
  if (method_id == "partial_correlation") {
    plots[["Original partial correlation matrix"]] <- matrix_heatmap(partial, "Original Partial Correlation Matrix", low = "#6A51A3", high = "#D7301F")
    n <- nrow(factor_data)
    df <- n - ncol(factor_data[, 1:4])
    t_value <- partial * sqrt(df / pmax(1e-12, 1 - partial^2))
    p_values <- 2 * stats::pt(-abs(t_value), df = df)
    significant <- partial
    significant[p_values >= 0.05] <- NA_real_
    plot_data <- matrix_long(significant)
    plots[["Significant partial correlations"]] <- ggplot2::ggplot(plot_data, ggplot2::aes(x, y, fill = value)) +
      ggplot2::geom_tile(color = "white") + ggplot2::scale_fill_gradient2(low = "#7B68EE", mid = "white", high = "#E63946", na.value = "grey85") +
      ggplot2::labs(title = "Significant Partial Correlation Heatmap", subtitle = "Grey = not significant (p >= 0.05)", x = NULL, y = NULL, fill = "Partial corr") + standard_theme()
  }

  if (method_id %in% c("anova", "ancova")) {
    macro <- as.data.frame(prepare_macro_group_data(data_bundle))
    macro <- macro[seq_len(min(nrow(macro), 3000)), , drop = FALSE]
  }
  if (method_id == "anova") {
    macro$NFP_log <- sign(macro$NFP) * log1p(abs(macro$NFP))
    macro$yield_rank <- cut(macro$US10Y, breaks = stats::quantile(macro$US10Y, seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE)
    plots[["10Y yield timeline"]] <- ggplot2::ggplot(macro, ggplot2::aes(date, US10Y, color = yield_rank)) +
      ggplot2::geom_line(linewidth = 0.65) + ggplot2::labs(title = "US 10Y Yield Timeline", x = NULL, y = "US OIS 10Y", color = "Yield quartile") + standard_theme()
    plots[["Log-transformed NFP timeline"]] <- ggplot2::ggplot(macro, ggplot2::aes(date, NFP_log, fill = NFP_log)) +
      ggplot2::geom_col() + ggplot2::scale_fill_gradient2(low = "#2B6CB0", mid = "grey90", high = "#C2410C") +
      ggplot2::labs(title = "Log-transformed NFP Through Time", x = NULL, y = "Signed log(1 + |NFP|)", fill = NULL) + standard_theme()
    plots[["NFP quartile boxplot"]] <- ggplot2::ggplot(macro, ggplot2::aes(NFP_cat, change_10y, fill = NFP_cat)) +
      ggplot2::geom_boxplot(show.legend = FALSE) + ggplot2::labs(title = "10Y Yield Change by NFP Quartile", x = "NFP quartile", y = "10Y yield change") + standard_theme()
    plots[["NFP quartile violin distribution"]] <- ggplot2::ggplot(macro, ggplot2::aes(NFP_cat, change_10y, fill = NFP_cat)) +
      ggplot2::geom_violin(alpha = 0.65) + ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, show.legend = FALSE) +
      ggplot2::labs(title = "Distribution of 10Y Yield Change by NFP Quartile", x = "NFP quartile", y = "10Y yield change") + standard_theme()
    plots[["NFP quartile density ridges"]] <- ggplot2::ggplot(macro, ggplot2::aes(change_10y, fill = NFP_cat)) +
      ggplot2::geom_density(alpha = 0.65, show.legend = FALSE) + ggplot2::facet_grid(NFP_cat ~ ., scales = "free_y") +
      ggplot2::labs(title = "Density of 10Y Yield Change by NFP Quartile", x = "10Y yield change", y = "Density") + standard_theme()
    fit <- stats::aov(change_10y ~ NFP_cat, data = macro)
    tukey <- as.data.frame(stats::TukeyHSD(fit)$NFP_cat)
    tukey$comparison <- rownames(tukey)
    plots[["Tukey HSD intervals"]] <- ggplot2::ggplot(tukey, ggplot2::aes(diff, reorder(comparison, diff))) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed") + ggplot2::geom_errorbar(ggplot2::aes(xmin = lwr, xmax = upr), width = 0.2) +
      ggplot2::geom_point(size = 3, color = "#335C67") + ggplot2::labs(title = "Tukey HSD Pairwise Mean Differences", x = "Mean difference with 95% family-wise interval", y = NULL) + standard_theme()
    raw_summary <- stats::aggregate(change_10y ~ NFP_cat + Risk_cat, macro, function(x) c(mean = mean(x), se = stats::sd(x) / sqrt(length(x))))
    raw <- data.frame(NFP_cat = raw_summary$NFP_cat, Risk_cat = raw_summary$Risk_cat, mean = raw_summary$change_10y[, "mean"], se = raw_summary$change_10y[, "se"])
    plots[["Raw NFP-risk interaction"]] <- interaction_mean_plot(raw, "Interaction of NFP and Risk on 10Y Change", "Observed mean change")
    additive <- stats::lm(change_10y ~ NFP_cat + Risk_cat, data = macro)
    grid <- expand.grid(NFP_cat = levels(macro$NFP_cat), Risk_cat = levels(macro$Risk_cat))
    prediction <- stats::predict(additive, newdata = grid, se.fit = TRUE)
    grid$mean <- prediction$fit
    grid$se <- prediction$se.fit
    plots[["Adjusted NFP-risk interaction"]] <- interaction_mean_plot(grid, "Adjusted Effect of NFP and Risk", "Predicted mean change")
  }
  if (method_id == "ancova") {
    raw_level <- stats::aggregate(US10Y ~ NFP_cat + Risk_cat, macro, mean)
    names(raw_level)[3] <- "mean"
    raw_level$se <- 0
    plots[["Yield-level interaction by NFP and Risk"]] <- interaction_mean_plot(raw_level, "10Y Yield Interaction by NFP and Risk", "Mean US OIS 10Y")
    plots[["ANCOVA slope check with uncertainty"]] <- ggplot2::ggplot(macro, ggplot2::aes(Risk, US10Y, color = NFP_cat)) +
      ggplot2::geom_point(alpha = 0.18) + ggplot2::geom_smooth(method = "lm", se = TRUE) +
      ggplot2::labs(title = "ANCOVA Slope Check with Confidence Bands", y = "US OIS 10Y") + standard_theme()
    plots[["ANCOVA parallel-slope comparison"]] <- ggplot2::ggplot(macro, ggplot2::aes(Risk, US10Y, color = NFP_cat)) +
      ggplot2::geom_point(alpha = 0.12) + ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
      ggplot2::labs(title = "Parallel-Slope Assumption Check", y = "US OIS 10Y") + standard_theme()
    plots[["ANCOVA variance distribution"]] <- ggplot2::ggplot(macro, ggplot2::aes(NFP_cat, US10Y, fill = NFP_cat)) +
      ggplot2::geom_violin(alpha = 0.6) + ggplot2::geom_boxplot(width = 0.12, outlier.shape = NA, show.legend = FALSE) +
      ggplot2::labs(title = "Yield Variance Across NFP Regimes", x = "NFP quartile", y = "US OIS 10Y") + standard_theme()
    risk_mean <- mean(macro$Risk)
    fit <- stats::lm(US10Y ~ Risk + NFP_cat, data = macro)
    grid <- data.frame(Risk = risk_mean, NFP_cat = levels(macro$NFP_cat))
    prediction <- stats::predict(fit, newdata = grid, se.fit = TRUE)
    adjusted <- data.frame(NFP_cat = grid$NFP_cat, adjusted = prediction$fit, se = prediction$se.fit)
    plots[["ANCOVA adjusted means"]] <- ggplot2::ggplot(adjusted, ggplot2::aes(NFP_cat, adjusted)) +
      ggplot2::geom_point(size = 3, color = "#C2410C") + ggplot2::geom_errorbar(ggplot2::aes(ymin = adjusted - se, ymax = adjusted + se), width = 0.18) +
      ggplot2::labs(title = "Adjusted Mean of 10Y by NFP Regime", subtitle = "All groups evaluated at the same average Risk level", x = "NFP quartile", y = "Adjusted US OIS 10Y") + standard_theme()
  }
  list(plots = plots, notes = notes[names(plots)])
}

# interaction_mean_plot()
# Role: draw comparable interaction charts from group means and standard errors.
interaction_mean_plot <- function(data, title, y_label) {
  ggplot2::ggplot(data, ggplot2::aes(NFP_cat, mean, color = Risk_cat, group = Risk_cat)) +
    ggplot2::geom_point(size = 3) + ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width = 0.15) +
    ggplot2::labs(title = title, x = "NFP quartile", y = y_label, color = "Risk quartile") + standard_theme()
}

# enrich_case_with_selected_plots()
# Role: apply the CSV decisions, append rebuilt plots, and store their teaching order.
enrich_case_with_selected_plots <- function(method_id, case, data_bundle, selected_requests) {
  covered <- unique(selected_requests$method_id)
  if (!method_id %in% covered) {
    case$visual_sections <- list("Visual Analysis" = names(case$plots))
    return(case)
  }
  current_decisions <- selected_requests[selected_requests$method_id == method_id & selected_requests$source == "Current App", , drop = FALSE]
  if (nrow(current_decisions) > 0) {
    drop_names <- current_decisions$plot_name[!current_decisions$keep]
    case$plots <- case$plots[setdiff(names(case$plots), drop_names)]
    case$plot_notes <- case$plot_notes[names(case$plots)]
  }
  rebuilt <- build_selected_reference_plots(method_id, data_bundle)
  case$plots <- c(case$plots, rebuilt$plots)
  case$plot_notes <- c(case$plot_notes, rebuilt$notes)
  case$visual_sections <- selected_visual_sections(method_id, names(case$plots))
  case
}

# format_display_number()
# Role: keep dashboard numbers readable without changing calculation precision.
format_display_number <- function(value) {
  result <- rep(NA_character_, length(value))
  valid <- !is.na(value)
  scientific <- valid & value != 0 & (abs(value) < 0.001 | abs(value) >= 1000000)
  regular <- valid & !scientific
  result[scientific] <- formatC(value[scientific], format = "e", digits = 2)
  result[regular] <- format(
    round(value[regular], 3),
    scientific = FALSE,
    trim = TRUE,
    drop0trailing = TRUE
  )
  result
}

# format_display_table()
# Role: convert numeric table columns to the shared dashboard format.
format_display_table <- function(table_object) {
  result <- as.data.frame(table_object)
  numeric_columns <- vapply(result, is.numeric, logical(1))
  result[numeric_columns] <- lapply(result[numeric_columns], format_display_number)
  result
}

# render_case_plot_cache()
# Role: render one case to ready PNGs; only Re-run case replaces these files.
render_case_plot_cache <- function(method_id, case, cache_dir) {
  method_dir <- file.path(cache_dir, method_id)
  dir.create(method_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- vapply(seq_along(case$plots), function(index) {
    path <- file.path(method_dir, sprintf("%02d.png", index))
    ggplot2::ggsave(path, case$plots[[index]], width = 10, height = 6.2, dpi = 125)
    path
  }, character(1))
  names(paths) <- names(case$plots)
  paths
}
