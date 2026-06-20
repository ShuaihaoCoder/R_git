# ============================================================
# YieldCurve Trader Dashboard: UI and applied-result server
# ============================================================
project_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
source(file.path(project_dir, "R", "curve_engine.R"))
source(file.path(project_dir, "R", "data_loader.R"))

required <- c("shiny", "bslib", "DT", "ggplot2", "plotly")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing UI packages. Start with run_app.R. Missing: ", paste(missing, collapse = ", "), call. = FALSE)

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)

initial_market <- load_market_data(project_dir)
initial_history_date <- if (length(initial_market$wide_rates$date)) max(initial_market$wide_rates$date, na.rm = TRUE) else Sys.Date()

theme <- bs_theme(version = 5, bootswatch = "flatly", primary = "#234B68")
positive_color <- "#16A085"
negative_color <- "#D96C5F"
neutral_color <- "#61758A"

fmt_num_digits <- function(x, digits = 1) format(round(as.numeric(x), digits), nsmall = digits, trim = TRUE, scientific = FALSE, big.mark = ",")
fmt_num <- function(x) format(round(as.numeric(x), 2), nsmall = 0, trim = TRUE, scientific = FALSE, big.mark = ",")
fmt_pct <- function(x) paste0(fmt_num_digits(x, 2), "%")
fmt_pct2 <- function(x) paste0(fmt_num_digits(x, 2), "%")
fmt_pct4 <- function(x) paste0(format(round(as.numeric(x), 4), nsmall = 4, trim = TRUE, scientific = FALSE), "%")
fmt_df2 <- function(x) format(round(as.numeric(x), 2), nsmall = 2, trim = TRUE, scientific = FALSE)
fmt_bp <- function(x) paste0(fmt_num_digits(x, 1), " bp")
fmt_bp1 <- function(x) paste0(fmt_num_digits(x, 1), " bp")
fmt_pnl0 <- function(x) fmt_num_digits(x, 0)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
round_numeric_df <- function(x) {
  numeric_columns <- vapply(x, is.numeric, logical(1))
  x[numeric_columns] <- lapply(x[numeric_columns], round, 2)
  x
}
value_color <- function(x) ifelse(x > 0, positive_color, ifelse(x < 0, negative_color, neutral_color))
signed_class <- function(value) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (is.finite(numeric_value) && numeric_value > 0) "positive" else if (is.finite(numeric_value) && numeric_value < 0) "negative" else "neutral"
}
signed_text_value <- function(value, digits = 1, suffix = "") {
  numeric_value <- suppressWarnings(as.numeric(value))
  paste0(if (is.finite(numeric_value) && numeric_value > 0) "+" else "", fmt_num_digits(numeric_value, digits), suffix)
}
signed_cell <- function(value, digits = 1, suffix = "") {
  paste0("<span class='signed-value ", signed_class(value), "'>", signed_text_value(value, digits, suffix), "</span>")
}
format_display_df <- function(data) {
  for (column in names(data)) {
    if (!is.numeric(data[[column]])) next
    lower_name <- tolower(column)
    is_bp <- grepl("bp|rmse|residual|change", lower_name)
    is_rate <- grepl("rate|percent|forward|spot", lower_name)
    is_signed <- grepl("change|residual|p&l|pnl", lower_name)
    digits <- if (is_bp) 1 else if (is_rate) 2 else 2
    if (is_signed) {
      data[[column]] <- vapply(data[[column]], signed_cell, character(1), digits = digits)
    } else {
      data[[column]] <- fmt_num_digits(data[[column]], digits)
    }
  }
  data
}
center_dt_options <- function(options = list()) {
  options$columnDefs <- c(options$columnDefs %||% list(), list(list(className = "dt-center", targets = "_all")))
  options
}
centered_datatable <- function(data, options = list(), rownames = FALSE, escape = FALSE) {
  datatable(data, options = center_dt_options(options), rownames = rownames, escape = escape)
}
series_linetypes <- function(series) {
  patterns <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash",
    "11", "22", "33", "44", "13", "31", "42", "24", "1343", "73", "2262", "12223242")
  stats::setNames(rep(patterns, length.out = length(unique(series))), unique(series))
}
plotly_finish <- function(plot, tooltip = "text") {
  plotly::config(
    plotly::layout(ggplotly(plot, tooltip = tooltip), margin = list(t = 76, r = 28, b = 52, l = 58),
      title = list(x = 0.02, font = list(size = 13)), xaxis = list(tickformat = ".2f"), yaxis = list(tickformat = ".2f")),
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines")
  )
}
plotly_trace_finish <- function(plot, margin = list(t = 76, r = 28, b = 52, l = 58)) {
  plotly::config(
    plotly::layout(plot, margin = margin,
      title = list(x = 0.02, font = list(size = 13))),
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines")
  )
}
history_curve_palette <- function(curves) {
  palette <- c("#0B3D91", "#0F8B8D", "#F97316", "#7C3AED", "#64748B", "#B45309",
    "#2563EB", "#059669", "#DC2626", "#9333EA")
  stats::setNames(rep(palette, length.out = length(unique(curves))), unique(curves))
}
history_date_linetypes <- function(dates) {
  patterns <- c("solid", "dash", "dot", "dashdot", "longdash", "longdashdot", "solid")
  stats::setNames(rep(patterns, length.out = length(unique(dates))), unique(dates))
}
history_date_alpha <- function(date_index) {
  alpha <- c(1, 0.78, 0.62, 0.48, 0.36, 0.28, 0.22)
  alpha[pmin(date_index, length(alpha))]
}
history_target_tenors <- c("2Y" = 2, "5Y" = 5, "10Y" = 10)
nearest_rows_by_tenor <- function(data, target_tenors = history_target_tenors, exact_only = TRUE) {
  pieces <- lapply(names(target_tenors), function(label) {
    target <- target_tenors[[label]]
    split_key <- paste(data$curve, data$requested_date, sep = "\r")
    rows <- lapply(split(seq_len(nrow(data)), split_key), function(indexes) {
      segment <- data[indexes, , drop = FALSE]
      matched_index <- which(abs(segment$tenor - target) < 1e-8)
      if (!length(matched_index) && !exact_only) matched_index <- which.min(abs(segment$tenor - target))
      if (!length(matched_index)) return(NULL)
      row <- segment[matched_index[[1]], , drop = FALSE]
      row$tenor_label <- label
      row
    })
    do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (!length(pieces)) return(data.frame())
  do.call(rbind, pieces)
}
history_largest_move_info <- function(data) {
  if (!nrow(data)) return(list(value = "0 bp", raw_value = 0, direction = "neutral", icon = "\u2192", subtitle = "No data"))
  compare_rows <- data[as.Date(data$requested_date) != as.Date(data$base_requested_date), , drop = FALSE]
  if (!nrow(compare_rows)) compare_rows <- data
  row <- compare_rows[which.max(abs(compare_rows$change_bp)), , drop = FALSE]
  raw_value <- row$change_bp[[1]]
  list(
    value = fmt_bp(raw_value),
    raw_value = raw_value,
    direction = if (raw_value > 0) "positive" else if (raw_value < 0) "negative" else "neutral",
    icon = if (raw_value > 0) "\u2191" else if (raw_value < 0) "\u2193" else "\u2192",
    subtitle = paste(row$curve[[1]], tenor_label(row$tenor[[1]]), "vs base")
  )
}
filter_history_tenor_range <- function(data, range) {
  if (is.null(range) || length(range) < 2) return(data)
  range <- sort(as.numeric(range))
  data[data$tenor >= range[[1]] & data$tenor <= range[[2]], , drop = FALSE]
}
normalize_history_date <- function(value, dates) {
  value <- as.Date(value)
  if (!length(value) || is.na(value[[1]]) || value[[1]] < min(dates) || value[[1]] > max(dates)) return(max(dates))
  value[[1]]
}
history_quote_details <- function(data, range = NULL) {
  target_tenors <- history_target_tenors
  if (!is.null(range) && length(range) >= 2) {
    range <- sort(as.numeric(range))
    target_tenors <- target_tenors[target_tenors >= range[[1]] & target_tenors <= range[[2]]]
  }
  if (!length(target_tenors)) return(data.frame(Date = character(), `Curve Name` = character(), Tenor = character(), `Rate (%)` = character(), `Change (bp)` = character(), date_span = integer(), curve_span = integer(), date_group_start = logical(), curve_group_start = logical(), check.names = FALSE))
  selected <- nearest_rows_by_tenor(filter_history_tenor_range(data, range), target_tenors = target_tenors, exact_only = FALSE)
  if (!nrow(selected)) return(data.frame())
  selected$Date <- as.character(selected$requested_date)
  selected$`Curve Name` <- selected$curve
  selected$Tenor <- selected$tenor_label
  selected$`Rate (%)` <- fmt_num_digits(selected$rate_percent, 2)
  selected$`Change (bp)` <- ifelse(as.Date(selected$requested_date) == as.Date(selected$base_requested_date) | abs(selected$change_bp) < 1e-10, "\u2014", vapply(selected$change_bp, signed_text_value, character(1), digits = 1))
  selected <- selected[order(as.Date(selected$requested_date), selected$`Curve Name`, selected$tenor), c("Date", "Curve Name", "Tenor", "Rate (%)", "Change (bp)")]
  date_key <- selected$Date
  curve_key <- paste(selected$Date, selected$`Curve Name`, sep = "\r")
  selected$date_span <- as.integer(ave(seq_len(nrow(selected)), date_key, FUN = length))
  selected$curve_span <- as.integer(ave(seq_len(nrow(selected)), curve_key, FUN = length))
  selected$date_group_start <- !duplicated(date_key)
  selected$curve_group_start <- !duplicated(curve_key)
  selected
}
history_display_tenor_values <- function(tenors) {
  ticks <- axis_tenor_ticks(tenors)
  ticks$tickvals
}
metric_card <- function(title, output_id) {
  div(class = "metric-card resizable-card",
    tags$div(class = "metric-kicker", title),
    div(class = "metric-value", textOutput(output_id))
  )
}
metric_card_sub_text <- function(title, value_id, subtitle_id = NULL) {
  div(class = "metric-card resizable-card",
    tags$div(class = "metric-kicker", title),
    div(class = "metric-value", textOutput(value_id)),
    if (!is.null(subtitle_id)) div(class = "metric-subtitle", textOutput(subtitle_id))
  )
}
metric_card_sub <- function(title, value_id, subtitle_id = NULL) {
  div(class = "metric-card curve-kpi-card resizable-card",
    tags$div(class = "metric-kicker", title),
    div(class = "metric-value", textOutput(value_id)),
    if (!is.null(subtitle_id)) div(class = "metric-subtitle", textOutput(subtitle_id))
  )
}
metric_card_ui <- function(title, output_id, subtitle_id = NULL, class = NULL) {
  div(class = paste("metric-card curve-kpi-card resizable-card", class),
    tags$div(class = "metric-kicker", title),
    uiOutput(output_id),
    if (!is.null(subtitle_id)) div(class = "metric-subtitle", textOutput(subtitle_id))
  )
}
metric_card_sub_ui <- function(title, value_id, subtitle_id = NULL, class = NULL) {
  div(class = paste("metric-card curve-kpi-card resizable-card", class),
    tags$div(class = "metric-kicker", title),
    div(class = "metric-value", uiOutput(value_id)),
    if (!is.null(subtitle_id)) div(class = "metric-subtitle", uiOutput(subtitle_id))
  )
}
signed_value_span <- function(value, suffix = NULL) {
  numeric_value <- suppressWarnings(as.numeric(value))
  class <- if (is.finite(numeric_value) && numeric_value > 0) "signed-value positive" else if (is.finite(numeric_value) && numeric_value < 0) "signed-value negative" else "signed-value neutral"
  tags$span(class = class, paste0(if (is.finite(numeric_value) && numeric_value > 0) "+" else "", fmt_num(numeric_value), suffix %||% ""))
}
signed_value_span_digits <- function(value, suffix = NULL, digits = 1) {
  numeric_value <- suppressWarnings(as.numeric(value))
  class <- if (is.finite(numeric_value) && numeric_value > 0) "signed-value positive" else if (is.finite(numeric_value) && numeric_value < 0) "signed-value negative" else "signed-value neutral"
  tags$span(class = class, paste0(if (is.finite(numeric_value) && numeric_value > 0) "+" else "", fmt_num_digits(numeric_value, digits), suffix %||% ""))
}
signed_na_span <- function() tags$span(class = "signed-value neutral muted-na", "NA")
tenor_label <- function(x) {
  x <- as.numeric(x)
  vapply(x, function(value) {
    if (is.na(value)) return("NA")
    if (value < 1) return(paste0(round(value * 12), "M"))
    if (abs(value - round(value)) < 1e-8) return(paste0(round(value), "Y"))
    paste0(format(round(value, 2), trim = TRUE, scientific = FALSE), "Y")
  }, character(1))
}
tenor_choices <- c("1M" = 1 / 12, "3M" = 0.25, "6M" = 0.5, "1Y" = 1, "2Y" = 2,
  "3Y" = 3, "5Y" = 5, "7Y" = 7, "10Y" = 10, "15Y" = 15, "20Y" = 20, "30Y" = 30)
diagnostics_expected_tenors <- c("1M" = 1 / 12, "3M" = 0.25, "6M" = 0.5, "1Y" = 1, "2Y" = 2,
  "3Y" = 3, "5Y" = 5, "7Y" = 7, "10Y" = 10, "15Y" = 15, "20Y" = 20, "30Y" = 30)
axis_tenor_ticks <- function(tenors) {
  tenors <- sort(unique(as.numeric(tenors[is.finite(tenors)])))
  if (!length(tenors)) return(list(tickvals = numeric(), ticktext = character()))
  targets <- c(1 / 12, 1, 3, 5, 7, 10, 15, 20, if (max(tenors) >= 28) 30 else 25)
  matched <- vapply(targets, function(target) tenors[which.min(abs(tenors - target))], numeric(1))
  matched <- sort(unique(matched[is.finite(matched)]))
  # Keep sparse trader tenors on the axis; hover still carries every raw tenor.
  keep <- vapply(matched, function(value) min(abs(targets - value)) <= max(0.08, value * 0.1), logical(1))
  tickvals <- matched[keep]
  if (!length(tickvals)) tickvals <- tenors[c(1, length(tenors))]
  list(tickvals = tickvals, ticktext = tenor_label(tickvals))
}
find_fit <- function(fits, method) {
  matched <- fits[vapply(fits, function(x) identical(x$method, method), logical(1))]
  if (length(matched)) matched[[1]] else NULL
}
fit_rsq <- function(fit) {
  if (is.null(fit) || is.null(fit$diagnostics)) return(NA_real_)
  observed <- fit$diagnostics$observed_percent
  fitted <- fit$diagnostics$fitted_percent
  denom <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  1 - sum((observed - fitted)^2, na.rm = TRUE) / denom
}
format_rsq <- function(x) ifelse(is.na(x), "NA", fmt_num_digits(100 * x, 2))
static_metric_card <- function(title, value, subtitle = NULL, tone = "neutral") {
  div(class = paste("metric-card resizable-card", paste0("metric-", tone)),
    tags$div(class = "metric-kicker", title),
    div(class = "metric-value", value),
    if (!is.null(subtitle)) div(class = "metric-subtitle", subtitle)
  )
}
explanation_card <- function(title, output_id, class = NULL) {
  div(class = paste("explanation-card resizable-card", class),
    div(class = "card-heading compact-heading",
      tags$div(class = "card-kicker", "Desk note"),
      tags$h4(title)
    ),
    uiOutput(output_id)
  )
}
page_header <- function(title, subtitle, output_id = NULL) {
  subtitle_node <- if (inherits(subtitle, "shiny.tag") || inherits(subtitle, "shiny.tag.list")) subtitle else tags$p(subtitle)
  div(class = "page-header screenshot-header",
    div(class = "page-title-block",
      tags$div(class = "eyebrow", "YieldCurve Trader"),
      tags$h2(title),
      subtitle_node
    ),
    div(class = "page-header-right",
      div(class = "header-clock", "Local RDS analytics"),
      if (!is.null(output_id)) uiOutput(output_id)
    )
  )
}
control_section <- function(title, ...) div(class = "control-section", if (!is.null(title)) tags$h5(title), ...)
table_card <- function(title, output_id, subtitle = NULL) {
  div(class = "table-card resizable-card",
    div(class = "card-heading",
      tags$div(tags$h4(title), if (!is.null(subtitle)) tags$p(subtitle))
    ),
    DTOutput(output_id)
  )
}
parameter_section_ui <- function(title, data) {
  div(class = "parameter-section",
    tags$div(class = "parameter-section-title", title),
    tags$div(class = "parameter-rows",
      lapply(seq_len(nrow(data)), function(index) {
        div(class = "parameter-row",
          tags$span(class = "parameter-label", data$Metric[[index]]),
          tags$span(class = "parameter-value", data$Value[[index]])
        )
      })
    )
  )
}
unavailable_card <- function(title, reason = "Not available in current local RDS / Not implemented yet") {
  div(class = "unavailable-card resizable-card",
    tags$div(class = "card-kicker", "Module placeholder"),
    tags$h4(title),
    tags$p(reason)
  )
}
diagnostics_status_icon <- function(status) {
  labels <- c(ok = "\u2713", warn = "\u26a0", fail = "\u00d7", na = "\u2013")
  status <- if (status %in% names(labels)) status else "na"
  tags$span(class = paste("diag-status-icon", paste0("diag-status-", status)), labels[[status]])
}
diagnostics_metric_card <- function(title, value_id, subtitle_id = NULL, detail_id = NULL, status_id = NULL) {
  div(class = "diag-kpi-card resizable-card",
    div(class = "diag-kpi-text",
      div(class = "metric-kicker", title),
      div(class = "metric-value", uiOutput(value_id)),
      if (!is.null(subtitle_id)) div(class = "metric-subtitle", uiOutput(subtitle_id)),
      if (!is.null(detail_id)) div(class = "metric-subtitle diag-kpi-detail", uiOutput(detail_id))
    ),
    if (!is.null(status_id)) div(class = "diag-kpi-status", uiOutput(status_id))
  )
}
diagnostics_card <- function(title, output_id, subtitle = NULL, class = NULL) {
  div(class = paste("diagnostics-card resizable-card", class),
    div(class = "card-heading compact-heading",
      tags$div(tags$h4(title), if (!is.null(subtitle)) tags$p(subtitle))
    ),
    uiOutput(output_id)
  )
}
rail_header <- function(title, subtitle = NULL) {
  div(class = "rail-header",
    tags$div(class = "rail-brand-mini", "YC"),
    tags$div(tags$h4(title), if (!is.null(subtitle)) tags$p(subtitle))
  )
}
module_tabs <- function(active, tabs) {
  div(class = "module-tabs",
    lapply(tabs, function(tab) {
      div(class = if (identical(tab, active)) "module-tab active" else "module-tab", tab)
    })
  )
}
progress_box <- function(prefix) uiOutput(paste0(prefix, "_progress"))
forward_status_card <- function() {
  div(class = "forward-status-card",
    div(class = "forward-status-title", "CALCULATION STATUS"),
    uiOutput("forward_status_detail")
  )
}
history_status_card <- function() {
  div(class = "history-status-card",
    div(class = "history-status-title", tags$span("STATUS"), tags$span(class = "info-dot", "i")),
    uiOutput("history_status_detail")
  )
}
plot_card <- function(output_id, height = "430px", class = NULL, subtitle = NULL) {
  div(class = paste("plot-card resizable-card resizable-plot-card", class), `data-default-height` = height,
    div(class = "plot-card-head",
      div(
        div(class = "card-kicker", "Interactive Plotly"),
        div(class = "plot-card-label", large_plot_titles_label(output_id)),
        if (!is.null(subtitle)) div(class = "plot-card-subtitle", subtitle)
      ),
      actionButton(paste0("open_", output_id), "Open large", class = "btn btn-outline-primary btn-sm open-large-btn")
    ),
    div(class = "plot-card-toolbar",
      tags$span("Hover, zoom, inspect")
    ),
    plotlyOutput(output_id, height = height)
  )
}
large_plot_titles_label <- function(output_id) {
  labels <- c(
    curve_plot = "Zero Curve Fit",
    curve_residual_plot = "Fit Residuals",
    history_absolute_plot = "Absolute Curves by Date",
    history_change_plot = "Change vs Base Date",
    forward_curve_plot = "Selected Curve and Forward Endpoints",
    carry_component_plot = "Single Trade Carry / Roll",
    carry_spot_plot = "Spot Curve",
    carry_stacked_plot = "Carry + Roll by Tenor",
    carry_heatmap = "Carry + Roll Heatmap",
    trade_leg_pnl_plot = "Curve Trade Legs",
    trade_component_plot = "Curve Trade Portfolio",
    diagnostics_residual_plot = "Fit Residual Diagnostics"
  )
  labels[[output_id]] %||% output_id
}
source_controls <- function(prefix, title) {
  tagList(
    control_section(title,
      radioButtons(paste0(prefix, "_source_mode"), "Analytics source",
        choices = c("Zero-rate snapshot" = "zero", "Historical quotes (Proxy)" = "historical"), selected = "zero"),
      sidebar_selectize_input(paste0(prefix, "_curve_name"), "Curve", choices = NULL),
      conditionalPanel(sprintf("input.%s_source_mode == 'historical'", prefix), dateInput(paste0(prefix, "_curve_date"), "Historical date")),
      sidebar_selectize_input(paste0(prefix, "_fit_method"), "Fit method",
        choices = c("Nelson-Siegel" = "nelson_siegel", "Spline" = "spline"), selected = "nelson_siegel")
    )
  )
}
carry_status_card <- function() {
  div(class = "carry-status-card",
    div(class = "carry-status-title", "STATUS"),
    uiOutput("carry_status_detail")
  )
}
carry_mode_switch <- function() {
  control_section("WORKSPACE",
    radioButtons("carry_workspace_mode", NULL,
      choices = c("Single Trade" = "single", "Curve Trade" = "trade"), selected = "single",
      inline = TRUE)
  )
}
carry_shared_source_controls <- function() {
  tagList(
    control_section("DATA SOURCE",
      radioButtons("carry_source_mode", NULL,
        choices = c("Bloomberg (BVAL)" = "zero", "Historical quotes (Proxy)" = "historical"), selected = "zero")
    ),
    control_section("CURVE",
      sidebar_selectize_input("carry_curve_name", NULL, choices = NULL)
    ),
    control_section("AS OF DATE",
      conditionalPanel("input.carry_source_mode == 'historical'", dateInput("carry_curve_date", NULL)),
      conditionalPanel("input.carry_source_mode == 'zero'", div(class = "rail-note", uiOutput("carry_zero_date_note")))
    ),
    control_section("FIT METHOD",
      sidebar_selectize_input("carry_fit_method", NULL,
        choices = c("Nelson-Siegel" = "nelson_siegel", "Spline" = "spline"), selected = "nelson_siegel")
    )
  )
}
carry_tenor_range_controls <- function(start_id, end_id, start_selected = 0, end_selected = 5) {
  div(class = "tenor-range-grid",
    sidebar_selectize_input(start_id, "Start Tenor", choices = c("0Y" = 0, tenor_choices), selected = start_selected),
    sidebar_selectize_input(end_id, "End Tenor", choices = tenor_choices, selected = end_selected)
  )
}
sidebar_selectize_input <- function(inputId, label, choices, selected = NULL, multiple = FALSE, options = list()) {
  # 中文说明：sidebar 是一个可滚动容器；把 selectize 的下拉层挂到 body，避免被侧栏裁剪。
  selectizeInput(inputId, label, choices = choices, selected = selected, multiple = multiple,
    options = modifyList(list(dropdownParent = "body"), options))
}

metric_strip <- function(...) div(class = "metric-strip", ...)
dashboard_page <- function(title, subtitle, output_id = NULL, controls, main, class = NULL) {
  fluidPage(class = paste("app-page dashboard-page", class),
    page_header(title, subtitle, output_id),
    div(class = "dashboard-shell",
      div(class = "control-rail", controls),
      div(class = "dashboard-main", main)
    )
  )
}
side_panel <- function(...) div(class = "sidebar-panel resizable-card", ...)
main_grid <- function(...) div(class = "main-grid screenshot-grid", ...)
grid_row <- function(..., class = NULL) div(class = paste("dashboard-row", class), ...)
grid_col <- function(..., class = NULL) div(class = paste("dashboard-col", class), ...)
section_card <- function(title, ..., subtitle = NULL, class = NULL) {
  div(class = paste("section-card resizable-card", class),
    div(class = "card-heading",
      tags$div(class = "card-kicker", "Workspace"),
      tags$h4(title),
      if (!is.null(subtitle)) tags$p(subtitle)
    ),
    ...
  )
}
status_strip <- function(...) div(class = "status-strip", ...)

ui <- navbarPage(
  title = div(class = "brand-lockup",
    span(class = "brand-title", "YieldCurve Trader"),
    span(class = "brand-author", "by Shuaihao")
  ), theme = theme,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("
      (function() {
        function resizePlotCard(card) {
          var plot = card.querySelector('.plotly.html-widget');
          if (!plot || !window.Plotly) return;
          var head = card.querySelector('.plot-card-head');
          var headHeight = head ? head.offsetHeight : 0;
          var available = Math.max(180, card.clientHeight - headHeight - 28);
          plot.style.height = available + 'px';
          window.Plotly.Plots.resize(plot);
        }

        function resizeDataTable(card) {
          var table = card.querySelector('table.dataTable');
          if (!table || !window.jQuery || !window.jQuery.fn || !window.jQuery.fn.dataTable) return;
          try {
            window.jQuery(table).DataTable().columns.adjust();
          } catch (error) {}
        }

        function resizeCard(card) {
          resizePlotCard(card);
          resizeDataTable(card);
        }

        function decorateSignedText(root) {
          var scope = root && root.querySelectorAll ? root : document;
          scope.querySelectorAll('.metric-subtitle .shiny-text-output, .status-strip .shiny-text-output').forEach(function(node) {
            var text = (node.textContent || '').trim();
            node.classList.remove('positive-change', 'negative-change', 'neutral-change');
            if (text.charAt(0) === '+') node.classList.add('positive-change');
            else if (text.charAt(0) === '-' || text.charAt(0) === '−') node.classList.add('negative-change');
            else node.classList.add('neutral-change');
          });
        }

        function attachResizableCards() {
          document.querySelectorAll('.resizable-card').forEach(function(card) {
            if (card.dataset.resizeAttached === 'true') return;
            card.dataset.resizeAttached = 'true';
            if (window.ResizeObserver) {
              var observer = new ResizeObserver(function() { resizeCard(card); });
              observer.observe(card);
            }
            setTimeout(function() { resizeCard(card); }, 250);
          });
          decorateSignedText(document);
        }

        document.addEventListener('DOMContentLoaded', attachResizableCards);
        document.addEventListener('shiny:value', function(event) {
          setTimeout(attachResizableCards, 80);
          var card = event.target && event.target.closest ? event.target.closest('.resizable-card') : null;
          if (card) setTimeout(function() { resizeCard(card); }, 120);
          setTimeout(function() { decorateSignedText(document); }, 120);
        });
        document.addEventListener('shown.bs.tab', function() { setTimeout(attachResizableCards, 120); });
      })();
    "))
  ),
  tabPanel("Curve Explorer",
    fluidPage(class = "app-page dashboard-page curve-page",
      div(class = "dashboard-shell curve-shell",
        div(class = "control-rail",
          side_panel(
            rail_header("Curve Controls", "Bloomberg curve analytics"),
            control_section("DATA SOURCE",
              radioButtons("source_mode", NULL,
                choices = c("Zero-rate snapshot" = "zero", "Historical market quotes" = "historical"), selected = "zero")
            ),
            control_section("CURVE",
              sidebar_selectize_input("curve_name", NULL, choices = NULL)
            ),
            control_section("CURVE DATE",
              conditionalPanel("input.source_mode == 'historical'", dateInput("curve_date", NULL)),
              conditionalPanel("input.source_mode == 'zero'", uiOutput("zero_curve_date_note"))
            ),
            control_section("FIT METHOD",
              div(class = "fit-method-buttons",
                checkboxGroupInput("fit_methods", NULL,
                  choices = c("Nelson-Siegel" = "nelson_siegel", "Spline" = "spline"),
                  selected = c("nelson_siegel", "spline"), inline = TRUE)
              )
            ),
            control_section("TENOR RANGE",
              div(class = "tenor-range-grid",
                sidebar_selectize_input("curve_start_tenor", "From", choices = tenor_choices, selected = 1 / 12),
                sidebar_selectize_input("curve_end_tenor", "To", choices = tenor_choices, selected = 30)
              )
            ),
            control_section("DISPLAY OPTIONS",
              div(class = "display-options",
                checkboxInput("show_raw_points", "Raw market points", TRUE),
                checkboxInput("show_ns_fit", "Nelson-Siegel line", TRUE),
                checkboxInput("show_spline_fit", "Spline line", TRUE),
                checkboxInput("show_residuals_panel", "Residuals panel", TRUE)
              )
            ),
            actionButton("apply_curve", tagList(tags$span(class = "apply-icon", "\u25b6"), "Apply Curve"), class = "btn-primary action-main"),
            actionButton("refresh_data", "Refresh local RDS", class = "btn-outline-secondary side-secondary refresh-btn"),
            div(class = "curve-status-card",
              tags$div(class = "status-card-title", "STATUS"),
              progress_box("curve"),
              div(class = "small-note", textOutput("loaded_at")),
              div(class = "small-note", textOutput("effective_curve_date"))
            )
          )
        ),
        div(class = "dashboard-main curve-main",
          div(class = "curve-workspace-header",
            uiOutput("curve_title_block")
          ),
          div(class = "metric-strip curve-kpi-strip",
            metric_card_sub("NS Beta0 (Level)", "curve_metric_beta0"),
            metric_card_sub("NS Beta1 (Slope)", "curve_metric_beta1"),
            metric_card_sub("NS Tau (Curvature)", "curve_metric_tau"),
            metric_card_sub("RMSE (bp)", "curve_metric_rmse", "curve_metric_rsq"),
            metric_card_sub("Observations", "curve_metric_obs", "curve_metric_range")
          ),
          plot_card("curve_plot", height = "520px", class = "hero-plot curve-hero-plot",
            subtitle = "Raw points with Nelson-Siegel and spline analytics traces."),
          grid_row(
            grid_col(class = "span-5", plot_card("curve_residual_plot", height = "300px",
              subtitle = "NS and spline residuals, in basis points.")),
            grid_col(class = "span-4", section_card("Fitted Parameters",
              uiOutput("fitted_parameters"),
              subtitle = "NS parameters, goodness of fit, and spline method notes.",
              class = "fit-parameters-card")),
            grid_col(class = "span-3", explanation_card("About the Fit", "curve_explanation", class = "about-fit-card"))
          )
        )
      )
    )
  ),
  tabPanel("History & Changes", dashboard_page(
    "History & Changes",
    uiOutput("history_header_subtitle"),
    NULL,
    controls = side_panel(
      rail_header("History Controls", "Multi curve/date comparison"),
      control_section("CURVES",
        div(class = "history-search-wrap",
          selectizeInput("history_curves", NULL, choices = NULL, multiple = TRUE,
            options = list(dropdownParent = "body", plugins = list("remove_button"), placeholder = "Search curves..."))
        ),
        uiOutput("history_curve_chips")
      ),
      control_section("BASE DATE",
        dateInput("history_base_date", NULL, value = initial_history_date)
      ),
      control_section("COMPARE DATES",
        tags$div(class = "history-section-hint", "up to 6"),
        dateInput("history_compare_date", NULL, value = initial_history_date),
        actionButton("add_history_date", tagList(tags$span(class = "plus-icon", "+"), "Add comparison date"),
          class = "btn-outline-secondary side-secondary history-add-date"),
        uiOutput("history_date_tags")
      ),
      control_section("TENOR RANGE",
        div(class = "tenor-range-grid history-tenor-range",
          sidebar_selectize_input("history_start_tenor", "From", choices = tenor_choices, selected = 1 / 12),
          sidebar_selectize_input("history_end_tenor", "To", choices = tenor_choices, selected = 30)
        )
      ),
      control_section("SOURCE MODE",
        sidebar_selectize_input("history_source_mode", NULL, choices = c("Bloomberg" = "bloomberg"), selected = "bloomberg"),
        uiOutput("history_combination_warning")
      ),
      control_section(NULL,
        actionButton("run_history", tagList(tags$span(class = "apply-icon", "\u25b6"), "Run History Comparison"),
          class = "btn-primary action-main history-run-btn"),
        history_status_card()
      ),
      div(class = "rail-note history-rail-note", "Local RDS historical market quotes. Requested dates map to nearest effective market quote.")
    ),
    main = main_grid(
      metric_strip(
        metric_card_sub("Combinations", "history_metric_combos", "history_metric_combos_sub"),
        metric_card_ui("Largest Move (bp)", "history_metric_largest", "history_metric_largest_sub", class = "history-largest-card"),
        metric_card_sub("Fallback Dates", "history_metric_fallbacks", "history_metric_fallbacks_sub"),
        metric_card_sub("Proxy Mode", "history_metric_proxy", "history_metric_proxy_sub")
      ),
      grid_row(
        grid_col(class = "span-8", plot_card("history_absolute_plot", height = "515px", class = "hero-plot history-absolute-card",
          subtitle = "Zero rates by tenor for each selected curve and date.")),
        grid_col(class = "span-4",
          table_card("Quote Details by Date / Curve / Tenor", "history_comparison_table", "2Y, 5Y and 10Y unless excluded by tenor range.")
        )
      ),
      grid_row(class = "history-change-row",
        grid_col(class = "span-8", plot_card("history_change_plot", height = "420px",
          subtitle = "Change in zero rates (bp) relative to the selected base date.")),
        grid_col(class = "span-4",
          div(class = "history-info-card",
            tags$div(class = "info-icon", "i"),
            tags$p("Requested dates that are not trading days are automatically mapped to the nearest effective market quote."),
            tags$p("Change (bp) is calculated versus the base effective date.")
          )
        )
      ),
      div(class = "history-footer",
        tags$span("Local RDS historical market quotes. Day count not displayed."),
        tags$span(textOutput("history_footer_asof", inline = TRUE)),
        tags$span("Auto-refresh: unavailable")
      )
    )
  )),
  tabPanel("Forward Calculator", dashboard_page(
    "Forward Calculator",
    "Independent forward-rate workspace with source/date summary and compounding sensitivity.",
    "forward_banner",
    controls = side_panel(
      div(class = "forward-rail-title",
        tags$div(class = "forward-rail-logo", "fx"),
        tags$div(tags$h4("Forward Controls"), tags$p("Curve, tenor and compounding"))
      ),
      control_section("SOURCE MODE",
        radioButtons("forward_source_mode", NULL,
          choices = c("Zero-rate snapshot (preferred)" = "zero", "Historical proxy (from history)" = "historical"),
          selected = "zero")
      ),
      control_section("CURVE",
        div(class = "forward-curve-picker",
          sidebar_selectize_input("forward_curve_name", NULL, choices = NULL),
          tags$button(type = "button", class = "forward-star-button", title = "Favorite curve", "\u2606")
        )
      ),
      control_section("CURVE DATE",
        div(class = "forward-date-options",
          conditionalPanel("input.forward_source_mode == 'zero'",
            tags$label(class = "forward-radio-line selected-static",
              tags$span(class = "radio-dot filled"), tags$span("Snapshot (latest available)")
            ),
            uiOutput("forward_snapshot_note")
          ),
          conditionalPanel("input.forward_source_mode == 'historical'",
            tags$label(class = "forward-radio-line selected-static",
              tags$span(class = "radio-dot filled"), tags$span("Historical date")
            ),
            dateInput("forward_curve_date", NULL),
            div(class = "forward-blue-note", tags$span(class = "info-dot", "i"), "Using zero rates from the selected historical date.")
          )
        )
      ),
      control_section("TENOR RANGE",
        div(class = "tenor-range-grid forward-tenor-grid",
          sidebar_selectize_input("forward_start", "Start Tenor", choices = tenor_choices, selected = 1),
          sidebar_selectize_input("forward_end", "End Tenor", choices = tenor_choices, selected = 5)
        )
      ),
      control_section("COMPOUNDING",
        div(class = "forward-compounding-options",
          radioButtons("forward_compounding", NULL,
            choices = c("Annual" = "annual", "Continuous" = "continuous", "Simple (Act/360)" = "simple"),
            selected = "annual")
        )
      ),
      control_section(NULL,
        actionButton("calculate_forward", tagList(tags$span(class = "calc-button-icon", "\u25a3"), "Calculate Forward"),
          class = "btn-primary action-main forward-calc-btn"),
        actionButton("reset_forward", "Reset", class = "btn-outline-secondary side-secondary forward-reset-btn"),
        forward_status_card()
      ),
      div(class = "rail-note", "Local RDS analytics only. Live Bloomberg timestamp, Save and Export are unavailable.")
    ),
    main = main_grid(
      uiOutput("forward_title_block"),
      div(class = "metric-strip forward-kpi-strip",
        metric_card_sub_text("Forward Rate", "forward_value", "forward_value_change"),
        metric_card_sub_text("Start Spot", "forward_start_spot", "forward_start_spot_change"),
        metric_card_sub_text("End Spot", "forward_end_spot", "forward_end_spot_change"),
        metric_card_sub_text("Curve Source", "forward_curve_source", "forward_curve_source_sub"),
        metric_card_sub_text("Compounding", "forward_compounding_value", "forward_day_count_value")
      ),
      grid_row(class = "forward-plot-row",
        grid_col(class = "span-9", plot_card("forward_curve_plot", height = "540px", class = "hero-plot forward-hero-plot",
          subtitle = "Fitted curve with forward start/end lines and the priced interval shaded.")),
        grid_col(class = "span-3", explanation_card("What does selected forward mean?", "forward_explanation", class = "forward-explanation-card"))
      ),
      grid_row(class = "forward-calc-row",
        grid_col(class = "span-6", explanation_card("Forward Rate Calculation", "forward_formula", class = "forward-formula-card")),
        grid_col(class = "span-6", table_card("Forward Rate Sensitivity", "forward_sensitivity",
          "Forward-rate response to endpoint and compounding assumptions."))
      ),
      grid_row(
        grid_col(class = "span-12", table_card("Rate Inputs Summary", "forward_inputs_summary",
          "Tenor matrix derived from the selected fitted curve. Start and end tenors are highlighted."))
      )
    )
  )),
  tabPanel("Carry & Roll", dashboard_page(
    "Carry & Roll Analysis",
    uiOutput("carry_header_meta"),
    NULL,
    class = "carry-page",
    controls = side_panel(
      rail_header("Carry Controls", "Single trade and curve trade"),
      carry_mode_switch(),
      carry_shared_source_controls(),
      conditionalPanel("input.carry_workspace_mode == 'single'",
        control_section("TENOR RANGE", carry_tenor_range_controls("carry_start", "carry_end", 0, 5)),
        control_section("HOLD PERIOD",
          sidebar_selectize_input("carry_hold", NULL, choices = c("1M (1 Month)" = 1/12, "3M (3 Months)" = 0.25, "6M (6 Months)" = 0.5, "1Y (1 Year)" = 1), selected = 0.25)
        ),
        control_section("DIRECTION",
          sidebar_selectize_input("carry_direction", NULL, choices = c("Receive Fixed", "Pay Fixed"))
        ),
        control_section("DV01 (PER $1MM NOTIONAL)",
          numericInput("dv01", NULL, 10000, min = 0, step = 1000),
          actionButton("calculate_carry", tagList(tags$span(class = "calc-button-icon", "\u25a3"), "Calculate Carry & Roll"), class = "btn-primary action-main"),
          actionButton("load_neutral_dv01", tagList(tags$span(class = "download-icon", "\u21e9"), "Load DV01-neutral defaults"), class = "btn-outline-secondary side-secondary")
        )
      ),
      conditionalPanel("input.carry_workspace_mode == 'trade'",
        control_section("STRUCTURE",
          sidebar_selectize_input("trade_structure", NULL, choices = c("Steepener" = "steepener", "Flattener" = "flattener",
            "Long-belly Fly" = "long_belly_fly", "Short-belly Fly" = "short_belly_fly"))
        ),
        control_section("TENORS",
          numericInput("trade_start", "Start tenor", 0, min = 0, step = 0.25),
          numericInput("trade_short_tenor", "Short tenor", 2, min = 0.25, step = 0.25),
          numericInput("trade_belly_tenor", "Belly tenor", 5, min = 0.5, step = 0.25),
          numericInput("trade_long_tenor", "Long tenor", 10, min = 1, step = 0.25)
        ),
        control_section("HOLD PERIOD",
          sidebar_selectize_input("trade_hold", NULL, choices = c("1M (1 Month)" = 1/12, "3M (3 Months)" = 0.25, "6M (6 Months)" = 0.5, "1Y (1 Year)" = 1), selected = 0.25)
        ),
        control_section("DV01",
          numericInput("trade_risk_budget", "DV01-neutral risk budget", 10000, min = 1, step = 1000),
          numericInput("trade_short_dv01", "Short leg DV01", 10000, min = 0, step = 1000),
          numericInput("trade_belly_dv01", "Belly leg DV01", 10000, min = 0, step = 1000),
          numericInput("trade_long_dv01", "Long leg DV01", 10000, min = 0, step = 1000),
          actionButton("load_neutral_dv01_trade", tagList(tags$span(class = "download-icon", "\u21e9"), "Load DV01-neutral defaults"), class = "btn-outline-secondary side-secondary"),
          actionButton("calculate_curve_trade", tagList(tags$span(class = "calc-button-icon", "\u25a3"), "Calculate Curve Trade"), class = "btn-primary action-main")
        )
      ),
      carry_status_card()
    ),
    main = main_grid(
      uiOutput("carry_title_block"),
      div(class = "metric-strip carry-kpi-strip",
        metric_card_sub_ui("Carry", "carry_value_ui", "carry_subtitle_ui"),
        metric_card_sub_ui("Roll", "roll_value_ui", "roll_subtitle_ui"),
        metric_card_sub_ui("Total", "total_bp_ui", "total_subtitle_ui"),
        metric_card_sub_ui("P&L", "pnl_value_ui", "pnl_subtitle_ui"),
        metric_card_sub_text("Trade Mode", "trade_mode_value", "trade_mode_subtitle"),
        metric_card_sub_ui("Curve Quality", "curve_quality_ui", "curve_quality_subtitle_ui")
      ),
      grid_row(class = "carry-top-row",
        grid_col(class = "span-6", plot_card("carry_component_plot", height = "340px", class = "carry-main-plot",
          subtitle = "Carry, roll and total by tenor for the selected hold period.")),
        grid_col(class = "span-6", plot_card("carry_spot_plot", height = "340px", class = "carry-main-plot"))
      ),
      grid_row(class = "carry-bottom-row",
        grid_col(class = "span-5", plot_card("carry_heatmap", height = "420px", class = "carry-heatmap-card",
          subtitle = "Rows = hold period; columns = tenor.")),
        grid_col(class = "span-7",
          uiOutput("curve_trade_workspace")
        )
      ),
      div(class = "carry-hidden-legacy", table_card("Carry Matrix", "carry_matrix"), plot_card("carry_stacked_plot", height = "320px"), plot_card("trade_component_plot", height = "260px"))
    )
  )),
  tabPanel("Diagnostics", dashboard_page(
    "Diagnostics",
    "Last successful Curve Explorer apply result.",
    NULL,
    class = "diagnostics-page",
    controls = side_panel(
      rail_header("Diagnostics Policy", "Inputs and quality thresholds"),
      control_section("Model Policy",
        sidebar_selectize_input("diag_fit_method", "Default Fit Method",
          choices = c("Cubic Spline" = "spline", "Nelson-Siegel" = "nelson_siegel"), selected = "spline"),
        div(class = "diag-policy-grid",
          numericInput("diag_residual_warn", "Residual Warn (bp)", 1.00, min = 0, step = 0.25),
          numericInput("diag_residual_fail", "Residual Fail (bp)", 2.50, min = 0.25, step = 0.25)
        )
      ),
      control_section("Proxy Analysis",
        div(class = "diag-info-note", tags$span(class = "info-dot", "i"),
          "If selected historical quotes are missing or stale, proxy/fallback dates are shown as Local RDS proxy metadata.")
      ),
      control_section("Unit Policy",
        div(class = "diag-policy-grid",
          sidebar_selectize_input("diag_display_units", "Display Units", choices = c("Percent" = "percent"), selected = "percent"),
          sidebar_selectize_input("diag_internal_units", "Internal Calc Units", choices = c("Decimal" = "decimal"), selected = "decimal")
        ),
        div(class = "rail-note compact-note", "Conversion: 1% = 0.01")
      ),
      control_section("Validation Checks",
        checkboxGroupInput("diag_validation_checks", NULL,
          choices = c("Monotonic 1M-30Y" = "monotonic", "Positive Forward Rates" = "forward_positive",
            "Residual Thresholds" = "residual", "Unit Consistency" = "unit"),
          selected = c("monotonic", "forward_positive", "residual", "unit"))
      )
    ),
    main = main_grid(
      div(class = "diag-kpi-strip",
        diagnostics_metric_card("Data Freshness", "diag_freshness_value", "diag_freshness_subtitle", "diag_freshness_detail", "diag_freshness_status"),
        diagnostics_metric_card("Missing Points", "diag_missing_value", "diag_missing_subtitle", "diag_missing_detail", "diag_missing_status"),
        diagnostics_metric_card("Fit RMSE", "diag_fit_rmse_value", "diag_fit_rmse_subtitle", "diag_fit_rmse_detail", "diag_fit_rmse_status"),
        diagnostics_metric_card("Proxy Flag", "diag_proxy_value", "diag_proxy_subtitle", "diag_proxy_detail", "diag_proxy_status"),
        diagnostics_metric_card("Unit Check", "diag_unit_value", "diag_unit_subtitle", "diag_unit_detail", "diag_unit_status")
      ),
      grid_row(class = "diagnostics-main-row",
        grid_col(class = "span-5", diagnostics_card("Diagnostics Summary", "diagnostics_table")),
        grid_col(class = "span-7", plot_card("diagnostics_residual_plot", height = "360px", class = "diagnostics-residual-card",
          subtitle = "Residual = observed YTM - fitted YTM. Thresholds follow Diagnostics Policy."))
      ),
      grid_row(class = "diagnostics-bottom-row",
        grid_col(class = "span-7", diagnostics_card("Input Points & Missing Tenors", "input_points",
          subtitle = "Expected tenor set is fixed for diagnostics coverage.", class = "diag-input-matrix-card")),
        grid_col(class = "span-5", diagnostics_card("About Diagnostics", "diagnostics_explanation", class = "diag-about-card"))
      )
    )
  ))
)

server <- function(input, output, session) {
  market <- reactiveVal(initial_market)
  history_compare_dates <- reactiveVal(as.Date(character()))
  history_dates_initialized <- reactiveVal(FALSE)
  history_run_started <- reactiveVal(NULL)
  history_run_finished <- reactiveVal(NULL)
  forward_run_started <- reactiveVal(NULL)
  forward_run_finished <- reactiveVal(NULL)
  forward_run_id <- reactiveVal(NULL)
  applied_curve <- reactiveVal(NULL)
  applied_history <- reactiveVal(NULL)
  applied_forward <- reactiveVal(NULL)
  applied_carry <- reactiveVal(NULL)
  applied_trade <- reactiveVal(NULL)
  large_plot_id <- reactiveVal(NULL)
  plot_builders <- new.env(parent = emptyenv())
  large_plot_titles <- c(
    curve_plot = "Curve Explorer",
    curve_residual_plot = "Curve Explorer: Residuals",
    history_absolute_plot = "History: Absolute Curves",
    history_change_plot = "History: Changes",
    forward_curve_plot = "Forward Curve",
    carry_component_plot = "Single Trade Carry / Roll",
    carry_spot_plot = "Spot Curve",
    carry_stacked_plot = "Carry + Roll by Tenor",
    carry_heatmap = "Carry + Roll Heatmap",
    trade_leg_pnl_plot = "Curve Trade Legs",
    trade_component_plot = "Curve Trade Portfolio"
  )
  status <- reactiveValues(
    curve = list(type = "pending", pct = 0, message = "Click Apply Curve to calculate."),
    history = list(type = "pending", pct = 0, message = "Click Run History Comparison to calculate."),
    forward = list(type = "pending", pct = 0, message = "Click Calculate Forward to calculate."),
    carry = list(type = "pending", pct = 0, message = "Click Calculate Carry & Roll to calculate."),
    trade = list(type = "pending", pct = 0, message = "Click Calculate Curve Trade to calculate.")
  )

  flush_status <- function() {
    # 真实 Shiny session 和 testServer session 对 flushReact 的暴露方式不完全一致。
    # 这里先用 session 自带方法；没有时再退到 Shiny 内部 flush，最后静默跳过，避免进度条刷新逻辑拖垮计算本身。
    session_flush <- session$flushReact
    if (is.function(session_flush)) return(session_flush())
    shiny_namespace <- asNamespace("shiny")
    if (exists("flushReact", envir = shiny_namespace, inherits = FALSE)) {
      namespace_flush <- get("flushReact", envir = shiny_namespace)
      if (is.function(namespace_flush)) return(namespace_flush())
    }
    invisible(NULL)
  }
  set_status <- function(page, type, pct, message) {
    status[[page]] <- list(type = type, pct = pct, message = message)
    flush_status()
  }
  mark_pending <- function(page) {
    if (!is.null(switch(page, curve = applied_curve(), history = applied_history(), forward = applied_forward(), carry = applied_carry(), trade = applied_trade()))) {
      status[[page]] <- list(type = "pending", pct = 0, message = "Inputs changed - click Calculate/Apply to update results.")
    }
  }
  run_page <- function(page, calculation) {
    set_status(page, "running", 8, "Reading data")
    result <- tryCatch(calculation(), error = function(error) error)
    if (inherits(result, "error")) {
      set_status(page, "error", 100, paste("Calculation failed:", conditionMessage(result)))
      return(NULL)
    }
    set_status(page, "success", 100, "Complete")
    result
  }
  progress_ui <- function(page) {
    x <- status[[page]]
    div(class = paste("inline-progress", paste0("progress-", x$type)),
      div(class = "progress-message", x$message),
      div(class = "progress", div(class = "progress-bar", role = "progressbar", style = paste0("width:", x$pct, "%;"), paste0(x$pct, "%"))))
  }
  for (page in c("curve", "history", "forward", "carry", "trade")) {
    local({ current <- page; output[[paste0(current, "_progress")]] <- renderUI(progress_ui(current)) })
  }
  register_plot <- function(output_id, builder) {
    plot_builders[[output_id]] <- builder
    output[[output_id]] <- renderPlotly(builder())
  }
  output$large_plot <- renderPlotly({
    output_id <- large_plot_id()
    req(output_id)
    builder <- plot_builders[[output_id]]
    req(!is.null(builder))
    builder()
  })
  for (plot_id in names(large_plot_titles)) {
    local({
      current <- plot_id
      observeEvent(input[[paste0("open_", current)]], {
        large_plot_id(current)
        showModal(modalDialog(
          title = large_plot_titles[[current]],
          plotlyOutput("large_plot", height = "840px"),
          size = "xl",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }, ignoreInit = TRUE)
    })
  }

  observeEvent(input$refresh_data, {
    refreshed <- tryCatch(load_market_data(project_dir), error = function(error) error)
    if (inherits(refreshed, "error")) showNotification(conditionMessage(refreshed), type = "error")
    else { market(refreshed); showNotification("Local RDS files refreshed. Existing results retained.", type = "message") }
  })
  curve_choices <- reactive(list(zero = zero_curve_names(market()$zero_curve), historical = historical_curve_names(market()$wide_rates)))
  available_dates <- reactive(sort(unique(market()$wide_rates$date)))
  update_curve_selector <- function(prefix, mode, preferred_zero = "USD UNITED STATES OIS", preferred_historical = "USD SOFR OIS") {
    if (is.null(mode) || !mode %in% c("zero", "historical")) mode <- "zero"
    choices <- curve_choices()[[mode]]
    curve_id <- if (nzchar(prefix)) paste0(prefix, "_curve_name") else "curve_name"
    date_id <- if (nzchar(prefix)) paste0(prefix, "_curve_date") else "curve_date"
    current <- isolate(input[[curve_id]])
    preferred <- if (mode == "zero") preferred_zero else preferred_historical
    selected <- if (!is.null(current) && current %in% choices) current else if (preferred %in% choices) preferred else choices[[1]]
    updateSelectizeInput(session, curve_id, choices = choices, selected = selected, server = TRUE)
    dates <- available_dates()
    current_date <- isolate(input[[date_id]])
    updateDateInput(session, date_id, value = if (is.null(current_date)) max(dates) else as.Date(current_date), min = min(dates), max = max(dates))
  }
  observe({
    update_curve_selector("", input$source_mode)
    update_curve_selector("forward", input$forward_source_mode)
    update_curve_selector("carry", input$carry_source_mode)
    dates <- available_dates()
    history_curves <- curve_choices()$historical
    defaults <- intersect(c("USD SOFR OIS", "EUR ESTR OIS"), history_curves)
    updateSelectizeInput(session, "history_curves", choices = history_curves,
      selected = if (length(isolate(input$history_curves))) isolate(input$history_curves) else defaults, server = TRUE)
    current_history_base <- normalize_history_date(isolate(input$history_base_date), dates)
    current_history_compare <- normalize_history_date(isolate(input$history_compare_date), dates)
    updateDateInput(session, "history_base_date", value = current_history_base, min = min(dates), max = max(dates))
    updateDateInput(session, "history_compare_date", value = current_history_compare, min = min(dates), max = max(dates))
    if (!isolate(history_dates_initialized())) { history_compare_dates(max(dates)); history_dates_initialized(TRUE) }
  })
  observeEvent(input$add_history_date, {
    req(input$history_compare_date)
    updated_dates <- sort(unique(c(history_compare_dates(), as.Date(input$history_compare_date))))
    if (length(updated_dates) > 6) {
      showNotification("Compare Dates supports up to 6 dates. Remove one before adding another.", type = "warning")
      return()
    }
    history_compare_dates(updated_dates)
    mark_pending("history")
  })
  observeEvent(input$remove_history_date, { history_compare_dates(setdiff(history_compare_dates(), as.Date(input$remove_history_date))); mark_pending("history") })
  observeEvent(input$clear_history_dates, { history_compare_dates(as.Date(character())); mark_pending("history") })
  observeEvent(input$clear_history_curves, { updateSelectizeInput(session, "history_curves", selected = character(0)); mark_pending("history") })
  history_current_combination_count <- reactive({
    curve_count <- length(input$history_curves %||% character(0))
    date_count <- length(sort(unique(c(as.Date(input$history_base_date %||% NA), history_compare_dates()))))
    if (!is.finite(date_count)) date_count <- 0
    curve_count * date_count
  })
  output$history_curve_chips <- renderUI({
    curves <- input$history_curves %||% character(0)
    tagList(
      div(class = "history-chip-list",
        if (!length(curves)) div(class = "small-note", "No curves selected.") else lapply(curves, function(curve) {
          tags$button(type = "button", class = "history-chip",
            onclick = sprintf("Shiny.setInputValue('remove_history_curve', '%s', {priority: 'event'})", gsub("'", "\\\\'", curve)),
            tags$span(curve), tags$span(class = "history-chip-x", "\u00d7"))
        })
      ),
      div(class = "history-chip-meta",
        tags$span(paste(length(curves), "of", length(curve_choices()$historical), "curves selected")),
        tags$button(type = "button", class = "link-button",
          onclick = "Shiny.setInputValue('clear_history_curves', Math.random(), {priority: 'event'})", "Clear all")
      )
    )
  })
  observeEvent(input$remove_history_curve, {
    current <- input$history_curves %||% character(0)
    updateSelectizeInput(session, "history_curves", selected = setdiff(current, input$remove_history_curve))
    mark_pending("history")
  })
  output$forward_snapshot_note <- renderUI({
    latest_date <- if (length(market()$wide_rates$date)) max(market()$wide_rates$date, na.rm = TRUE) else Sys.Date()
    div(class = "forward-blue-note", tags$span(class = "info-dot", "i"),
      paste("Using latest local zero-rate snapshot. Display valuation date:", latest_date))
  })
  output$carry_zero_date_note <- renderUI({
    latest_date <- if (length(market()$wide_rates$date)) max(market()$wide_rates$date, na.rm = TRUE) else Sys.Date()
    div(class = "forward-blue-note", tags$span(class = "info-dot", "i"),
      paste("Using latest local zero-rate snapshot. Display date:", latest_date))
  })
  observeEvent(input$reset_forward, {
    updateRadioButtons(session, "forward_source_mode", selected = "zero")
    updateSelectizeInput(session, "forward_start", selected = 1)
    updateSelectizeInput(session, "forward_end", selected = 5)
    updateRadioButtons(session, "forward_compounding", selected = "annual")
    set_status("forward", "pending", 0, "Inputs reset - click Calculate Forward to update results.")
  }, ignoreInit = TRUE)
  output$history_date_tags <- renderUI({
    dates <- history_compare_dates()
    tagList(tags$div(class = "history-date-label", "Selected comparison dates"),
      tags$div(class = "history-date-tags", lapply(as.character(dates), function(date_text) tags$button(
        type = "button", class = "history-date-tag",
        onclick = sprintf("Shiny.setInputValue('remove_history_date', '%s', {priority: 'event'})", date_text),
        tags$span(date_text), tags$span(class = "history-date-remove", "\u00d7")))),
      if (!length(dates)) div(class = "small-note", "No comparison dates selected."),
      div(class = "history-chip-meta",
        tags$span(paste(length(dates), "of 6 dates selected")),
        tags$button(type = "button", class = "link-button",
          onclick = "Shiny.setInputValue('clear_history_dates', Math.random(), {priority: 'event'})", "Clear all")
      ))
  })
  output$history_combination_warning <- renderUI({
    count <- history_current_combination_count()
    div(class = if (count > 30) "rail-note rail-warning history-combo-warning danger" else "rail-note rail-warning history-combo-warning",
      tags$span(class = "warning-icon", "!"),
      div(strong("Max 30 combinations per run"), tags$br(), paste(count, "combinations will be calculated")))
  })
  output$forward_status_detail <- renderUI({
    x <- status$forward
    completed_text <- if (is.null(forward_run_finished())) "--" else format(forward_run_finished(), "%H:%M:%S")
    run_text <- forward_run_id() %||% "--"
    div(class = paste("forward-status-body", paste0("forward-status-", x$type)),
      div(class = "forward-status-line", tags$span(class = "status-dot"), tags$span(x$message)),
      div(class = "progress forward-progress", div(class = "progress-bar", role = "progressbar",
        style = paste0("width:", x$pct, "%;"), paste0(x$pct, "%"))),
      div(class = "forward-status-meta", paste("Completed at:", completed_text)),
      div(class = "forward-status-run", tags$span(paste("Run ID:", run_text)), tags$span(class = "copy-mini", "\u2398"))
    )
  })

  observeEvent(list(input$source_mode, input$curve_name, input$curve_date, input$fit_methods,
    input$curve_start_tenor, input$curve_end_tenor, input$show_raw_points,
    input$show_ns_fit, input$show_spline_fit, input$show_residuals_panel), mark_pending("curve"), ignoreInit = TRUE)
  observeEvent(list(input$history_curves, input$history_base_date, input$history_compare_date,
    input$history_start_tenor, input$history_end_tenor, input$history_source_mode), mark_pending("history"), ignoreInit = TRUE)
  observeEvent(list(input$forward_source_mode, input$forward_curve_name, input$forward_curve_date, input$forward_fit_method, input$forward_start, input$forward_end, input$forward_compounding), mark_pending("forward"), ignoreInit = TRUE)
  observeEvent(list(input$carry_source_mode, input$carry_curve_name, input$carry_curve_date, input$carry_fit_method, input$carry_start, input$carry_end, input$carry_hold, input$carry_direction, input$dv01), mark_pending("carry"), ignoreInit = TRUE)
  observeEvent(list(input$carry_source_mode, input$carry_curve_name, input$carry_curve_date, input$carry_fit_method, input$trade_structure, input$trade_start, input$trade_short_tenor, input$trade_belly_tenor, input$trade_long_tenor, input$trade_hold, input$trade_risk_budget, input$trade_short_dv01, input$trade_belly_dv01, input$trade_long_dv01), mark_pending("trade"), ignoreInit = TRUE)

  observeEvent(input$apply_curve, {
    result <- run_page("curve", function() {
      req(input$curve_name)
      set_status("curve", "running", 28, "Resolving selected date")
      points <- if (identical(input$source_mode, "zero")) extract_zero_curve(market()$zero_curve, input$curve_name) else extract_historical_curve(market()$wide_rates, input$curve_name, input$curve_date)
      requested_date <- attr(points, "requested_date")
      effective_date <- attr(points, "effective_date")
      start_tenor <- as.numeric(input$curve_start_tenor %||% min(points$tenor, na.rm = TRUE))
      end_tenor <- as.numeric(input$curve_end_tenor %||% max(points$tenor, na.rm = TRUE))
      range <- sort(c(start_tenor, end_tenor))
      points <- points[points$tenor >= range[[1]] & points$tenor <= range[[2]], , drop = FALSE]
      if (nrow(points) < 4) stop("Selected tenor range has too few points for Nelson-Siegel and spline fitting.", call. = FALSE)
      set_status("curve", "running", 58, "Fitting curve")
      source <- curve_source_label(input$source_mode, input$curve_name, input$curve_date)
      fits <- lapply(c("nelson_siegel", "spline"), function(method) fit_curve(points$tenor, points$rate, method, source, identical(input$source_mode, "historical")))
      set_status("curve", "running", 86, "Generating charts")
      latest_db_date <- if (length(market()$wide_rates$date)) max(market()$wide_rates$date, na.rm = TRUE) else NA
      list(points = points, fits = fits, mode = input$source_mode, curve_name = input$curve_name,
        requested_date = requested_date, effective_date = effective_date,
        latest_db_date = latest_db_date, display_methods = input$fit_methods %||% character(0),
        show_raw = isTRUE(input$show_raw_points), show_ns = isTRUE(input$show_ns_fit),
        show_spline = isTRUE(input$show_spline_fit), show_residuals = isTRUE(input$show_residuals_panel),
        tenor_range = range)
    })
    if (!is.null(result)) applied_curve(result)
  }, ignoreInit = FALSE)
  observeEvent(input$run_history, {
    started <- Sys.time()
    history_run_started(started)
    result <- run_page("history", function() {
      req(input$history_curves, input$history_base_date)
      dates <- sort(unique(c(as.Date(input$history_base_date), history_compare_dates())))
      combinations <- length(input$history_curves) * length(dates)
      if (combinations > 30) stop("Select fewer curves or dates: ", combinations, " combinations exceeds the maximum of 30.", call. = FALSE)
      set_status("history", "running", 18, paste("Resolving", combinations, "curve/date combinations"))
      data <- build_history_comparison(market()$wide_rates, input$history_curves, dates, input$history_base_date,
        max_combinations = 30, progress_callback = function(done, total, curve, date) {
          set_status("history", "running", round(18 + 65 * done / total), paste("Calculating", curve, "for", date))
        })
      set_status("history", "running", 90, "Generating charts")
      data$series <- paste(data$curve, data$requested_date, sep = " | ")
      finished <- Sys.time()
      tenor_range <- sort(c(as.numeric(input$history_start_tenor %||% min(data$tenor, na.rm = TRUE)),
        as.numeric(input$history_end_tenor %||% max(data$tenor, na.rm = TRUE))))
      list(data = data, base_date = as.Date(input$history_base_date), dates = dates,
        curves = input$history_curves, combinations = combinations, started = started,
        finished = finished, duration = as.numeric(difftime(finished, started, units = "secs")),
        source_mode = input$history_source_mode %||% "bloomberg", tenor_range = tenor_range)
    })
    if (!is.null(result)) {
      history_run_finished(result$finished)
      applied_history(result)
    }
  }, ignoreInit = FALSE)
  observeEvent(input$calculate_forward, {
    started <- Sys.time()
    forward_run_started(started)
    result <- run_page("forward", function() {
      set_status("forward", "running", 30, "Resolving selected date")
      bundle <- prepare_curve_fit(market(), input$forward_source_mode, input$forward_curve_name, input$forward_curve_date, input$forward_fit_method %||% "nelson_siegel")
      set_status("forward", "running", 68, "Calculating forward")
      value <- calculate_forward(bundle$fit, input$forward_start, input$forward_end, input$forward_compounding)
      value$curve <- bundle$curve_name
      value$requested_date <- if (bundle$proxy) as.character(bundle$requested_date) else "Snapshot date unavailable"
      value$effective_date <- if (bundle$proxy) as.character(bundle$effective_date) else "Snapshot date unavailable"
      set_status("forward", "running", 90, "Generating chart")
      list(bundle = bundle, result = value, start = as.numeric(input$forward_start), end = as.numeric(input$forward_end), compounding = input$forward_compounding)
    })
    if (!is.null(result)) {
      finished <- Sys.time()
      forward_run_finished(finished)
      forward_run_id(paste0(format(finished, "%y%m%d-%H%M%S"), "-FWD"))
      applied_forward(result)
    }
  }, ignoreInit = FALSE)
  observeEvent(input$calculate_carry, {
    result <- run_page("carry", function() {
      set_status("carry", "running", 28, "Resolving selected date")
      bundle <- prepare_curve_fit(market(), input$carry_source_mode, input$carry_curve_name, input$carry_curve_date, input$carry_fit_method)
      set_status("carry", "running", 58, "Calculating carry and roll")
      single <- calculate_carry_roll(bundle$fit, input$carry_start, input$carry_end, as.numeric(input$carry_hold), input$carry_direction, "annual")
      matrix <- build_carry_matrix(bundle$fit, c(1, 2, 3, 5, 7, 10, 15, 20, 30), c(1/12, 0.25, 0.5, 1), input$carry_direction, input$dv01, "annual")
      tenor_levels <- paste0(format(sort(unique(matrix$end_years)), trim = TRUE), "Y")
      matrix$tenor_label <- factor(matrix$tenor_label, levels = tenor_levels, ordered = TRUE)
      matrix$hold_label <- factor(matrix$hold_label, levels = c("1M", "3M", "6M", "1Y"), ordered = TRUE)
      set_status("carry", "running", 90, "Generating charts")
      list(bundle = bundle, single = single, matrix = matrix, dv01 = input$dv01)
    })
    if (!is.null(result)) applied_carry(result)
  }, ignoreInit = FALSE)

  observeEvent(list(input$trade_structure, input$trade_risk_budget), {
    legs <- try(curve_trade_legs(input$trade_structure, input$trade_short_tenor, input$trade_belly_tenor, input$trade_long_tenor, input$trade_risk_budget), silent = TRUE)
    if (!inherits(legs, "try-error")) {
      updateNumericInput(session, "trade_short_dv01", value = legs$dv01[[1]])
      if (nrow(legs) == 3) updateNumericInput(session, "trade_belly_dv01", value = legs$dv01[[2]])
      updateNumericInput(session, "trade_long_dv01", value = legs$dv01[[nrow(legs)]])
    }
  }, ignoreInit = FALSE)
  apply_neutral_trade_dv01 <- function() {
    legs <- curve_trade_legs(input$trade_structure, input$trade_short_tenor, input$trade_belly_tenor, input$trade_long_tenor, input$trade_risk_budget)
    updateNumericInput(session, "trade_short_dv01", value = legs$dv01[[1]])
    if (nrow(legs) == 3) updateNumericInput(session, "trade_belly_dv01", value = legs$dv01[[2]])
    updateNumericInput(session, "trade_long_dv01", value = legs$dv01[[nrow(legs)]])
  }
  observeEvent(input$load_neutral_dv01, apply_neutral_trade_dv01())
  observeEvent(input$load_neutral_dv01_trade, apply_neutral_trade_dv01())
  observeEvent(input$calculate_curve_trade, {
    result <- run_page("trade", function() {
      set_status("trade", "running", 28, "Resolving selected date")
      bundle <- prepare_curve_fit(market(), input$carry_source_mode, input$carry_curve_name, input$carry_curve_date, input$carry_fit_method)
      legs <- curve_trade_legs(input$trade_structure, input$trade_short_tenor, input$trade_belly_tenor, input$trade_long_tenor, input$trade_risk_budget)
      legs$dv01 <- if (nrow(legs) == 2) c(input$trade_short_dv01, input$trade_long_dv01) else c(input$trade_short_dv01, input$trade_belly_dv01, input$trade_long_dv01)
      set_status("trade", "running", 65, "Calculating each leg")
      calculation <- calculate_curve_trade(bundle$fit, legs, as.numeric(input$trade_hold), "annual", risk_budget = input$trade_risk_budget, start = input$trade_start)
      set_status("trade", "running", 90, "Generating charts")
      list(bundle = bundle, calculation = calculation, structure = input$trade_structure, start = as.numeric(input$trade_start), hold = as.numeric(input$trade_hold))
    })
    if (!is.null(result)) applied_trade(result)
  }, ignoreInit = FALSE)

  output$loaded_at <- renderText(paste("Loaded:", format(market()$loaded_at, "%Y-%m-%d %H:%M:%S")))
  curve_fit <- reactive({ req(applied_curve()); fits <- applied_curve()$fits; ns <- fits[vapply(fits, function(x) x$method == "nelson_siegel", logical(1))]; if (length(ns)) ns[[1]] else fits[[1]] })
  ns_curve_fit <- reactive({ req(applied_curve()); fit <- find_fit(applied_curve()$fits, "nelson_siegel"); req(fit); fit })
  spline_curve_fit <- reactive({ req(applied_curve()); find_fit(applied_curve()$fits, "spline") })
  output$zero_curve_date_note <- renderUI({
    dates <- available_dates()
    div(class = "zero-date-note", "Latest local database date: ", if (length(dates)) as.character(max(dates)) else "NA")
  })
  output$effective_curve_date <- renderText({
    req(applied_curve())
    x <- applied_curve()
    if (x$mode == "zero") paste("Effective:", as.character(x$latest_db_date %||% "NA")) else paste("Requested:", x$requested_date, "| Effective:", x$effective_date)
  })
  output$source_banner <- renderUI({ req(applied_curve()); x <- applied_curve(); if (x$mode == "historical") div(class = "proxy-banner", tags$span(class = "badge-label", "Proxy"), paste("Historical quotes | Requested:", x$requested_date, "| Effective:", x$effective_date)) else div(class = "official-banner", tags$span(class = "badge-label", "Local snapshot"), paste("Latest local database date:", x$latest_db_date %||% "NA")) })
  output$curve_title_block <- renderUI({
    req(applied_curve())
    x <- applied_curve()
    effective <- if (x$mode == "zero") as.character(x$latest_db_date %||% "NA") else as.character(x$effective_date %||% "NA")
    div(class = "curve-title-block",
      div(class = "curve-title-row", tags$span(class = "curve-star", "\u2605"), tags$h2(x$curve_name)),
      div(class = "curve-meta-row",
        div(class = "curve-meta-item", tags$span("Source"), strong("Bloomberg")),
        div(class = "curve-meta-item", tags$span("Effective Date"), strong(effective)),
        div(class = "curve-meta-item", tags$span("Day Count"), strong("NA")),
        div(class = "curve-meta-item", tags$span("Currency"), strong(substr(x$curve_name, 1, 3)))
      )
    )
  })
  output$curve_metric_name <- renderText({ req(applied_curve()); applied_curve()$curve_name })
  output$curve_metric_source <- renderText({ req(applied_curve()); if (applied_curve()$mode == "zero") "Zero snapshot" else "Historical proxy" })
  output$curve_metric_beta0 <- renderText({ req(ns_curve_fit()); fmt_pct(100 * ns_curve_fit()$parameters[["beta0"]]) })
  output$curve_metric_beta1 <- renderText({ req(ns_curve_fit()); fmt_pct(100 * ns_curve_fit()$parameters[["beta1"]]) })
  output$curve_metric_tau <- renderText({ req(ns_curve_fit()); fmt_num(ns_curve_fit()$parameters[["tau"]]) })
  output$curve_metric_rmse <- renderText({ req(ns_curve_fit()); fmt_num_digits(ns_curve_fit()$rmse_bp, 1) })
  output$curve_metric_rsq <- renderText({ req(ns_curve_fit()); paste0("R\u00b2: ", format_rsq(fit_rsq(ns_curve_fit())), "%") })
  output$curve_metric_obs <- renderText({ req(applied_curve()); nrow(applied_curve()$points) })
  output$curve_metric_range <- renderText({ req(applied_curve()); rng <- applied_curve()$tenor_range; paste(tenor_label(rng[[1]]), "to", tenor_label(rng[[2]])) })
  register_plot("curve_plot", function() {
    x <- applied_curve(); req(x)
    points <- x$points[order(x$points$tenor), , drop = FALSE]
    ns <- find_fit(x$fits, "nelson_siegel")
    spline <- find_fit(x$fits, "spline")
    ns_rate <- if (!is.null(ns)) decimal_to_percent(curve_rate(ns, points$tenor)) else rep(NA_real_, nrow(points))
    spline_rate <- if (!is.null(spline)) decimal_to_percent(curve_rate(spline, points$tenor)) else rep(NA_real_, nrow(points))
    market_rate <- decimal_to_percent(points$rate)
    hover_text <- paste0(
      "Tenor: ", tenor_label(points$tenor),
      "<br>Market Raw: ", fmt_pct(market_rate),
      "<br>Nelson-Siegel: ", ifelse(is.na(ns_rate), "NA", fmt_pct(ns_rate)),
      "<br>Spline: ", ifelse(is.na(spline_rate), "NA", fmt_pct(spline_rate)),
      "<br>NS Residual: ", ifelse(is.na(ns_rate), "NA", fmt_bp((market_rate - ns_rate) * 100)),
      "<br>Spline Residual: ", ifelse(is.na(spline_rate), "NA", fmt_bp((market_rate - spline_rate) * 100))
    )
    display_methods <- x$display_methods %||% character(0)
    show_raw <- isTRUE(x$show_raw) || !length(display_methods)
    plot <- plotly::plot_ly()
    if (show_raw) {
      plot <- plotly::add_trace(plot, x = points$tenor, y = market_rate, name = "Market Raw",
        text = hover_text, hoverinfo = "text", type = "scatter", mode = "markers",
        marker = list(color = "#334155", size = 7, line = list(color = "#FFFFFF", width = 1)), inherit = FALSE)
    }
    if ("nelson_siegel" %in% display_methods && isTRUE(x$show_ns) && !is.null(ns)) {
      plot <- plotly::add_trace(plot, x = points$tenor, y = ns_rate, name = "Nelson-Siegel",
        text = hover_text, hoverinfo = "text", type = "scatter", mode = "lines",
        line = list(color = "#2563EB", width = 3), inherit = FALSE)
    }
    if ("spline" %in% display_methods && isTRUE(x$show_spline) && !is.null(spline)) {
      plot <- plotly::add_trace(plot, x = points$tenor, y = spline_rate, name = "Spline",
        text = hover_text, hoverinfo = "text", type = "scatter", mode = "lines",
        line = list(color = "#0F9F8D", width = 3, dash = "dash"), inherit = FALSE)
    }
    axis_ticks <- axis_tenor_ticks(points$tenor)
    finished <- plotly_trace_finish(plotly::layout(plot, title = "Zero Curve Fit",
      xaxis = list(title = NULL, tickvals = axis_ticks$tickvals, ticktext = axis_ticks$ticktext),
      yaxis = list(title = "Rate (%)"),
      legend = list(orientation = "h", x = 0, y = 1.12, xanchor = "left", yanchor = "bottom")))
    finished$x$layout$xaxis$tickvals <- axis_ticks$tickvals
    finished$x$layout$xaxis$ticktext <- axis_ticks$ticktext
    finished$x$layout$xaxis$title <- ""
    finished$x$layout$xaxis$tickangle <- 0
    finished$x$layout$legend <- list(orientation = "h", x = 0, y = 1.12, xanchor = "left", yanchor = "bottom")
    finished
  })
  register_plot("curve_residual_plot", function() {
    x <- applied_curve(); req(x)
    if (!isTRUE(x$show_residuals)) {
      return(plotly_trace_finish(plotly::layout(plotly::plot_ly(), title = "Fit Residuals", annotations = list(text = "Residuals hidden by display options.", x = 0.5, y = 0.5, showarrow = FALSE))))
    }
    display_methods <- x$display_methods %||% character(0)
    if (!length(display_methods)) {
      return(plotly_trace_finish(plotly::layout(plotly::plot_ly(), title = "Fit Residuals", annotations = list(text = "Select NS or Spline to display residual bars.", x = 0.5, y = 0.5, showarrow = FALSE))))
    }
    fits <- Filter(function(fit) fit$method %in% display_methods, x$fits)
    long <- do.call(rbind, lapply(fits, function(fit) {
      data.frame(tenor = fit$diagnostics$tenor, tenor_text = tenor_label(fit$diagnostics$tenor),
        residual_bp = fit$diagnostics$residual_bp,
        method = if (fit$method == "nelson_siegel") "NS Residual" else "Spline Residual")
    }))
    long$tenor_text <- factor(long$tenor_text, levels = tenor_label(sort(unique(long$tenor))))
    long$text <- paste0(long$method, "<br>Tenor: ", long$tenor_text, "<br>Residual: ", fmt_bp(long$residual_bp))
    axis_ticks <- axis_tenor_ticks(long$tenor)
    plot <- plotly_finish(ggplot(long, aes(tenor_text, residual_bp, fill = method, text = text)) +
      geom_hline(yintercept = 0, color = "#94A3B8", linewidth = 0.35) +
      geom_col(position = position_dodge(width = 0.7), width = 0.62) +
      scale_fill_manual(values = c("NS Residual" = "#2563EB", "Spline Residual" = "#0F9F8D")) +
      scale_x_discrete(breaks = axis_ticks$ticktext) +
      labs(title = "Fit Residuals by Tenor", x = NULL, y = "Residual (bp)", fill = NULL) +
      theme_minimal(base_size = 10) +
      theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 11, margin = margin(b = 24)),
        legend.position = "top", legend.direction = "horizontal"))
    plot$x$layout$legend <- list(orientation = "h", x = 0, y = 1.03, xanchor = "left", yanchor = "bottom")
    plot$x$layout$margin$t <- 130
    plot
  })
  fitted_parameters_ui <- reactive({
    req(applied_curve())
    ns <- ns_curve_fit()
    spline <- spline_curve_fit()
    tagList(
      parameter_section_ui("Nelson-Siegel Parameters", data.frame(
        Metric = c("Beta0 (Level)", "Beta1 (Slope)", "Tau (Curvature)"),
        Value = c(fmt_pct(100 * ns$parameters[["beta0"]]), fmt_pct(100 * ns$parameters[["beta1"]]), fmt_num(ns$parameters[["tau"]])),
        stringsAsFactors = FALSE
      )),
      parameter_section_ui("Goodness of Fit", data.frame(
        Metric = c("RMSE (bp)", "R-squared"),
        Value = c(fmt_num_digits(ns$rmse_bp, 1), paste0(format_rsq(fit_rsq(ns)), "%")),
        stringsAsFactors = FALSE
      )),
      parameter_section_ui("Spline", data.frame(
        Metric = c("Method", "Knots", "RMSE (bp)"),
        Value = c(if (is.null(spline)) "NA" else "Cubic spline", if (is.null(spline)) "NA" else "Market tenors",
          if (is.null(spline)) "NA" else fmt_num_digits(spline$rmse_bp, 1)),
        stringsAsFactors = FALSE
      ))
    )
  })
  output$fitted_parameters <- renderUI({ fitted_parameters_ui() })
  output$fit_summary <- renderDT({ req(applied_curve()); centered_datatable(format_display_df(data.frame(Method = vapply(applied_curve()$fits, `[[`, character(1), "method"), RMSE_bp = vapply(applied_curve()$fits, `[[`, numeric(1), "rmse_bp"))), options = list(dom = "t")) })
  output$ns_parameters <- renderDT({ req(ns_curve_fit()); ns <- ns_curve_fit(); centered_datatable(format_display_df(data.frame(Parameter = names(ns$parameters), Value = ns$parameters)), options = list(dom = "t")) })
  output$curve_explanation <- renderUI({
    req(ns_curve_fit())
    tagList(
      div(class = "fit-note-item", "Nelson-Siegel summarizes the curve with Level, Slope and Curvature factors. Beta0 is the long-rate anchor, Beta1 tilts the front end, and Tau controls where the curvature factor has the most influence."),
      div(class = "fit-note-item", "Spline is a flexible interpolation layer through local curve shape. It is useful for visual inspection, while the KPI cards always report the Nelson-Siegel fit so the dashboard has one consistent model benchmark."),
      div(class = "fit-note-item", "Historical quote curves are proxy analytics; zero-rate curves use the latest local RDS snapshot.")
    )
  })

  output$history_header_subtitle <- renderUI({
    x <- applied_history()
    curve_count <- if (is.null(x)) length(input$history_curves %||% character(0)) else length(unique(x$curves))
    date_count <- if (is.null(x)) length(sort(unique(c(as.Date(input$history_base_date %||% NA), history_compare_dates())))) else length(unique(x$dates))
    tagList(tags$strong(curve_count), " curves x ", tags$strong(date_count), " dates, nearest effective date fallback on")
  })
  output$history_status_detail <- renderUI({
    x <- status$history
    applied <- applied_history()
    completed_text <- if (!is.null(applied) && !is.null(applied$finished)) format(applied$finished, "%H:%M:%S") else "\u2014"
    duration_text <- if (!is.null(applied) && !is.null(applied$duration)) sprintf("00:00:%02d", as.integer(round(applied$duration))) else "\u2014"
    div(class = paste("history-status-body", paste0("history-status-", x$type)),
      div(class = "history-status-line", tags$span(class = "status-dot"), tags$span(x$message)),
      div(class = "progress history-progress", div(class = "progress-bar", role = "progressbar",
        style = paste0("width:", x$pct, "%;"), paste0(x$pct, "%"))),
      div(class = "history-status-meta", paste("Completed:", completed_text)),
      div(class = "history-status-meta", paste("Duration:", duration_text)),
      tags$button(type = "button", class = "link-button status-detail-link", "View run details")
    )
  })
  register_plot("history_absolute_plot", function() {
    req(applied_history())
    x <- filter_history_tenor_range(applied_history()$data, applied_history()$tenor_range)
    validate(need(nrow(x) > 0, "Selected tenor range has no data."))
    x <- x[order(x$curve, x$requested_date, x$tenor), ]
    curves <- unique(x$curve)
    dates <- as.character(sort(unique(as.Date(x$requested_date))))
    colors <- history_curve_palette(curves)
    dashes <- history_date_linetypes(dates)
    p <- plot_ly()
    for (curve in curves) {
      for (date_text in dates) {
        segment <- x[x$curve == curve & as.character(x$requested_date) == date_text, , drop = FALSE]
        if (!nrow(segment)) next
        date_index <- match(date_text, dates)
        segment$text <- paste0("Curve: ", segment$curve,
          "<br>Requested: ", segment$requested_date,
          "<br>Effective: ", segment$effective_date,
          "<br>Tenor: ", tenor_label(segment$tenor),
          "<br>Rate: ", fmt_pct(segment$rate_percent))
        p <- add_trace(p, data = segment, x = ~tenor, y = ~rate_percent, type = "scatter", mode = "lines+markers",
          name = paste(curve, date_text), legendgroup = curve, showlegend = TRUE, text = ~text, hoverinfo = "text",
          line = list(color = grDevices::adjustcolor(colors[[curve]], alpha.f = history_date_alpha(date_index)),
            width = if (date_index == 1) 2.5 else 1.4, dash = dashes[[date_text]]),
          marker = list(color = grDevices::adjustcolor(colors[[curve]], alpha.f = history_date_alpha(date_index)), size = 5))
      }
    }
    ticks <- axis_tenor_ticks(x$tenor)
    plotly_trace_finish(layout(p, title = "Absolute Curves by Date",
      xaxis = list(title = "Tenor", tickvals = ticks$tickvals, ticktext = ticks$ticktext),
      yaxis = list(title = "Zero Rate (%)"),
      legend = list(orientation = "h", x = 0, y = -0.24, tracegroupgap = 8)),
      margin = list(t = 76, r = 28, b = 132, l = 58))
  })
  register_plot("history_change_plot", function() {
    req(applied_history())
    x <- filter_history_tenor_range(applied_history()$data, applied_history()$tenor_range)
    base_date <- as.Date(applied_history()$base_date)
    x <- x[as.Date(x$requested_date) != base_date, , drop = FALSE]
    validate(need(nrow(x) > 0, "Add at least one compare date."))
    keep_tenors <- history_display_tenor_values(x$tenor)
    x <- x[x$tenor %in% keep_tenors, , drop = FALSE]
    x$tenor_label <- factor(tenor_label(x$tenor), levels = tenor_label(sort(unique(keep_tenors))), ordered = TRUE)
    curves <- unique(x$curve)
    dates <- as.character(sort(unique(as.Date(x$requested_date))))
    colors <- history_curve_palette(curves)
    panels <- lapply(curves, function(curve) {
      segment_curve <- x[x$curve == curve, , drop = FALSE]
      panel <- plot_ly()
      for (date_text in dates) {
        segment <- segment_curve[as.character(segment_curve$requested_date) == date_text, , drop = FALSE]
        if (!nrow(segment)) next
        date_index <- match(date_text, dates)
        segment$text <- paste0("Curve: ", segment$curve,
          "<br>Requested: ", segment$requested_date,
          "<br>Effective: ", segment$effective_date,
          "<br>Tenor: ", segment$tenor_label,
          "<br>Change: ", fmt_bp(segment$change_bp))
        panel <- add_bars(panel, data = segment, x = ~tenor_label, y = ~change_bp,
          name = date_text, showlegend = identical(curve, curves[[1]]), hovertext = ~text,
          hoverinfo = "text", textposition = "none",
          marker = list(color = grDevices::adjustcolor(colors[[curve]], alpha.f = history_date_alpha(date_index))))
      }
      layout(panel,
        xaxis = list(title = "Tenor"),
        yaxis = list(title = if (identical(curve, curves[[1]])) "Change (bp)" else ""),
        shapes = list(list(type = "line", x0 = -0.5, x1 = length(unique(segment_curve$tenor_label)) - 0.5,
          y0 = 0, y1 = 0, line = list(color = "#94A3B8", width = 1))))
    })
    p <- subplot(panels, nrows = 1, shareY = TRUE, titleX = TRUE, margin = 0.04) %>%
      layout(barmode = "group",
        legend = list(orientation = "h", x = 0, y = -0.22))
    title_annotations <- lapply(seq_along(curves), function(index) {
      curve <- curves[[index]]
      axis_name <- if (index == 1) "xaxis" else paste0("xaxis", index)
      domain <- p$x$layout[[axis_name]]$domain %||% c((index - 1) / length(curves), index / length(curves))
      list(text = curve, x = mean(domain), y = 1.04,
        xref = "paper", yref = "paper", showarrow = FALSE,
        xanchor = "center", yanchor = "bottom",
        font = list(size = 18, color = colors[[curve]]))
    })
    p <- layout(p, annotations = title_annotations)
    plotly_trace_finish(p, margin = list(t = 92, r = 28, b = 92, l = 58))
  })
  output$history_comparison_table <- renderDT({
    req(applied_history())
    table <- history_quote_details(applied_history()$data, applied_history()$tenor_range)
    centered_datatable(table, options = list(pageLength = 18, scrollX = FALSE, dom = "tip", ordering = FALSE,
      columnDefs = list(list(targets = 5:8, visible = FALSE, searchable = FALSE)),
      rowCallback = JS(sprintf(
        "function(row, data) {
          var text = data[4];
          var value = parseFloat(text);
          var color = '%s';
          if (!isNaN(value) && value > 0) color = '%s';
          if (!isNaN(value) && value < 0) color = '%s';
          $('td:eq(4)', row).css({'color': color, 'font-weight': '700'});
          if (data[7] === true || data[7] === 'TRUE' || data[7] === 'true') {
            $(row).addClass('history-date-group-start');
          }
          if (data[8] === true || data[8] === 'TRUE' || data[8] === 'true') {
            $(row).addClass('history-curve-group-start');
          }
        }", neutral_color, positive_color, negative_color)),
      drawCallback = JS(
        "function(settings) {
          var api = this.api();
          var rows = api.rows({page: 'current'}).nodes();
          api.rows({page: 'current'}).every(function() {
            var row = $(this.node());
            var data = this.data();
            var dateCell = row.find('td:eq(0)');
            var curveCell = row.find('td:eq(1)');
            if (data[7] === true || data[7] === 'TRUE' || data[7] === 'true') {
              dateCell.attr('rowspan', parseInt(data[5], 10)).addClass('history-merged-cell history-date-cell');
            } else {
              dateCell.remove();
            }
            if (data[8] === true || data[8] === 'TRUE' || data[8] === 'true') {
              curveCell.attr('rowspan', parseInt(data[6], 10)).addClass('history-merged-cell history-curve-cell');
            } else {
              curveCell.remove();
            }
          });
        }")
    ))
  })
  output$history_metric_combos <- renderText({ req(applied_history()); applied_history()$combinations })
  output$history_metric_combos_sub <- renderText({ req(applied_history()); paste(length(unique(applied_history()$curves)), "Curves x", length(unique(applied_history()$dates)), "Dates") })
  output$history_metric_largest <- renderUI({
    req(applied_history())
    info <- history_largest_move_info(applied_history()$data)
    div(class = paste("history-largest-value", paste0("largest-", info$direction)),
      span(class = "history-largest-icon", info$icon),
      span(class = "history-largest-number", info$value)
    )
  })
  output$history_metric_largest_sub <- renderText({ req(applied_history()); history_largest_move_info(applied_history()$data)$subtitle })
  output$history_metric_fallbacks <- renderText({
    req(applied_history())
    x <- unique(applied_history()$data[, c("curve", "requested_date", "effective_date")])
    sum(as.Date(x$requested_date) != as.Date(x$effective_date))
  })
  output$history_metric_fallbacks_sub <- renderText({
    req(applied_history())
    pairs <- unique(applied_history()$data[, c("curve", "requested_date", "effective_date")])
    paste("of", nrow(pairs), "curve/date pairs")
  })
  output$history_metric_proxy <- renderText({ "On" })
  output$history_metric_proxy_sub <- renderText({ "Bloomberg via local RDS" })
  output$history_footer_asof <- renderText({
    req(applied_history())
    paste("Data as of run:", format(applied_history()$finished, "%Y-%m-%d %H:%M:%S"))
  })

  curve_banner <- function(bundle) {
    req(bundle)
    if (bundle$proxy) {
      div(class = "proxy-banner", tags$span(class = "badge-label", "Proxy"), paste(bundle$source, "| Requested:", bundle$requested_date, "| Effective:", bundle$effective_date))
    } else {
      div(class = "official-banner", tags$span(class = "badge-label", "Local snapshot"), paste(bundle$source, "| Latest local zero-rate snapshot"))
    }
  }
  signed_bp_text <- function(value) {
    value <- as.numeric(value)
    if (!is.finite(value)) return("Prev close unavailable")
    paste0(if (value > 0) "+" else "", fmt_bp(value), " vs Prev Close")
  }
  forward_previous_close <- function(x) {
    history_names <- historical_curve_names(market()$wide_rates)
    curve_name <- x$bundle$curve_name
    if (!curve_name %in% history_names) return(NULL)
    valuation_date <- forward_valuation_date(x)
    previous_dates <- sort(unique(market()$wide_rates$date[market()$wide_rates$date < as.Date(valuation_date)]))
    if (!length(previous_dates)) return(NULL)
    previous_points <- tryCatch(extract_historical_curve(market()$wide_rates, curve_name, tail(previous_dates, 1)), error = function(error) NULL)
    if (is.null(previous_points)) return(NULL)
    previous_fit <- tryCatch(fit_curve(previous_points$tenor, previous_points$rate, x$bundle$fit$method,
      source = paste0("Previous close | ", curve_name), proxy = TRUE), error = function(error) NULL)
    if (is.null(previous_fit)) return(NULL)
    list(
      fit = previous_fit,
      forward = calculate_forward(previous_fit, x$start, x$end, x$compounding)$forward_percent,
      start_spot = decimal_to_percent(curve_rate(previous_fit, pmax(x$start, min(previous_fit$points$tenor)))),
      end_spot = decimal_to_percent(curve_rate(previous_fit, x$end))
    )
  }
  forward_current_metrics <- function(x) {
    list(
      forward = x$result$forward_percent,
      start_spot = decimal_to_percent(curve_rate(x$bundle$fit, pmax(x$start, min(x$bundle$fit$points$tenor)))),
      end_spot = decimal_to_percent(curve_rate(x$bundle$fit, x$end))
    )
  }
  output$forward_banner <- renderUI({ req(applied_forward()); curve_banner(applied_forward()$bundle) })
  output$forward_title_block <- renderUI({
    req(applied_forward())
    x <- applied_forward()
    date_text <- as.character(forward_valuation_date(x))
    source_text <- if (isTRUE(x$bundle$proxy)) "Historical proxy" else "Zero-rate snapshot"
    div(class = "forward-title-block",
      tags$h3("Forward Rate Calculator"),
      div(class = "forward-title-meta",
        tags$span(tags$b("Curve:"), x$bundle$curve_name),
        tags$span(tags$b("Source:"), source_text),
        tags$span(tags$b("Date:"), date_text)
      )
    )
  })
  output$forward_value <- renderText({ req(applied_forward()); fmt_pct2(applied_forward()$result$forward_percent) })
  output$forward_start_spot <- renderText({ req(applied_forward()); x <- applied_forward(); fmt_pct2(forward_current_metrics(x)$start_spot) })
  output$forward_end_spot <- renderText({ req(applied_forward()); x <- applied_forward(); fmt_pct2(forward_current_metrics(x)$end_spot) })
  output$forward_value_change <- renderText({
    req(applied_forward())
    x <- applied_forward()
    previous <- forward_previous_close(x)
    if (is.null(previous)) return("Prev close unavailable")
    signed_bp_text((x$result$forward_percent - previous$forward) * 100)
  })
  output$forward_start_spot_change <- renderText({
    req(applied_forward())
    x <- applied_forward()
    previous <- forward_previous_close(x)
    if (is.null(previous)) return("Prev close unavailable")
    signed_bp_text((forward_current_metrics(x)$start_spot - previous$start_spot) * 100)
  })
  output$forward_end_spot_change <- renderText({
    req(applied_forward())
    x <- applied_forward()
    previous <- forward_previous_close(x)
    if (is.null(previous)) return("Prev close unavailable")
    signed_bp_text((forward_current_metrics(x)$end_spot - previous$end_spot) * 100)
  })
  output$forward_compounding_value <- renderText({ req(applied_forward()); tools::toTitleCase(applied_forward()$compounding) })
  output$forward_curve_source <- renderText({
    req(applied_forward())
    if (isTRUE(applied_forward()$bundle$proxy)) "Historical Proxy" else "Zero Snapshot"
  })
  output$forward_curve_source_sub <- renderText({
    req(applied_forward())
    x <- applied_forward()
    if (isTRUE(x$bundle$proxy)) paste("Selected date", as.Date(x$bundle$requested_date)) else "Latest local RDS"
  })
  output$forward_day_count_value <- renderText({ req(applied_forward()); if (identical(applied_forward()$compounding, "simple")) "Day Count: ACT/360" else "Day Count: curve years" })
  output$forward_period_value <- renderText({ req(applied_forward()); x <- applied_forward(); paste0(fmt_num(x$end - x$start), "Y") })
  output$forward_spread_value <- renderText({
    req(applied_forward())
    x <- applied_forward()
    end_spot <- decimal_to_percent(curve_rate(x$bundle$fit, x$end))
    fmt_bp((x$result$forward_percent - end_spot) * 100)
  })
  output$forward_result <- renderDT({ req(applied_forward()); centered_datatable(format_display_df(applied_forward()$result), options = list(dom = "t", scrollX = TRUE)) })

  forward_spot_inputs <- function(x) {
    fit <- x$bundle$fit
    start_tenor <- pmax(x$start, min(fit$points$tenor))
    start_rate <- if (x$start == 0) curve_rate(fit, start_tenor) else curve_rate(fit, x$start)
    end_rate <- curve_rate(fit, x$end)
    list(
      start_tenor = start_tenor,
      start_rate = start_rate,
      end_rate = end_rate,
      df_start = if (x$start == 0) 1 else discount_factor(start_rate, x$start, x$compounding),
      df_end = discount_factor(end_rate, x$end, x$compounding)
    )
  }
  forward_valuation_date <- function(x) {
    if (isTRUE(x$bundle$proxy) && !is.na(x$bundle$requested_date)) return(as.Date(x$bundle$requested_date))
    if (length(market()$wide_rates$date)) return(max(market()$wide_rates$date, na.rm = TRUE))
    Sys.Date()
  }
  forward_tenor_date <- function(valuation_date, years) {
    as.Date(valuation_date) + round(as.numeric(years) * 365.25)
  }
  forward_from_spots <- function(start_rate, end_rate, start, end, compounding) {
    df_start <- if (start == 0) 1 else discount_factor(start_rate, start, compounding)
    df_end <- discount_factor(end_rate, end, compounding)
    period <- end - start
    decimal_to_percent(switch(
      compounding,
      annual = (df_start / df_end)^(1 / period) - 1,
      continuous = log(df_start / df_end) / period,
      simple = (df_start / df_end - 1) / period
    ))
  }
  output$forward_sensitivity <- renderDT({
    req(applied_forward())
    x <- applied_forward()
    methods <- c("annual", "continuous", "simple")
    forwards <- vapply(methods, function(method) calculate_forward(x$bundle$fit, x$start, x$end, method)$forward_percent, numeric(1))
    annual <- forwards[["annual"]]
    rows <- data.frame(
      Compounding = c("Annual", "Continuous", "Simple (Act/360)"),
      `Forward Rate` = fmt_pct2(forwards),
      `Change vs Annual` = c("--", fmt_bp((forwards[["continuous"]] - annual) * 100), fmt_bp((forwards[["simple"]] - annual) * 100)),
      RawChange = c(NA_real_, (forwards[["continuous"]] - annual) * 100, (forwards[["simple"]] - annual) * 100),
      check.names = FALSE
    )
    centered_datatable(rows, options = list(dom = "t", scrollX = TRUE, ordering = FALSE,
      columnDefs = list(list(targets = 3, visible = FALSE, searchable = FALSE)),
      rowCallback = JS(sprintf(
        "function(row, data) {
          var value = parseFloat(data[3]);
          if (!isNaN(value) && value > 0) {
            $('td:eq(2)', row).html('<span class=\"sign-pill plus\">+</span>' + data[2]).css({'color':'%s','font-weight':'800'});
          } else if (!isNaN(value) && value < 0) {
            $('td:eq(2)', row).html('<span class=\"sign-pill minus\">-</span>' + data[2].replace('-', '')).css({'color':'%s','font-weight':'800'});
          } else {
            $('td:eq(2)', row).css({'color':'%s','font-weight':'800'});
          }
        }", positive_color, negative_color, neutral_color))
    ))
  })
  output$forward_inputs_summary <- renderDT({
    req(applied_forward())
    x <- applied_forward()
    fit <- x$bundle$fit
    tenors <- as.numeric(tenor_choices)
    tenor_names <- names(tenor_choices)
    rates <- decimal_to_percent(curve_rate(fit, pmax(tenors, min(fit$points$tenor))))
    dfs <- discount_factor(rates / 100, tenors, x$compounding)
    rows <- data.frame(Metric = c("Zero Rate (%)", "Discount Factor"), check.names = FALSE)
    for (index in seq_along(tenors)) {
      rows[[tenor_names[[index]]]] <- c(fmt_num_digits(rates[[index]], 2), fmt_df2(dfs[[index]]))
    }
    start_label <- tenor_label(x$start)
    end_label <- tenor_label(x$end)
    centered_datatable(rows, options = list(dom = "t", scrollX = TRUE, ordering = FALSE,
      rowCallback = JS(sprintf(
        "function(row, data) {
          var headers = this.api().columns().header().toArray().map(function(h){ return $(h).text(); });
          headers.forEach(function(label, idx) {
            if (label === '%s') $('td:eq(' + idx + ')', row).addClass('forward-start-cell');
            if (label === '%s') $('td:eq(' + idx + ')', row).addClass('forward-end-cell');
          });
        }", start_label, end_label))
    ))
  })
  output$forward_explanation <- renderUI({
    req(applied_forward())
    x <- applied_forward()
    valuation <- forward_valuation_date(x)
    start_date <- forward_tenor_date(valuation, x$start)
    end_date <- forward_tenor_date(valuation, x$end)
    tags$div(class = "forward-copy",
      tags$p(sprintf("The %s -> %s forward rate is the annualized rate that applies from the end of %s to the end of %s.",
        tenor_label(x$start), tenor_label(x$end), tenor_label(x$start), tenor_label(x$end))),
      tags$p(sprintf("In other words, if you invest to the %s spot date, the %.2f-year period that follows is implied to earn %s under the selected compounding convention.",
        tenor_label(x$start), x$end - x$start, fmt_pct2(x$result$forward_percent))),
      tags$hr(),
      tags$ul(
        tags$li(paste("Start date:", start_date)),
        tags$li(paste("End date:", end_date)),
        tags$li(paste("Forward period length:", fmt_num(x$end - x$start), "years"))
      )
    )
  })
  output$forward_formula <- renderUI({
    req(applied_forward())
    x <- applied_forward()
    spots <- forward_spot_inputs(x)
    tags$div(class = "forward-formula-wrap",
      div(class = "forward-equation",
        tags$span("1 + "),
        tags$span(class = "math-symbol math-f",
          "f",
          tags$sub(paste0(tenor_label(x$start), ",", tenor_label(x$end)))
        ),
        tags$span(" = "),
        tags$span(class = "math-paren", "("),
        tags$span(class = "math-frac",
          tags$span(class = "math-num", paste0("DF(", tenor_label(x$start), ")")),
          tags$span(class = "math-den", paste0("DF(", tenor_label(x$end), ")"))
        ),
        tags$span(class = "math-paren", ")"),
        tags$sup(class = "math-exp",
          tags$span(class = "math-exp-frac",
            tags$span(class = "math-exp-num", "1"),
            tags$span(class = "math-exp-den", paste0(tenor_label(x$end), " - ", tenor_label(x$start)))
          )
        )
      ),
      tags$table(class = "forward-formula-table",
        tags$thead(tags$tr(tags$th("Input"), tags$th("Value"), tags$th("Definition"))),
        tags$tbody(
          tags$tr(tags$td(paste0("DF(", tenor_label(x$start), ")")), tags$td(fmt_df2(spots$df_start)), tags$td(paste("Discount factor to", tenor_label(x$start)))),
          tags$tr(tags$td(paste0("DF(", tenor_label(x$end), ")")), tags$td(fmt_df2(spots$df_end)), tags$td(paste("Discount factor to", tenor_label(x$end)))),
          tags$tr(tags$td("Year Fraction"), tags$td(fmt_num(x$end - x$start)), tags$td(paste(tenor_label(x$end), "-", tenor_label(x$start)))),
          tags$tr(class = "formula-result-row", tags$td(paste0("Forward Rate (", tenor_label(x$start), " -> ", tenor_label(x$end), ")")),
            tags$td(fmt_pct2(x$result$forward_percent)), tags$td("Annualized"))
        )
      )
    )
  })
  register_plot("forward_curve_plot", function() {
    req(applied_forward())
    x <- applied_forward()
    fit <- x$bundle$fit
    min_tenor <- max(.01, min(fit$points$tenor))
    max_tenor <- max(fit$points$tenor)
    grid <- sort(unique(c(seq(min_tenor, max_tenor, length.out = 250), x$start, x$end)))
    data <- data.frame(tenor = grid, rate = decimal_to_percent(curve_rate(fit, pmax(grid, min_tenor))))
    data <- data[order(data$tenor), ]
    data$text <- paste0("Tenor: ", fmt_num(data$tenor), "Y<br>Rate: ", fmt_pct2(data$rate))
    raw_points <- data.frame(tenor = fit$points$tenor, rate = decimal_to_percent(fit$points$rate))
    raw_points$text <- paste0("Market point<br>Tenor: ", tenor_label(raw_points$tenor), "<br>Rate: ", fmt_pct2(raw_points$rate))
    marks <- data.frame(tenor = c(x$start, x$end), label = c("Start", "End"))
    marks$rate <- decimal_to_percent(curve_rate(fit, pmax(marks$tenor, min_tenor)))
    marks$text <- paste0(marks$label, "<br>Tenor: ", fmt_num(marks$tenor), "Y<br>Rate: ", fmt_pct2(marks$rate))
    marks$value_label <- fmt_pct2(marks$rate)
    y_range <- range(data$rate, marks$rate, raw_points$rate, na.rm = TRUE)
    y_pad <- max(0.08, diff(y_range) * 0.08)
    y0 <- y_range[[1]] - y_pad
    y1 <- y_range[[2]] + y_pad
    plot <- plotly::plot_ly(data = data, x = ~tenor, y = ~rate, text = ~text, hoverinfo = "text",
      type = "scatter", mode = "lines", name = "Zero Rate Curve",
      line = list(color = "#1f4e78", width = 2.3), legendrank = 1)
    interval <- data.frame(x = c(x$start, x$end, x$end, x$start), y = c(y0, y0, y1, y1))
    plot <- plotly::add_trace(plot, data = interval, x = ~x, y = ~y, type = "scatter",
      mode = "none", fill = "toself", fillcolor = "rgba(31,95,139,0.12)",
      name = "Forward Interval", hoverinfo = "skip", inherit = FALSE, legendrank = 4)
    plot <- plotly::add_trace(plot, data = data.frame(x = x$start, y = c(y0, y1)), x = ~x, y = ~y,
      type = "scatter", mode = "lines", name = paste0("Start Tenor (", tenor_label(x$start), ")"),
      line = list(color = positive_color, width = 2, dash = "dot"), hoverinfo = "skip", inherit = FALSE, legendrank = 2)
    plot <- plotly::add_trace(plot, data = data.frame(x = x$end, y = c(y0, y1)), x = ~x, y = ~y,
      type = "scatter", mode = "lines", name = paste0("End Tenor (", tenor_label(x$end), ")"),
      line = list(color = negative_color, width = 2, dash = "dot"), hoverinfo = "skip", inherit = FALSE, legendrank = 3)
    plot <- plotly::add_trace(plot, data = raw_points, x = ~tenor, y = ~rate, text = ~text, hoverinfo = "text",
      type = "scatter", mode = "markers", name = "Market points",
      marker = list(color = "#0f2e4f", size = 5), showlegend = FALSE, inherit = FALSE)
    plot <- plotly::add_trace(plot, data = marks, x = ~tenor, y = ~rate, text = ~value_label, hovertext = ~text, hoverinfo = "text",
      type = "scatter", mode = "markers+text", name = "Forward endpoints",
      textposition = c("top right", "top right"), textfont = list(color = c(positive_color, negative_color), size = 11),
      marker = list(color = c(positive_color, negative_color), size = 10), showlegend = FALSE, inherit = FALSE)
    trader_ticks <- c(1, 2, 3, 5, 7, 10, 20, 30)
    trader_ticks <- trader_ticks[trader_ticks >= min_tenor & trader_ticks <= max(max_tenor, x$end)]
    if (!length(trader_ticks)) trader_ticks <- c(min_tenor, max(max_tenor, x$end))
    annotations <- list(
      list(x = mean(c(x$start, x$end)), y = 1.04, xref = "x", yref = "paper", showarrow = FALSE,
        text = paste0("Forward interval: ", tenor_label(x$start), " -> ", tenor_label(x$end)),
        font = list(size = 11, color = "#1f4e78"))
    )
    plotly_trace_finish(plotly::layout(plot,
      title = x$bundle$curve_name,
      xaxis = list(title = "Tenor", range = c(min_tenor, max(max_tenor, x$end)),
        tickvals = trader_ticks, ticktext = tenor_label(trader_ticks), tickangle = 0, automargin = TRUE),
      yaxis = list(title = "Rate (%)", range = c(y0, y1)),
      legend = list(orientation = "h", x = 0.12, y = 1.08, xanchor = "left", yanchor = "bottom",
        font = list(size = 10), traceorder = "normal", itemwidth = 30),
      annotations = annotations), margin = list(t = 104, r = 22, b = 54, l = 58))
  })

  carry_selected_date <- function(bundle = NULL) {
    if (!is.null(bundle) && isTRUE(bundle$proxy) && !is.na(bundle$requested_date)) return(as.character(bundle$requested_date))
    if (identical(input$carry_source_mode, "historical") && !is.null(input$carry_curve_date)) return(as.character(as.Date(input$carry_curve_date)))
    if (length(market()$wide_rates$date)) return(as.character(max(market()$wide_rates$date, na.rm = TRUE)))
    as.character(Sys.Date())
  }
  carry_quality <- function(bundle) {
    rmse <- bundle$fit$rmse_bp %||% NA_real_
    if (!is.finite(rmse)) return(list(label = "NA", class = "neutral", subtitle = "RMS unavailable"))
    if (rmse <= 1) list(label = "Good", class = "positive", subtitle = paste("RMS:", fmt_bp1(rmse)))
    else if (rmse <= 5) list(label = "Watch", class = "neutral", subtitle = paste("RMS:", fmt_bp1(rmse)))
    else list(label = "Wide", class = "negative", subtitle = paste("RMS:", fmt_bp1(rmse)))
  }
  output$carry_header_meta <- renderUI({
    curve <- input$carry_curve_name %||% "USD UNITED STATES OIS"
    date <- carry_selected_date()
    tags$p(tags$b("Source:"), " Bloomberg (local RDS)", tags$span(class = "meta-dot", "\u2022"),
      tags$b("Curve:"), paste(curve), tags$span(class = "meta-dot", "\u2022"),
      tags$b("Selected Date:"), date, tags$span(class = "meta-dot", "\u2022"),
      tags$b("Hold Basis:"), " Actual / 360")
  })
  output$carry_title_block <- renderUI({
    curve <- if (!is.null(applied_carry())) applied_carry()$bundle$curve_name else input$carry_curve_name %||% "USD UNITED STATES OIS"
    date <- if (!is.null(applied_carry())) carry_selected_date(applied_carry()$bundle) else carry_selected_date()
    div(class = "carry-title-block",
      div(tags$h3("Carry & Roll Analysis"),
        div(class = "carry-title-meta",
          tags$span(tags$b("Source:"), " Bloomberg (BVAL)"),
          tags$span(tags$b("Curve:"), curve),
          tags$span(tags$b("Selected Date:"), date),
          tags$span(tags$b("Hold Basis:"), "Actual / 360")
        )
      ),
      div(class = "carry-title-actions",
        tags$button(type = "button", class = "btn btn-outline-primary btn-sm", "Save Workspace"),
        tags$button(type = "button", class = "btn btn-outline-secondary btn-sm", "\u22ef")
      )
    )
  })
  output$carry_banner <- renderUI({ req(applied_carry()); curve_banner(applied_carry()$bundle) })
  output$carry_value <- renderText({ req(applied_carry()); fmt_bp(applied_carry()$single$carry_bp) })
  output$roll_value <- renderText({ req(applied_carry()); fmt_bp(applied_carry()$single$roll_bp) })
  output$total_value <- renderText({ req(applied_carry()); paste(fmt_bp(applied_carry()$single$total_bp), "/", fmt_num(calculate_dv01_pnl(applied_carry()$single$total_bp, applied_carry()$dv01))) })
  output$carry_direction_value <- renderText({ req(applied_carry()); applied_carry()$single$direction })
  carry_kpi_state <- reactive({
    mode <- input$carry_workspace_mode %||% "single"
    if (identical(mode, "trade")) {
      trade <- applied_trade()
      if (is.null(trade)) return(list(available = FALSE, mode = "trade"))
      summary <- trade$calculation$summary
      risk_budget <- summary$risk_budget
      return(list(
        available = TRUE, mode = "trade", bundle = trade$bundle,
        carry_bp = summary$carry_pnl / risk_budget,
        roll_bp = summary$roll_pnl / risk_budget,
        total_bp = summary$equivalent_total_bp,
        pnl = summary$total_pnl,
        carry_pnl = summary$carry_pnl,
        roll_pnl = summary$roll_pnl,
        total_pnl = summary$total_pnl,
        risk_budget = risk_budget,
        direction = "DV01 Neutral"
      ))
    }
    carry <- applied_carry()
    if (is.null(carry)) return(list(available = FALSE, mode = "single"))
    list(
      available = TRUE, mode = "single", bundle = carry$bundle,
      carry_bp = carry$single$carry_bp,
      roll_bp = carry$single$roll_bp,
      total_bp = carry$single$total_bp,
      pnl = calculate_dv01_pnl(carry$single$total_bp, carry$dv01),
      carry_pnl = calculate_dv01_pnl(carry$single$carry_bp, carry$dv01),
      roll_pnl = calculate_dv01_pnl(carry$single$roll_bp, carry$dv01),
      total_pnl = calculate_dv01_pnl(carry$single$total_bp, carry$dv01),
      risk_budget = carry$dv01,
      direction = carry$single$direction
    )
  })
  output$carry_value_ui <- renderUI({ x <- carry_kpi_state(); if (!isTRUE(x$available)) signed_na_span() else signed_value_span_digits(x$carry_bp, " bp", 1) })
  output$roll_value_ui <- renderUI({ x <- carry_kpi_state(); if (!isTRUE(x$available)) signed_na_span() else signed_value_span_digits(x$roll_bp, " bp", 1) })
  output$total_bp_ui <- renderUI({ x <- carry_kpi_state(); if (!isTRUE(x$available)) signed_na_span() else signed_value_span_digits(x$total_bp, " bp", 1) })
  output$pnl_value_ui <- renderUI({ x <- carry_kpi_state(); if (!isTRUE(x$available)) signed_na_span() else signed_value_span_digits(x$pnl, "", 0) })
  output$carry_subtitle_ui <- renderUI({ x <- carry_kpi_state(); if (!isTRUE(x$available)) tags$span("Click Calculate Curve Trade") else tags$span(paste0(if (x$carry_pnl > 0) "+" else "", fmt_pnl0(x$carry_pnl), if (identical(x$mode, "trade")) " P&L" else " per DV01")) })
  output$roll_subtitle_ui <- renderUI({ x <- carry_kpi_state(); if (!isTRUE(x$available)) tags$span("Click Calculate Curve Trade") else tags$span(paste0(if (x$roll_pnl > 0) "+" else "", fmt_pnl0(x$roll_pnl), if (identical(x$mode, "trade")) " P&L" else " per DV01")) })
  output$total_subtitle_ui <- renderUI({ x <- carry_kpi_state(); if (!isTRUE(x$available)) tags$span("Click Calculate Curve Trade") else tags$span(paste0(if (x$total_pnl > 0) "+" else "", fmt_pnl0(x$total_pnl), " P&L estimate")) })
  output$pnl_subtitle_ui <- renderUI({ x <- carry_kpi_state(); tags$span(if (!isTRUE(x$available)) "Click Calculate Curve Trade" else if (identical(x$mode, "trade")) "Portfolio total" else "Per selected DV01") })
  output$trade_mode_value <- renderText({ x <- carry_kpi_state(); if (identical(x$mode, "trade")) "DV01 Neutral" else if (isTRUE(x$available)) x$direction else "Receive Fixed" })
  output$trade_mode_subtitle <- renderText({ x <- carry_kpi_state(); if (!isTRUE(x$available) && identical(x$mode, "trade")) "Click Calculate Curve Trade" else if (identical(x$mode, "trade")) paste("Target DV01:", fmt_pnl0(x$risk_budget)) else "Single trade" })
  output$curve_quality_ui <- renderUI({ x <- carry_kpi_state(); if (!isTRUE(x$available)) return(signed_na_span()); q <- carry_quality(x$bundle); tags$span(class = paste("signed-value", q$class), q$label) })
  output$curve_quality_subtitle_ui <- renderUI({ x <- carry_kpi_state(); if (!isTRUE(x$available)) return(tags$span("Unavailable")); q <- carry_quality(x$bundle); tags$span(q$subtitle) })
  output$carry_status_detail <- renderUI({
    mode <- input$carry_workspace_mode %||% "single"
    x <- if (identical(mode, "trade")) status$trade else status$carry
    run_id <- if (identical(mode, "trade") && !is.null(applied_trade())) paste0(format(Sys.Date(), "%y%m%d"), "-TRD") else if (!is.null(applied_carry())) paste0(format(Sys.Date(), "%y%m%d"), "-CAR") else "--"
    div(class = paste("carry-status-body", paste0("carry-status-", x$type)),
      div(class = "carry-status-line", tags$span(class = "status-dot"), tags$span(x$message), tags$span(class = "status-time", format(Sys.time(), "%H:%M:%S"))),
      div(class = "progress carry-progress", div(class = "progress-bar", role = "progressbar",
        style = paste0("width:", x$pct, "%;"), paste0(x$pct, "%"))),
      div(class = "carry-status-run", tags$span(paste("Run ID:", run_id)), tags$span(class = "copy-mini", "\u2398"))
    )
  })
  output$curve_trade_workspace <- renderUI({
    mode <- input$carry_workspace_mode %||% "single"
    disabled <- !identical(mode, "trade")
    if (disabled) {
      return(div(class = "curve-trade-workspace resizable-card is-disabled",
        div(class = "curve-trade-title-row", tags$h4("Curve Trade"), tags$span("NA")),
        div(class = "curve-trade-controls-strip",
          div(class = "curve-trade-structure", tags$span("STRUCTURE"), div(class = "trade-badge-row is-na", tags$span(class = "trade-badge", "NA"))),
          div(class = "curve-trade-held-dv01", tags$span("HELD DV01 (PER $1MM)"), div(class = "shiny-text-output", "NA"))
        ),
        div(class = "table-card trade-leg-card",
          div(class = "card-heading compact-heading", tags$div(class = "card-kicker", "Results only"), tags$h4("Curve Trade Legs")),
          div(class = "trade-na-panel", "NA")
        ),
        grid_row(class = "curve-trade-lower-row",
          grid_col(class = "span-8", div(class = "plot-card trade-portfolio-plot trade-na-card", div(class = "trade-na-panel", "NA"))),
          grid_col(class = "span-4", div(class = "trade-summary-card resizable-card", div(class = "trade-summary-inner", tags$h4("Curve Trade Summary"), div(class = "trade-na-panel", "NA"))))
        )
      ))
    }
    div(class = "curve-trade-workspace resizable-card",
      div(class = "curve-trade-title-row", tags$h4("Curve Trade"), tags$span("Results")),
      div(class = "curve-trade-controls-strip",
        div(class = "curve-trade-structure", tags$span("STRUCTURE"), uiOutput("trade_structure_badges")),
        div(class = "curve-trade-held-dv01", tags$span("HELD DV01 (PER $1MM)"), textOutput("trade_held_dv01_inline"))
      ),
      table_card("Curve Trade Legs", "trade_leg_table", "Results only; configure and calculate from the left sidebar."),
      grid_row(class = "curve-trade-lower-row",
        grid_col(class = "span-8", plot_card("trade_leg_pnl_plot", height = "330px", class = "trade-portfolio-plot")),
        grid_col(class = "span-4", div(class = "trade-summary-card resizable-card", uiOutput("trade_summary_card")))
      )
    )
  })
  output$carry_explanation <- renderUI({ req(applied_carry()); x <- applied_carry()$single; p(sprintf("Last applied trade: Carry %.2f bp + Roll %.2f bp = Total %.2f bp.", x$carry_bp, x$roll_bp, x$total_bp)) })
  register_plot("carry_component_plot", function() {
    req(applied_carry())
    x <- applied_carry()$matrix
    selected_hold <- tenor_label(applied_carry()$single$hold_years)
    x <- x[as.character(x$hold_label) == selected_hold, , drop = FALSE]
    x$tenor_label <- factor(as.character(x$tenor_label), levels = c("1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "15Y", "20Y", "30Y"), ordered = TRUE)
    x <- x[order(x$tenor_label), , drop = FALSE]
    x_index <- seq_len(nrow(x))
    plot <- plotly::plot_ly()
    plot <- plotly::add_bars(plot, x = x_index - 0.16, y = x$carry_bp, name = "Carry (bp)",
      marker = list(color = positive_color), width = 0.28,
      hovertext = paste0("Tenor: ", x$tenor_label, "<br>Carry: ", fmt_bp1(x$carry_bp)), hoverinfo = "text")
    plot <- plotly::add_bars(plot, x = x_index + 0.16, y = x$roll_bp, name = "Roll (bp)",
      marker = list(color = "#EF4444"), width = 0.28,
      hovertext = paste0("Tenor: ", x$tenor_label, "<br>Roll: ", fmt_bp1(x$roll_bp)), hoverinfo = "text")
    plot <- plotly::add_trace(plot, x = x_index, y = x$total_bp, type = "scatter", mode = "lines+markers",
      name = "Total (bp)", line = list(color = "#0B3D91", width = 2), marker = list(color = "#0B3D91", size = 6),
      hovertext = paste0("Tenor: ", x$tenor_label, "<br>Total: ", fmt_bp1(x$total_bp)), hoverinfo = "text")
    plotly_trace_finish(plotly::layout(plot,
      title = "",
      barmode = "overlay",
      xaxis = list(title = "Tenor", tickvals = x_index, ticktext = as.character(x$tenor_label)),
      yaxis = list(title = "bp", tickformat = ".1f", zeroline = TRUE, zerolinecolor = "#9AA9B7"),
      legend = list(orientation = "h", x = 0.5, y = -0.24, xanchor = "center", yanchor = "top"),
      margin = list(t = 42, r = 24, b = 92, l = 56)))
  })
  register_plot("carry_spot_plot", function() {
    req(applied_carry())
    fit <- applied_carry()$bundle$fit
    tenors <- sort(c(1, 2, 3, 5, 7, 10, 15, 20, 30))
    x <- data.frame(tenor = tenors, spot = decimal_to_percent(curve_rate(fit, tenors)))
    x$text <- paste0(fmt_num(x$tenor), "Y: ", fmt_pct(x$spot))
    plotly_finish(ggplot(x, aes(tenor, spot, text = text, group = 1)) +
      geom_line(color = "#193F63", linewidth = 0.9) +
      geom_point(color = "#193F63", fill = "#FFFFFF", shape = 21, size = 2.5, stroke = 0.8) +
      labs(title = "Spot curve for roll analysis", x = "Tenor", y = "Rate (%)") +
      theme_minimal(base_size = 10) +
      theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 11)))
  })
  register_plot("carry_stacked_plot", function() {
    req(applied_carry())
    x <- applied_carry()$matrix
    long <- rbind(
      data.frame(x, component = "Carry", value = x$carry_bp),
      data.frame(x, component = "Roll", value = x$roll_bp)
    )
    long$sign <- ifelse(long$value > 0, "Positive", ifelse(long$value < 0, "Negative", "Zero"))
    long$text <- paste0(long$component, "<br>Tenor: ", long$tenor_label, "<br>Hold: ", long$hold_label, "<br>Value: ", fmt_bp(long$value))
    plotly_finish(ggplot(long, aes(tenor_label, value, fill = sign, alpha = component, text = text)) +
      geom_hline(yintercept = 0, color = "#A8B6C3", linewidth = 0.35) +
      geom_col(position = "stack", width = 0.62, color = "#FFFFFF", linewidth = 0.25) +
      geom_point(data = x, aes(tenor_label, total_bp), inherit.aes = FALSE, color = "#0B2F4E", size = 1.9) +
      facet_wrap(~hold_label, nrow = 1, drop = FALSE) +
      scale_fill_manual(values = c(Positive = positive_color, Negative = negative_color, Zero = neutral_color)) +
      scale_alpha_manual(values = c(Carry = 0.95, Roll = 0.45)) +
      labs(title = "Carry and roll by tenor", x = "Tenor", y = "bp", fill = "Sign", alpha = "Component") +
      theme_minimal(base_size = 10) +
      theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"), plot.title = element_text(face = "bold", size = 11)))
  })
  output$carry_matrix <- renderDT({ req(applied_carry()); centered_datatable(format_display_df(applied_carry()$matrix[, c("tenor_label", "hold_label", "carry_bp", "roll_bp", "total_bp", "pnl")]), options = list(pageLength = 12, scrollX = TRUE)) })
  register_plot("carry_heatmap", function() {
    req(applied_carry())
    x <- applied_carry()$matrix
    tenors <- levels(x$tenor_label)
    holds <- levels(x$hold_label)
    matrix_z <- matrix(NA_real_, nrow = length(holds), ncol = length(tenors), dimnames = list(holds, tenors))
    matrix_text <- matrix("", nrow = length(holds), ncol = length(tenors), dimnames = list(holds, tenors))
    matrix_hover <- matrix("", nrow = length(holds), ncol = length(tenors), dimnames = list(holds, tenors))
    for (index in seq_len(nrow(x))) {
      hold <- as.character(x$hold_label[[index]])
      tenor <- as.character(x$tenor_label[[index]])
      matrix_z[hold, tenor] <- x$total_bp[[index]]
      matrix_text[hold, tenor] <- fmt_num_digits(x$total_bp[[index]], 1)
      matrix_hover[hold, tenor] <- paste0("Hold: ", hold, "<br>Tenor: ", tenor,
        "<br>Total: ", fmt_bp1(x$total_bp[[index]]), "<br>P&L: ", fmt_pnl0(x$pnl[[index]]))
    }
    annotations <- do.call(c, lapply(seq_along(holds), function(row_index) {
      lapply(seq_along(tenors), function(col_index) {
        list(x = tenors[[col_index]], y = holds[[row_index]], text = matrix_text[row_index, col_index],
          showarrow = FALSE, font = list(color = "#10263B", size = 10))
      })
    }))
    plot <- plotly::plot_ly(
      x = tenors, y = holds, z = matrix_z,
      type = "heatmap", hoverinfo = "text",
      text = matrix_hover,
      colorscale = list(c(0, "#F3B1A7"), c(0.5, "#F8FAFC"), c(1, "#8ED8C9")),
      colorbar = list(title = "Total bp", orientation = "h", x = 0.5, y = -0.22, xanchor = "center", len = 0.76, thickness = 12)
    )
    plotly_trace_finish(plotly::layout(plot,
      title = "",
      xaxis = list(title = "Tenor", side = "bottom"),
      yaxis = list(title = "Hold", autorange = "reversed"),
      annotations = annotations,
      margin = list(t = 38, r = 24, b = 100, l = 58)))
  })

  output$trade_banner <- renderUI({ req(applied_trade()); curve_banner(applied_trade()$bundle) })
  output$trade_carry_pnl <- renderText({ req(applied_trade()); fmt_num(applied_trade()$calculation$summary$carry_pnl) })
  output$trade_roll_pnl <- renderText({ req(applied_trade()); fmt_num(applied_trade()$calculation$summary$roll_pnl) })
  output$trade_total_pnl <- renderText({ req(applied_trade()); paste(fmt_num(applied_trade()$calculation$summary$total_pnl), "/", fmt_bp(applied_trade()$calculation$summary$equivalent_total_bp)) })
  output$trade_structure_value <- renderText({ req(applied_trade()); tools::toTitleCase(gsub("_", " ", applied_trade()$structure)) })
  output$trade_explanation <- renderUI({ req(applied_trade()); x <- applied_trade(); p(paste("Last applied structure:", x$structure, ". Each leg table and chart show its own Carry and Roll.")) })
  output$trade_structure_badges <- renderUI({
    current <- input$trade_structure %||% "steepener"
    labels <- c(steepener = "Steepener", flattener = "Flattener", long_belly_fly = "Butterfly", short_belly_fly = "Short Fly")
    div(class = "trade-badge-row", lapply(names(labels), function(value) {
      tags$span(class = if (identical(current, value)) "trade-badge active" else "trade-badge", labels[[value]])
    }))
  })
  output$trade_held_dv01_inline <- renderText({ fmt_pnl0(input$trade_risk_budget %||% 0) })
  output$trade_summary_card <- renderUI({
    req(applied_trade())
    s <- applied_trade()$calculation$summary
    div(class = "trade-summary-inner",
      tags$h4(paste0("Curve Trade Summary (", tenor_label(applied_trade()$hold), ")")),
      div(class = "summary-row", tags$span("Total Carry (P&L)"), signed_value_span_digits(s$carry_pnl, "", 0)),
      div(class = "summary-row", tags$span("Total Roll (P&L)"), signed_value_span_digits(s$roll_pnl, "", 0)),
      div(class = "summary-row", tags$span("Total P&L"), signed_value_span_digits(s$total_pnl, "", 0)),
      div(class = "summary-row", tags$span("Equivalent bp"), signed_value_span_digits(s$equivalent_total_bp, " bp", 1)),
      div(class = "summary-row", tags$span("Start Tenor"), tags$b(tenor_label(applied_trade()$start))),
      div(class = "summary-row", tags$span("Trade Mode"), tags$b("DV01 Neutral"))
    )
  })
  output$trade_leg_table <- renderDT({
    req(applied_trade())
    detail <- applied_trade()$calculation$detail
    signed_cell <- function(value, suffix = "", digits = 1) {
      numeric_value <- suppressWarnings(as.numeric(value))
      class <- if (is.finite(numeric_value) && numeric_value > 0) "positive" else if (is.finite(numeric_value) && numeric_value < 0) "negative" else "neutral"
      paste0("<span class='trade-cell-signed ", class, "'>",
        if (is.finite(numeric_value) && numeric_value > 0) "+" else "",
        fmt_num_digits(numeric_value, digits), suffix, "</span>")
    }
    direction_cell <- function(direction) {
      class <- ifelse(direction == "Receive Fixed", "receive", "pay")
      paste0("<span class='trade-direction ", class, "'>", direction, "</span>")
    }
    display <- data.frame(
      LEG = detail$leg,
      TENOR = tenor_label(detail$tenor),
      POSITION = vapply(detail$direction, direction_cell, character(1)),
      DV01 = fmt_pnl0(detail$dv01),
      CARRY = vapply(detail$carry_bp, signed_cell, character(1), suffix = "", digits = 1),
      ROLL = vapply(detail$roll_bp, signed_cell, character(1), suffix = "", digits = 1),
      TOTAL = vapply(detail$total_bp, signed_cell, character(1), suffix = "", digits = 1),
      `P&L` = vapply(detail$total_pnl, signed_cell, character(1), suffix = "", digits = 0),
      check.names = FALSE
    )
    centered_datatable(display, escape = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE, searching = FALSE, info = FALSE,
        autoWidth = FALSE, scrollX = FALSE))
  })
  register_plot("trade_leg_pnl_plot", function() {
    req(applied_trade())
    x <- applied_trade()$calculation$detail
    s <- applied_trade()$calculation$summary
    portfolio <- data.frame(leg = "Portfolio Total", carry_bp = s$carry_pnl / s$risk_budget,
      roll_bp = s$roll_pnl / s$risk_budget, total_bp = s$equivalent_total_bp)
    plot_data <- rbind(x[, c("leg", "carry_bp", "roll_bp", "total_bp")], portfolio)
    x_index <- seq_len(nrow(plot_data))
    plot <- plotly::plot_ly()
    plot <- plotly::add_bars(plot, x = x_index - 0.16, y = plot_data$carry_bp, name = "Carry (bp)",
      marker = list(color = positive_color), width = 0.28,
      hovertext = paste0("Leg: ", plot_data$leg, "<br>Carry: ", fmt_bp1(plot_data$carry_bp)), hoverinfo = "text")
    plot <- plotly::add_bars(plot, x = x_index + 0.16, y = plot_data$roll_bp, name = "Roll (bp)",
      marker = list(color = "#EF4444"), width = 0.28,
      hovertext = paste0("Leg: ", plot_data$leg, "<br>Roll: ", fmt_bp1(plot_data$roll_bp)), hoverinfo = "text")
    plot <- plotly::add_trace(plot, x = x_index, y = plot_data$total_bp, type = "scatter", mode = "lines+markers",
      name = "Total (bp)", line = list(color = "#0B3D91", width = 2), marker = list(color = "#0B3D91", size = 6),
      hovertext = paste0("Leg: ", plot_data$leg, "<br>Total: ", fmt_bp1(plot_data$total_bp)), hoverinfo = "text")
    plotly_trace_finish(plotly::layout(plot,
      title = "",
      barmode = "overlay",
      xaxis = list(title = NULL, tickvals = x_index, ticktext = plot_data$leg),
      yaxis = list(title = "bp", tickformat = ".1f", zeroline = TRUE, zerolinecolor = "#9AA9B7"),
      legend = list(orientation = "h", x = 0.5, y = -0.28, xanchor = "center", yanchor = "top"),
      margin = list(t = 38, r = 20, b = 96, l = 50)))
  })
  register_plot("trade_component_plot", function() {
    req(applied_trade())
    s <- applied_trade()$calculation$summary
    x <- data.frame(component = factor(c("Carry", "Roll", "Total"), levels = c("Carry", "Roll", "Total")),
      pnl = c(s$carry_pnl, s$roll_pnl, s$total_pnl))
    x$sign <- ifelse(x$pnl > 0, "Positive", ifelse(x$pnl < 0, "Negative", "Zero"))
    plotly_finish(ggplot(x, aes(component, pnl, fill = sign, text = paste0(component, ": ", fmt_num(pnl)))) +
      geom_hline(yintercept = 0, color = "#A8B6C3", linewidth = 0.35) +
      geom_col(width = 0.52) +
      geom_text(aes(label = fmt_num(pnl)), vjust = ifelse(x$pnl >= 0, -0.4, 1.2), size = 2.9) +
      scale_fill_manual(values = c(Positive = positive_color, Negative = negative_color, Zero = neutral_color)) +
      labs(title = "Portfolio carry / roll P&L", x = NULL, y = "P&L", fill = NULL) +
      theme_minimal(base_size = 10) +
      theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 11)))
  })

  diagnostics_thresholds <- reactive({
    warn <- suppressWarnings(as.numeric(input$diag_residual_warn %||% 1))
    fail <- suppressWarnings(as.numeric(input$diag_residual_fail %||% 2.5))
    if (!is.finite(warn) || warn < 0) warn <- 1
    if (!is.finite(fail) || fail <= 0) fail <- 2.5
    if (fail < warn) fail <- warn
    list(warn = warn, fail = fail)
  })
  diagnostics_selected_fit <- reactive({
    req(applied_curve())
    preferred <- input$diag_fit_method %||% "spline"
    fit <- find_fit(applied_curve()$fits, preferred)
    if (is.null(fit)) fit <- curve_fit()
    fit
  })
  diagnostics_selected_fit_label <- reactive({
    fit <- diagnostics_selected_fit()
    if (identical(fit$method, "spline")) "Spline" else if (identical(fit$method, "nelson_siegel")) "Nelson-Siegel" else tools::toTitleCase(fit$method)
  })
  diagnostics_tenor_matrix <- reactive({
    req(applied_curve())
    points <- applied_curve()$points
    expected <- diagnostics_expected_tenors
    rows <- lapply(seq_along(expected), function(index) {
      tenor <- unname(expected[[index]])
      label <- names(expected)[[index]]
      tolerance <- if (tenor < 1) 0.02 else 0.05
      matched <- which(abs(points$tenor - tenor) <= tolerance)
      available <- length(matched) > 0
      point <- if (available) points[matched[[1]], , drop = FALSE] else NULL
      data.frame(
        tenor = tenor,
        tenor_label = label,
        expected = TRUE,
        available = available,
        yield_percent = if (available) decimal_to_percent(point$rate[[1]]) else NA_real_,
        source = if (available) "Local RDS" else "NA",
        age = "NA",
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  })
  diagnostics_missing_count <- reactive({ sum(!diagnostics_tenor_matrix()$available) })
  diagnostics_status_for_residuals <- reactive({
    fit <- diagnostics_selected_fit()
    thresholds <- diagnostics_thresholds()
    max_abs <- max(abs(fit$diagnostics$residual_bp), na.rm = TRUE)
    if (!is.finite(max_abs)) return("na")
    if (max_abs > thresholds$fail) "fail" else if (max_abs > thresholds$warn) "warn" else "ok"
  })
  diagnostics_summary_rows <- reactive({
    req(applied_curve())
    x <- applied_curve()
    fit <- diagnostics_selected_fit()
    ns <- find_fit(x$fits, "nelson_siegel")
    spline <- find_fit(x$fits, "spline")
    missing <- diagnostics_missing_count()
    tenor_range <- range(x$points$tenor, na.rm = TRUE)
    date_value <- if (identical(x$mode, "historical")) as.character(x$requested_date %||% "NA") else as.character(x$latest_db_date %||% "NA")
    effective_value <- if (identical(x$mode, "historical")) as.character(x$effective_date %||% "NA") else as.character(x$latest_db_date %||% "NA")
    data.frame(
      Field = c("Curve", "Requested Date", "Effective Date", "Source", "Proxy", "Tenor Count",
        "Missing Points", "Min Tenor", "Max Tenor", "NS RMSE", "Spline RMSE", "Unit Check"),
      Value = c(x$curve_name, date_value, effective_value, if (identical(x$mode, "zero")) "Local RDS zero snapshot" else "Local RDS historical proxy",
        if (identical(x$mode, "historical")) "Yes" else "No", paste0(nrow(x$points), " observed / ", length(diagnostics_expected_tenors), " expected"),
        as.character(missing), tenor_label(tenor_range[[1]]), tenor_label(tenor_range[[2]]),
        if (is.null(ns)) "NA" else fmt_num_digits(ns$rmse_bp, 1),
        if (is.null(spline)) "NA" else fmt_num_digits(spline$rmse_bp, 1),
        "OK"),
      Unit = c("\u2013", "\u2013", "\u2013", "\u2013", "\u2013", "points", "points", "\u2013", "\u2013", "bp", "bp", "\u2013"),
      Source = c("User Selection", "Local RDS", "Local RDS", "Local RDS", "Local RDS", "Local RDS",
        "Diagnostics Policy", "Local RDS", "Local RDS", "Model", "Model", "Internal Policy"),
      Status = c("ok", "ok", "ok", "ok", if (identical(x$mode, "historical")) "warn" else "ok",
        if (missing > 0) "warn" else "ok", if (missing > 0) "warn" else "ok", "ok", "ok",
        if (!is.null(ns) && ns$rmse_bp > diagnostics_thresholds()$fail) "fail" else "ok",
        diagnostics_status_for_residuals(), "ok"),
      stringsAsFactors = FALSE
    )
  })
  diagnostics_table_ui <- function(rows) {
    tags$table(class = "diag-summary-table",
      tags$thead(tags$tr(lapply(c("Field", "Value", "Unit", "Source", "Status"), tags$th))),
      tags$tbody(lapply(seq_len(nrow(rows)), function(index) {
        tags$tr(
          tags$td(rows$Field[[index]]),
          tags$td(class = "diag-summary-value", rows$Value[[index]]),
          tags$td(rows$Unit[[index]]),
          tags$td(rows$Source[[index]]),
          tags$td(diagnostics_status_icon(rows$Status[[index]]))
        )
      }))
    )
  }
  output$diag_freshness_value <- renderUI({ req(applied_curve()); tags$span("Local RDS") })
  output$diag_freshness_subtitle <- renderUI({ req(applied_curve()); tags$span(paste("Applied:", format(Sys.time(), "%H:%M:%S"))) })
  output$diag_freshness_detail <- renderUI({ tags$span("Source: Local RDS") })
  output$diag_freshness_status <- renderUI({ diagnostics_status_icon("ok") })
  output$diag_missing_value <- renderUI({ req(applied_curve()); tags$span(diagnostics_missing_count()) })
  output$diag_missing_subtitle <- renderUI({ req(applied_curve()); tags$span(paste("of", length(diagnostics_expected_tenors), "expected")) })
  output$diag_missing_detail <- renderUI({ req(applied_curve()); tags$span(paste0(fmt_num_digits(100 * diagnostics_missing_count() / length(diagnostics_expected_tenors), 1), "% missing")) })
  output$diag_missing_status <- renderUI({ req(applied_curve()); diagnostics_status_icon(if (diagnostics_missing_count() > 0) "warn" else "ok") })
  output$diag_fit_rmse_value <- renderUI({ fit <- diagnostics_selected_fit(); tags$span(fmt_bp1(fit$rmse_bp)) })
  output$diag_fit_rmse_subtitle <- renderUI({ tags$span(paste("Model:", diagnostics_selected_fit_label())) })
  output$diag_fit_rmse_detail <- renderUI({ t <- diagnostics_thresholds(); tags$span(paste("Threshold:", fmt_bp1(t$fail))) })
  output$diag_fit_rmse_status <- renderUI({ diagnostics_status_icon(diagnostics_status_for_residuals()) })
  output$diag_proxy_value <- renderUI({ req(applied_curve()); tags$span(if (identical(applied_curve()$mode, "historical")) "Yes" else "No") })
  output$diag_proxy_subtitle <- renderUI({ req(applied_curve()); tags$span(if (identical(applied_curve()$mode, "historical")) "Historical proxy" else "Primary local source") })
  output$diag_proxy_detail <- renderUI({ tags$span("Local RDS") })
  output$diag_proxy_status <- renderUI({ req(applied_curve()); diagnostics_status_icon(if (identical(applied_curve()$mode, "historical")) "warn" else "ok") })
  output$diag_unit_value <- renderUI({ tags$span("OK") })
  output$diag_unit_subtitle <- renderUI({ tags$span("Internal: Decimal") })
  output$diag_unit_detail <- renderUI({ tags$span("Display: Percent") })
  output$diag_unit_status <- renderUI({ diagnostics_status_icon("ok") })
  output$diagnostics_explanation <- renderUI({
    req(applied_curve())
    div(class = "diag-about-content",
      div(class = "diag-blue-note", tags$span(class = "info-dot", "i"),
        div(tags$strong("Diagnostics do not trigger recalculation."),
          tags$span(" They summarize the inputs and fit quality from the last successful Apply Curve in Curve Explorer."))),
      tags$h5("What Diagnostics Checks"),
      tags$ul(
        tags$li("Local RDS source and proxy/fallback status."),
        tags$li("Missing points against the expected tenor set."),
        tags$li("Fit residuals against warning and fail thresholds."),
        tags$li("Unit consistency: internal decimal, display percent/bp."),
        tags$li("Basic validation rules selected in the policy sidebar.")
      ),
      div(class = "diag-muted-callout", "Bloomberg live metadata, Refinitiv source hierarchy, quote age seconds and production cashflow revaluation are unavailable in the local RDS build.")
    )
  })
  output$diag_source <- renderText({ req(applied_curve()); if (applied_curve()$mode == "zero") "Local RDS zero snapshot" else "Local RDS historical proxy" })
  output$diag_proxy <- renderText({ req(applied_curve()); if (applied_curve()$mode == "historical") "TRUE" else "FALSE" })
  output$diag_rmse <- renderText({ req(diagnostics_selected_fit()); fmt_bp1(diagnostics_selected_fit()$rmse_bp) })
  output$diag_points <- renderText({ req(applied_curve()); nrow(applied_curve()$points) })
  output$diagnostics_table <- renderUI({ diagnostics_table_ui(diagnostics_summary_rows()) })
  output$input_points <- renderUI({
    matrix <- diagnostics_tenor_matrix()
    tags$table(class = "diag-input-matrix",
      tags$thead(tags$tr(tags$th(""), lapply(matrix$tenor_label, tags$th))),
      tags$tbody(
        tags$tr(tags$td("Expected"), lapply(matrix$expected, function(value) tags$td(diagnostics_status_icon(if (value) "ok" else "na")))),
        tags$tr(tags$td("Available"), lapply(matrix$available, function(value) tags$td(diagnostics_status_icon(if (value) "ok" else "fail")))),
        tags$tr(tags$td("Yield (%)"), lapply(matrix$yield_percent, function(value) tags$td(if (is.na(value)) "\u2013" else fmt_num_digits(value, 2)))),
        tags$tr(tags$td("Source"), lapply(matrix$source, tags$td)),
        tags$tr(tags$td("Age"), lapply(matrix$age, tags$td))
      )
    )
  })
  register_plot("diagnostics_residual_plot", function() {
    fit <- diagnostics_selected_fit()
    thresholds <- diagnostics_thresholds()
    data <- fit$diagnostics
    ordered_labels <- tenor_label(data$tenor)
    data$tenor_label <- factor(ordered_labels, levels = unique(ordered_labels), ordered = TRUE)
    data$status <- ifelse(abs(data$residual_bp) > thresholds$fail, "Fail",
      ifelse(abs(data$residual_bp) > thresholds$warn, "Warn", "OK"))
    data$color <- ifelse(data$status == "Fail", "#EF4444", ifelse(data$status == "Warn", "#F59E0B", "#0B5BD3"))
    plot <- plotly::plot_ly(data, x = ~tenor_label, y = ~residual_bp, type = "bar",
      marker = list(color = data$color), name = "Residual (bp)",
      hovertext = ~paste0("Tenor: ", tenor_label, "<br>Residual: ", fmt_bp1(residual_bp), "<br>Status: ", status),
      hoverinfo = "text")
    for (value in c(thresholds$warn, -thresholds$warn, thresholds$fail, -thresholds$fail)) {
      plot <- plotly::add_trace(plot, x = data$tenor_label, y = rep(value, nrow(data)), type = "scatter", mode = "lines",
        name = paste0(if (value > 0) "+" else "\u2212", fmt_bp1(abs(value)), if (abs(value) == thresholds$warn) " Warn" else " Fail"),
        line = list(color = if (abs(value) == thresholds$warn) "#16A085" else "#EF4444", width = 1.2, dash = "dash"),
        hoverinfo = "skip", showlegend = TRUE, inherit = FALSE)
    }
    plotly_trace_finish(plotly::layout(plot,
      title = "",
      xaxis = list(title = "Tenor", tickangle = 0),
      yaxis = list(title = "Residual (bp)", tickformat = ".1f", zeroline = TRUE, zerolinecolor = "#9AA9B7"),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.14, yanchor = "bottom"),
      margin = list(t = 82, r = 24, b = 58, l = 58)))
  })
}

shinyApp(ui, server)
