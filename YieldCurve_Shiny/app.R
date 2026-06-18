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

theme <- bs_theme(version = 5, bootswatch = "flatly", primary = "#234B68")
positive_color <- "#16A085"
negative_color <- "#D96C5F"
neutral_color <- "#61758A"

fmt_num <- function(x) format(round(as.numeric(x), 2), nsmall = 0, trim = TRUE, scientific = FALSE, big.mark = ",")
fmt_pct <- function(x) paste0(fmt_num(x), "%")
fmt_bp <- function(x) paste0(fmt_num(x), " bp")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
round_numeric_df <- function(x) {
  numeric_columns <- vapply(x, is.numeric, logical(1))
  x[numeric_columns] <- lapply(x[numeric_columns], round, 2)
  x
}
value_color <- function(x) ifelse(x > 0, positive_color, ifelse(x < 0, negative_color, neutral_color))
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
plotly_trace_finish <- function(plot) {
  plotly::config(
    plotly::layout(plot, margin = list(t = 76, r = 28, b = 52, l = 58),
      title = list(x = 0.02, font = list(size = 13)), xaxis = list(tickformat = ".2f"), yaxis = list(tickformat = ".2f")),
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines")
  )
}
metric_card <- function(title, output_id) {
  div(class = "metric-card",
    tags$div(class = "metric-kicker", title),
    div(class = "metric-value", textOutput(output_id))
  )
}
metric_card_sub <- function(title, value_id, subtitle_id = NULL) {
  div(class = "metric-card curve-kpi-card",
    tags$div(class = "metric-kicker", title),
    div(class = "metric-value", textOutput(value_id)),
    if (!is.null(subtitle_id)) div(class = "metric-subtitle", textOutput(subtitle_id))
  )
}
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
format_rsq <- function(x) ifelse(is.na(x), "NA", fmt_num(100 * x))
static_metric_card <- function(title, value, subtitle = NULL, tone = "neutral") {
  div(class = paste("metric-card", paste0("metric-", tone)),
    tags$div(class = "metric-kicker", title),
    div(class = "metric-value", value),
    if (!is.null(subtitle)) div(class = "metric-subtitle", subtitle)
  )
}
explanation_card <- function(title, output_id, class = NULL) {
  div(class = paste("explanation-card", class),
    div(class = "card-heading compact-heading",
      tags$div(class = "card-kicker", "Desk note"),
      tags$h4(title)
    ),
    uiOutput(output_id)
  )
}
page_header <- function(title, subtitle, output_id = NULL) {
  div(class = "page-header screenshot-header",
    div(class = "page-title-block",
      tags$div(class = "eyebrow", "YieldCurve Trader"),
      tags$h2(title),
      tags$p(subtitle)
    ),
    div(class = "page-header-right",
      div(class = "header-clock", "Local RDS analytics"),
      if (!is.null(output_id)) uiOutput(output_id)
    )
  )
}
control_section <- function(title, ...) div(class = "control-section", tags$h5(title), ...)
table_card <- function(title, output_id, subtitle = NULL) {
  div(class = "table-card",
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
  div(class = "unavailable-card",
    tags$div(class = "card-kicker", "Module placeholder"),
    tags$h4(title),
    tags$p(reason)
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
plot_card <- function(output_id, height = "430px", class = NULL, subtitle = NULL) {
  div(class = paste("plot-card", class),
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
    trade_component_plot = "Curve Trade Portfolio"
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
side_panel <- function(...) div(class = "sidebar-panel", ...)
main_grid <- function(...) div(class = "main-grid screenshot-grid", ...)
grid_row <- function(...) div(class = "dashboard-row", ...)
grid_col <- function(..., class = NULL) div(class = paste("dashboard-col", class), ...)
section_card <- function(title, ..., subtitle = NULL, class = NULL) {
  div(class = paste("section-card", class),
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
  title = "YieldCurve Trader", theme = theme,
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
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
    "Multi-curve, multi-date proxy comparison with nearest effective quote date fallback.",
    NULL,
    controls = side_panel(
      rail_header("History Controls", "Multi curve/date comparison"),
      control_section("Comparison setup",
        selectizeInput("history_curves", "Historical curves", choices = NULL, multiple = TRUE,
          options = list(dropdownParent = "body", plugins = list("remove_button"), placeholder = "Type to search curves")),
        dateInput("history_base_date", "Base date"),
        dateInput("history_compare_date", "Add comparison date"),
        actionButton("add_history_date", "Add comparison date", class = "btn-outline-secondary side-secondary"),
        uiOutput("history_date_tags"),
        actionButton("run_history", "Run History Comparison", class = "btn-primary action-main"),
        progress_box("history")
      ),
      div(class = "rail-note rail-warning", strong("Proxy mode"), tags$br(), "Maximum 30 Curve x Date combinations. Base date is included automatically.")
    ),
    main = main_grid(
      module_tabs("History & Changes", c("Absolute Curves", "Changes", "Effective Dates", "Proxy Notes")),
      metric_strip(
        metric_card("Curve x Date", "history_metric_combos"),
        metric_card("Largest Move", "history_metric_largest"),
        metric_card("Fallback Dates", "history_metric_fallbacks"),
        static_metric_card("Mode", "Proxy", "Historical quotes", "warning")
      ),
      grid_row(
        grid_col(class = "span-8", plot_card("history_absolute_plot", height = "510px", class = "hero-plot",
          subtitle = "Each Curve + Date is connected by tenor and given its own trace.")),
        grid_col(class = "span-4", explanation_card("How to read history comparison", "history_explanation"))
      ),
      grid_row(
        grid_col(class = "span-7", plot_card("history_change_plot", height = "430px",
          subtitle = "Change in bp versus selected base date.")),
        grid_col(class = "span-5",
          table_card("Requested vs Effective Date Detail", "history_comparison_table", "Fallbacks are shown explicitly."),
          unavailable_card("Live Bloomberg source audit", "Current dashboard reads local RDS only, so live contributor/timestamp audit is unavailable.")
        )
      )
    )
  )),
  tabPanel("Forward Calculator", dashboard_page(
    "Forward Calculator",
    "Independent forward-rate workspace with source/date summary and compounding sensitivity.",
    "forward_banner",
    controls = side_panel(
      rail_header("Forward Controls", "Choose curve, dates and compounding"),
      source_controls("forward", "Forward curve"),
      control_section("Forward terms",
        numericInput("forward_start", "Forward start (years)", 1, min = 0, step = 0.25),
        numericInput("forward_end", "Forward end (years)", 5, min = 0.01, step = 0.25),
        sidebar_selectize_input("forward_compounding", "Compounding", choices = c("Annual" = "annual", "Continuous" = "continuous", "Simple" = "simple")),
        actionButton("calculate_forward", "Calculate Forward", class = "btn-primary action-main"),
        progress_box("forward")
      ),
      div(class = "rail-note", "Zero snapshots use the latest local file. Historical mode shows requested and effective dates.")
    ),
    main = main_grid(
      module_tabs("Forward Calculator", c("Forward Setup", "Endpoint Curve", "Sensitivity", "Formula")),
      metric_strip(
        metric_card("Forward Rate", "forward_value"),
        metric_card("Start Spot", "forward_start_spot"),
        metric_card("End Spot", "forward_end_spot"),
        metric_card("Compounding", "forward_compounding_value")
      ),
      grid_row(
        grid_col(class = "span-8", plot_card("forward_curve_plot", height = "540px", class = "hero-plot",
          subtitle = "Fitted curve with start/end forward endpoints.")),
        grid_col(class = "span-4", explanation_card("What this forward means", "forward_explanation"))
      ),
      grid_row(
        grid_col(class = "span-6", table_card("Forward Result", "forward_result")),
        grid_col(class = "span-3", table_card("Compounding Sensitivity", "forward_sensitivity")),
        grid_col(class = "span-3", unavailable_card("Coupon cashflow revaluation", "This version does not revalue coupon cashflows; forward is curve-implied spot-to-spot."))
      )
    )
  )),
  tabPanel("Carry & Roll", dashboard_page(
    "Carry & Roll",
    "Single-trade carry/roll plus DV01-neutral steepener, flattener and fly workspace.",
    NULL,
    controls = side_panel(
      rail_header("Carry Controls", "Single trade and curve trade"),
      source_controls("carry", "Single trade source"),
      control_section("Single trade",
        numericInput("carry_start", "Trade start (years)", 0, min = 0, step = 0.25),
        numericInput("carry_end", "Trade end (years)", 5, min = 0.25, step = 0.25),
        sidebar_selectize_input("carry_hold", "Hold period", choices = c("1M" = 1/12, "3M" = 0.25, "6M" = 0.5, "1Y" = 1), selected = 0.25),
        sidebar_selectize_input("carry_direction", "Direction", choices = c("Receive Fixed", "Pay Fixed")),
        numericInput("dv01", "DV01 per bp", 10000, min = 0, step = 1000),
        actionButton("calculate_carry", "Calculate Carry & Roll", class = "btn-primary action-main"),
        progress_box("carry")
      ),
      source_controls("trade", "Curve trade source"),
      control_section("Curve trade structure",
        sidebar_selectize_input("trade_structure", "Structure", choices = c("Steepener" = "steepener", "Flattener" = "flattener",
          "Long-belly Fly" = "long_belly_fly", "Short-belly Fly" = "short_belly_fly")),
        numericInput("trade_short_tenor", "Short tenor", 2, min = 0.25, step = 0.25),
        numericInput("trade_belly_tenor", "Belly tenor", 5, min = 0.5, step = 0.25),
        numericInput("trade_long_tenor", "Long tenor", 10, min = 1, step = 0.25),
        sidebar_selectize_input("trade_hold", "Hold period", choices = c("1M" = 1/12, "3M" = 0.25, "6M" = 0.5, "1Y" = 1), selected = 0.25),
        numericInput("trade_risk_budget", "DV01-neutral risk budget", 10000, min = 1, step = 1000),
        numericInput("trade_short_dv01", "Short leg DV01", 10000, min = 0, step = 1000),
        numericInput("trade_belly_dv01", "Belly leg DV01", 10000, min = 0, step = 1000),
        numericInput("trade_long_dv01", "Long leg DV01", 10000, min = 0, step = 1000),
        actionButton("load_neutral_dv01", "Load DV01-neutral defaults", class = "btn-outline-secondary side-secondary"),
        actionButton("calculate_curve_trade", "Calculate Curve Trade", class = "btn-primary action-main"),
        progress_box("trade")
      )
    ),
    main = main_grid(
      module_tabs("Carry & Roll", c("Single Trade", "Curve Trade", "Tenor Matrix", "Risk Notes")),
      section_card("Single Trade", subtitle = "Carry, roll and DV01 P&L for one receive/pay fixed view.",
        uiOutput("carry_banner"),
        metric_strip(
          metric_card("Carry", "carry_value"),
          metric_card("Roll", "roll_value"),
          metric_card("Total / P&L", "total_value"),
          metric_card("Direction", "carry_direction_value")
        ),
        grid_row(
          grid_col(class = "span-4", explanation_card("How to interpret Carry and Roll", "carry_explanation")),
          grid_col(class = "span-4", plot_card("carry_component_plot", height = "340px")),
          grid_col(class = "span-4", plot_card("carry_spot_plot", height = "340px"))
        ),
        grid_row(
          grid_col(class = "span-7", plot_card("carry_stacked_plot", height = "680px",
            subtitle = "Tenors are ordered by maturity, not alphabetically.")),
          grid_col(class = "span-5",
            plot_card("carry_heatmap", height = "380px"),
            unavailable_card("Strict OIS/IRS bootstrap", "Current local data supports fitted-curve proxy analytics, not strict multi-curve bootstrapping.")
          )
        ),
        table_card("Carry Matrix", "carry_matrix")
      ),
      section_card("Curve Trade", subtitle = "DV01-neutral recommended structures remain visible and calculated separately.",
        uiOutput("trade_banner"),
        metric_strip(
          metric_card("Portfolio Carry P&L", "trade_carry_pnl"),
          metric_card("Portfolio Roll P&L", "trade_roll_pnl"),
          metric_card("Total P&L / Eq. bp", "trade_total_pnl"),
          metric_card("Structure", "trade_structure_value")
        ),
        explanation_card("How this curve trade is constructed", "trade_explanation"),
        grid_row(
          grid_col(class = "span-5", table_card("Curve Trade Legs", "trade_leg_table", "Each leg shows carry, roll, total bp and P&L.")),
          grid_col(class = "span-7", plot_card("trade_leg_pnl_plot", height = "430px"))
        ),
        plot_card("trade_component_plot", height = "380px")
      )
    )
  )),
  tabPanel("Diagnostics", dashboard_page(
    "Diagnostics",
    "Data lineage, proxy policy, unit checks and fit residuals from the last successful Curve Explorer apply.",
    NULL,
    controls = side_panel(
      rail_header("Diagnostics Controls", "Policy and data quality"),
      control_section("Model policy",
        div(class = "rail-note", strong("Local RDS only"), tags$br(), "No Bloomberg live refresh in this version."),
        div(class = "rail-note", strong("Proxy analytics"), tags$br(), "Historical quotes are not strict multi-curve bootstrap results."),
        div(class = "rail-note", strong("Unit policy"), tags$br(), "Market percent inputs are converted to decimals internally and displayed as % or bp.")
      )
    ),
    main = main_grid(
      module_tabs("Diagnostics", c("Data Quality", "Model Fit", "Unavailable Metadata", "Unit Policy")),
      metric_strip(
        metric_card("Data Source", "diag_source"),
        metric_card("Proxy Flag", "diag_proxy"),
        metric_card("Fit RMSE", "diag_rmse"),
        metric_card("Tenor Points", "diag_points")
      ),
      grid_row(
        grid_col(class = "span-4", explanation_card("How to read diagnostics", "diagnostics_explanation")),
        grid_col(class = "span-4", table_card("Fit Diagnostics", "diagnostics_table")),
        grid_col(class = "span-4", table_card("Input Points", "input_points"))
      ),
      grid_row(
        grid_col(class = "span-6", unavailable_card("Bloomberg curve source metadata", "Ticker, contributor, quote timestamp and live source hierarchy are not present in the local RDS.")),
        grid_col(class = "span-6", unavailable_card("Production cashflow engine", "Coupon schedules and full cashflow revaluation are outside the current local dashboard."))
      )
    )
  ))
)

server <- function(input, output, session) {
  market <- reactiveVal(load_market_data(project_dir))
  history_compare_dates <- reactiveVal(as.Date(character()))
  history_dates_initialized <- reactiveVal(FALSE)
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
    update_curve_selector("trade", input$trade_source_mode)
    dates <- available_dates()
    history_curves <- curve_choices()$historical
    defaults <- intersect(c("USD SOFR OIS", "EUR ESTR OIS"), history_curves)
    updateSelectizeInput(session, "history_curves", choices = history_curves,
      selected = if (length(isolate(input$history_curves))) isolate(input$history_curves) else defaults, server = TRUE)
    updateDateInput(session, "history_base_date", value = if (is.null(isolate(input$history_base_date))) dates[max(1, length(dates) - 21)] else as.Date(isolate(input$history_base_date)), min = min(dates), max = max(dates))
    updateDateInput(session, "history_compare_date", value = if (is.null(isolate(input$history_compare_date))) max(dates) else as.Date(isolate(input$history_compare_date)), min = min(dates), max = max(dates))
    if (!isolate(history_dates_initialized())) { history_compare_dates(max(dates)); history_dates_initialized(TRUE) }
  })
  observeEvent(input$add_history_date, { req(input$history_compare_date); history_compare_dates(sort(unique(c(history_compare_dates(), as.Date(input$history_compare_date))))); mark_pending("history") })
  observeEvent(input$remove_history_date, { history_compare_dates(setdiff(history_compare_dates(), as.Date(input$remove_history_date))); mark_pending("history") })
  output$history_date_tags <- renderUI({
    dates <- history_compare_dates()
    if (!length(dates)) return(div(class = "small-note", "No comparison dates selected."))
    tagList(tags$div(class = "history-date-label", "Selected comparison dates"),
      tags$div(class = "history-date-tags", lapply(as.character(dates), function(date_text) tags$button(
        type = "button", class = "history-date-tag",
        onclick = sprintf("Shiny.setInputValue('remove_history_date', '%s', {priority: 'event'})", date_text),
        tags$span(date_text), tags$span(class = "history-date-remove", "\u00d7")))))
  })

  observeEvent(list(input$source_mode, input$curve_name, input$curve_date, input$fit_methods,
    input$curve_start_tenor, input$curve_end_tenor, input$show_raw_points,
    input$show_ns_fit, input$show_spline_fit, input$show_residuals_panel), mark_pending("curve"), ignoreInit = TRUE)
  observeEvent(list(input$history_curves, input$history_base_date), mark_pending("history"), ignoreInit = TRUE)
  observeEvent(list(input$forward_source_mode, input$forward_curve_name, input$forward_curve_date, input$forward_fit_method, input$forward_start, input$forward_end, input$forward_compounding), mark_pending("forward"), ignoreInit = TRUE)
  observeEvent(list(input$carry_source_mode, input$carry_curve_name, input$carry_curve_date, input$carry_fit_method, input$carry_start, input$carry_end, input$carry_hold, input$carry_direction, input$dv01), mark_pending("carry"), ignoreInit = TRUE)
  observeEvent(list(input$trade_source_mode, input$trade_curve_name, input$trade_curve_date, input$trade_fit_method, input$trade_structure, input$trade_short_tenor, input$trade_belly_tenor, input$trade_long_tenor, input$trade_hold, input$trade_risk_budget, input$trade_short_dv01, input$trade_belly_dv01, input$trade_long_dv01), mark_pending("trade"), ignoreInit = TRUE)

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
      list(data = data, base_date = as.Date(input$history_base_date), combinations = combinations)
    })
    if (!is.null(result)) applied_history(result)
  }, ignoreInit = FALSE)
  observeEvent(input$calculate_forward, {
    result <- run_page("forward", function() {
      set_status("forward", "running", 30, "Resolving selected date")
      bundle <- prepare_curve_fit(market(), input$forward_source_mode, input$forward_curve_name, input$forward_curve_date, input$forward_fit_method)
      set_status("forward", "running", 68, "Calculating forward")
      value <- calculate_forward(bundle$fit, input$forward_start, input$forward_end, input$forward_compounding)
      value$curve <- bundle$curve_name
      value$requested_date <- if (bundle$proxy) as.character(bundle$requested_date) else "Snapshot date unavailable"
      value$effective_date <- if (bundle$proxy) as.character(bundle$effective_date) else "Snapshot date unavailable"
      set_status("forward", "running", 90, "Generating chart")
      list(bundle = bundle, result = value, start = input$forward_start, end = input$forward_end, compounding = input$forward_compounding)
    })
    if (!is.null(result)) applied_forward(result)
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
  observeEvent(input$load_neutral_dv01, {
    legs <- curve_trade_legs(input$trade_structure, input$trade_short_tenor, input$trade_belly_tenor, input$trade_long_tenor, input$trade_risk_budget)
    updateNumericInput(session, "trade_short_dv01", value = legs$dv01[[1]])
    if (nrow(legs) == 3) updateNumericInput(session, "trade_belly_dv01", value = legs$dv01[[2]])
    updateNumericInput(session, "trade_long_dv01", value = legs$dv01[[nrow(legs)]])
  })
  observeEvent(input$calculate_curve_trade, {
    result <- run_page("trade", function() {
      set_status("trade", "running", 28, "Resolving selected date")
      bundle <- prepare_curve_fit(market(), input$trade_source_mode, input$trade_curve_name, input$trade_curve_date, input$trade_fit_method)
      legs <- curve_trade_legs(input$trade_structure, input$trade_short_tenor, input$trade_belly_tenor, input$trade_long_tenor, input$trade_risk_budget)
      legs$dv01 <- if (nrow(legs) == 2) c(input$trade_short_dv01, input$trade_long_dv01) else c(input$trade_short_dv01, input$trade_belly_dv01, input$trade_long_dv01)
      set_status("trade", "running", 65, "Calculating each leg")
      calculation <- calculate_curve_trade(bundle$fit, legs, as.numeric(input$trade_hold), "annual", input$trade_risk_budget)
      set_status("trade", "running", 90, "Generating charts")
      list(bundle = bundle, calculation = calculation, structure = input$trade_structure)
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
  output$curve_metric_rmse <- renderText({ req(ns_curve_fit()); fmt_num(ns_curve_fit()$rmse_bp) })
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
        Value = c(fmt_num(ns$rmse_bp), paste0(format_rsq(fit_rsq(ns)), "%")),
        stringsAsFactors = FALSE
      )),
      parameter_section_ui("Spline", data.frame(
        Metric = c("Method", "Knots", "RMSE (bp)"),
        Value = c(if (is.null(spline)) "NA" else "Cubic spline", if (is.null(spline)) "NA" else "Market tenors",
          if (is.null(spline)) "NA" else fmt_num(spline$rmse_bp)),
        stringsAsFactors = FALSE
      ))
    )
  })
  output$fitted_parameters <- renderUI({ fitted_parameters_ui() })
  output$fit_summary <- renderDT({ req(applied_curve()); datatable(round_numeric_df(data.frame(Method = vapply(applied_curve()$fits, `[[`, character(1), "method"), RMSE_bp = vapply(applied_curve()$fits, `[[`, numeric(1), "rmse_bp"))), options = list(dom = "t"), rownames = FALSE) })
  output$ns_parameters <- renderDT({ req(ns_curve_fit()); ns <- ns_curve_fit(); datatable(round_numeric_df(data.frame(Parameter = names(ns$parameters), Value = ns$parameters)), options = list(dom = "t"), rownames = FALSE) })
  output$curve_explanation <- renderUI({
    req(ns_curve_fit())
    tagList(
      div(class = "fit-note-item", "Nelson-Siegel summarizes the curve with Level, Slope and Curvature factors. Beta0 is the long-rate anchor, Beta1 tilts the front end, and Tau controls where the curvature factor has the most influence."),
      div(class = "fit-note-item", "Spline is a flexible interpolation layer through local curve shape. It is useful for visual inspection, while the KPI cards always report the Nelson-Siegel fit so the dashboard has one consistent model benchmark."),
      div(class = "fit-note-item", "Historical quote curves are proxy analytics; zero-rate curves use the latest local RDS snapshot.")
    )
  })

  register_plot("history_absolute_plot", function() { req(applied_history()); x <- applied_history()$data; x <- x[order(x$series, x$tenor), ]; x$text <- paste0("Curve: ", x$curve, "<br>Requested: ", x$requested_date, "<br>Effective: ", x$effective_date, "<br>Tenor: ", fmt_num(x$tenor), "Y<br>Rate: ", fmt_pct(x$rate_percent)); plotly_finish(ggplot(x, aes(tenor, rate_percent, color = series, linetype = series, group = series, text = text)) + geom_line(linewidth = 1) + scale_linetype_manual(values = series_linetypes(x$series)) + labs(title = "Absolute Curves", x = "Tenor", y = "Rate (%)", color = "Curve | Date", linetype = "Curve | Date") + theme_minimal(base_size = 11) + theme(plot.title = element_text(margin = margin(b = 15)))) })
  register_plot("history_change_plot", function() { req(applied_history()); x <- applied_history()$data; x <- x[order(x$series, x$tenor), ]; x$text <- paste0("Curve: ", x$curve, "<br>Requested: ", x$requested_date, "<br>Effective: ", x$effective_date, "<br>Tenor: ", fmt_num(x$tenor), "Y<br>Change: ", fmt_bp(x$change_bp)); plotly_finish(ggplot(x, aes(tenor, change_bp, color = series, linetype = series, group = series, text = text)) + geom_hline(yintercept = 0, color = "grey65") + geom_line(linewidth = 1) + scale_linetype_manual(values = series_linetypes(x$series)) + labs(title = paste("Change vs Base Date", applied_history()$base_date), x = "Tenor", y = "Change (bp)", color = "Curve | Date", linetype = "Curve | Date") + theme_minimal(base_size = 11) + theme(plot.title = element_text(margin = margin(b = 15)))) })
  output$history_comparison_table <- renderDT({ req(applied_history()); datatable(round_numeric_df(applied_history()$data), options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE) })
  output$history_explanation <- renderUI({ req(applied_history()); p(paste("Last run:", applied_history()$combinations, "Curve x Date combinations. Every Curve + Date has its own color and line type.")) })
  output$history_metric_combos <- renderText({ req(applied_history()); applied_history()$combinations })
  output$history_metric_largest <- renderText({
    req(applied_history())
    x <- applied_history()$data
    if (!nrow(x)) return("0 bp")
    fmt_bp(x$change_bp[which.max(abs(x$change_bp))])
  })
  output$history_metric_fallbacks <- renderText({
    req(applied_history())
    x <- unique(applied_history()$data[, c("curve", "requested_date", "effective_date")])
    sum(as.Date(x$requested_date) != as.Date(x$effective_date))
  })

  curve_banner <- function(bundle) {
    req(bundle)
    if (bundle$proxy) {
      div(class = "proxy-banner", tags$span(class = "badge-label", "Proxy"), paste(bundle$source, "| Requested:", bundle$requested_date, "| Effective:", bundle$effective_date))
    } else {
      div(class = "official-banner", tags$span(class = "badge-label", "Local snapshot"), paste(bundle$source, "| Latest local zero-rate snapshot"))
    }
  }
  output$forward_banner <- renderUI({ req(applied_forward()); curve_banner(applied_forward()$bundle) })
  output$forward_value <- renderText({ req(applied_forward()); fmt_pct(applied_forward()$result$forward_percent) })
  output$forward_start_spot <- renderText({ req(applied_forward()); x <- applied_forward(); fmt_pct(decimal_to_percent(curve_rate(x$bundle$fit, pmax(x$start, min(x$bundle$fit$points$tenor))))) })
  output$forward_end_spot <- renderText({ req(applied_forward()); x <- applied_forward(); fmt_pct(decimal_to_percent(curve_rate(x$bundle$fit, x$end))) })
  output$forward_compounding_value <- renderText({ req(applied_forward()); tools::toTitleCase(applied_forward()$compounding) })
  output$forward_result <- renderDT({ req(applied_forward()); datatable(round_numeric_df(applied_forward()$result), options = list(dom = "t", scrollX = TRUE), rownames = FALSE) })
  output$forward_sensitivity <- renderDT({
    req(applied_forward())
    x <- applied_forward()
    rows <- do.call(rbind, lapply(c("annual", "continuous", "simple"), function(method) {
      calculate_forward(x$bundle$fit, x$start, x$end, method)
    }))
    datatable(round_numeric_df(rows[, c("compounding", "forward_percent")]), options = list(dom = "t"), rownames = FALSE)
  })
  output$forward_explanation <- renderUI({ req(applied_forward()); x <- applied_forward(); p(sprintf("Last applied result: %.2fY to %.2fY on %s. Historical calculations show requested and effective dates; zero-rate snapshots have no date field.", x$start, x$end, x$bundle$curve_name)) })
  register_plot("forward_curve_plot", function() { req(applied_forward()); x <- applied_forward(); fit <- x$bundle$fit; grid <- seq(max(.01, min(fit$points$tenor)), max(fit$points$tenor), length.out = 250); data <- data.frame(tenor = grid, rate = decimal_to_percent(curve_rate(fit, grid))); data <- data[order(data$tenor), ]; data$text <- paste0("Tenor: ", fmt_num(data$tenor), "Y<br>Rate: ", fmt_pct(data$rate)); marks <- data.frame(tenor = c(x$start, x$end), label = c("Start", "End")); marks$rate <- decimal_to_percent(curve_rate(fit, pmax(marks$tenor, min(fit$points$tenor)))); marks$text <- paste0(marks$label, "<br>Tenor: ", fmt_num(marks$tenor), "Y<br>Rate: ", fmt_pct(marks$rate)); plot <- plotly::plot_ly(data = data, x = ~tenor, y = ~rate, text = ~text, hoverinfo = "text", type = "scatter", mode = "lines", name = "Fitted curve", line = list(color = "#1f4e78", width = 2)); plot <- plotly::add_trace(plot, data = marks, x = ~tenor, y = ~rate, text = ~text, hoverinfo = "text", type = "scatter", mode = "markers", name = "Forward endpoints", marker = list(color = c(positive_color, negative_color), size = 9), inherit = FALSE); plotly_trace_finish(plotly::layout(plot, title = "Selected Curve and Forward Endpoints", xaxis = list(title = "Tenor"), yaxis = list(title = "Rate (%)"))) })

  output$carry_banner <- renderUI({ req(applied_carry()); curve_banner(applied_carry()$bundle) })
  output$carry_value <- renderText({ req(applied_carry()); fmt_bp(applied_carry()$single$carry_bp) })
  output$roll_value <- renderText({ req(applied_carry()); fmt_bp(applied_carry()$single$roll_bp) })
  output$total_value <- renderText({ req(applied_carry()); paste(fmt_bp(applied_carry()$single$total_bp), "/", fmt_num(calculate_dv01_pnl(applied_carry()$single$total_bp, applied_carry()$dv01))) })
  output$carry_direction_value <- renderText({ req(applied_carry()); applied_carry()$single$direction })
  output$carry_explanation <- renderUI({ req(applied_carry()); x <- applied_carry()$single; p(sprintf("Last applied trade: Carry %.2f bp + Roll %.2f bp = Total %.2f bp.", x$carry_bp, x$roll_bp, x$total_bp)) })
  register_plot("carry_component_plot", function() {
    req(applied_carry())
    x <- data.frame(
      component = factor(c("Carry", "Roll", "Total"), levels = c("Carry", "Roll", "Total")),
      bp = c(applied_carry()$single$carry_bp, applied_carry()$single$roll_bp, applied_carry()$single$total_bp)
    )
    x$sign <- ifelse(x$bp > 0, "Positive", ifelse(x$bp < 0, "Negative", "Zero"))
    x$text <- paste0(x$component, ": ", fmt_bp(x$bp))
    plotly_finish(ggplot(x, aes(component, bp, fill = sign, text = text)) +
      geom_hline(yintercept = 0, color = "#A8B6C3", linewidth = 0.35) +
      geom_col(width = 0.52) +
      geom_text(aes(label = fmt_num(bp)), vjust = ifelse(x$bp >= 0, -0.45, 1.2), size = 3) +
      scale_fill_manual(values = c(Positive = positive_color, Negative = negative_color, Zero = neutral_color)) +
      labs(title = "Single trade decomposition", x = NULL, y = "bp", fill = NULL) +
      theme_minimal(base_size = 10) +
      theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 11)))
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
  output$carry_matrix <- renderDT({ req(applied_carry()); datatable(round_numeric_df(applied_carry()$matrix[, c("tenor_label", "hold_label", "carry_bp", "roll_bp", "total_bp", "pnl")]), options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE) })
  register_plot("carry_heatmap", function() {
    req(applied_carry())
    x <- applied_carry()$matrix
    plotly_finish(ggplot(x, aes(tenor_label, hold_label, fill = total_bp,
      text = paste0("Total: ", fmt_bp(total_bp), "<br>P&L: ", fmt_num(pnl)))) +
      geom_tile(color = "#FFFFFF", linewidth = 0.7) +
      geom_text(aes(label = fmt_num(total_bp)), size = 2.7, color = "#10263B") +
      scale_fill_gradient2(low = "#F3B1A7", mid = "#F8FAFC", high = "#8ED8C9") +
      labs(title = "Tenor / hold heatmap", x = "Tenor", y = "Hold", fill = "Total bp") +
      theme_minimal(base_size = 10) +
      theme(panel.grid = element_blank(), plot.title = element_text(face = "bold", size = 11)))
  })

  output$trade_banner <- renderUI({ req(applied_trade()); curve_banner(applied_trade()$bundle) })
  output$trade_carry_pnl <- renderText({ req(applied_trade()); fmt_num(applied_trade()$calculation$summary$carry_pnl) })
  output$trade_roll_pnl <- renderText({ req(applied_trade()); fmt_num(applied_trade()$calculation$summary$roll_pnl) })
  output$trade_total_pnl <- renderText({ req(applied_trade()); paste(fmt_num(applied_trade()$calculation$summary$total_pnl), "/", fmt_bp(applied_trade()$calculation$summary$equivalent_total_bp)) })
  output$trade_structure_value <- renderText({ req(applied_trade()); tools::toTitleCase(gsub("_", " ", applied_trade()$structure)) })
  output$trade_explanation <- renderUI({ req(applied_trade()); x <- applied_trade(); p(paste("Last applied structure:", x$structure, ". Each leg table and chart show its own Carry and Roll.")) })
  output$trade_leg_table <- renderDT({ req(applied_trade()); datatable(round_numeric_df(applied_trade()$calculation$detail[, c("leg", "tenor", "direction", "dv01", "carry_bp", "roll_bp", "total_bp", "carry_pnl", "roll_pnl", "total_pnl")]), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  register_plot("trade_leg_pnl_plot", function() {
    req(applied_trade())
    x <- applied_trade()$calculation$detail
    long <- rbind(
      data.frame(leg = x$leg, component = "Carry", value = x$carry_bp, total = x$total_bp),
      data.frame(leg = x$leg, component = "Roll", value = x$roll_bp, total = x$total_bp)
    )
    long$sign <- ifelse(long$value > 0, "Positive", ifelse(long$value < 0, "Negative", "Zero"))
    long$text <- paste0(long$component, "<br>Leg: ", long$leg, "<br>Value: ", fmt_bp(long$value))
    plotly_finish(ggplot(long, aes(leg, value, fill = sign, alpha = component, text = text)) +
      geom_hline(yintercept = 0, color = "#A8B6C3", linewidth = 0.35) +
      geom_col(position = "stack", width = 0.55, color = "#FFFFFF", linewidth = 0.25) +
      geom_point(data = x, aes(leg, total_bp), inherit.aes = FALSE, color = "#0B2F4E", size = 2.6) +
      geom_text(data = x, aes(leg, total_bp, label = fmt_num(total_bp)), inherit.aes = FALSE, vjust = -0.55, size = 2.8) +
      scale_fill_manual(values = c(Positive = positive_color, Negative = negative_color, Zero = neutral_color)) +
      scale_alpha_manual(values = c(Carry = 0.95, Roll = 0.45)) +
      labs(title = "Curve trade leg decomposition", x = NULL, y = "bp", fill = "Sign", alpha = "Component") +
      theme_minimal(base_size = 10) +
      theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 11)))
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

  output$diagnostics_explanation <- renderUI(p(sprintf("Diagnostics uses the last applied Curve Explorer fit. Current RMSE: %.2f bp.", curve_fit()$rmse_bp)))
  output$diag_source <- renderText({ req(applied_curve()); if (applied_curve()$mode == "zero") "Zero snapshot" else "Historical proxy" })
  output$diag_proxy <- renderText({ req(applied_curve()); if (applied_curve()$mode == "historical") "TRUE" else "FALSE" })
  output$diag_rmse <- renderText({ req(curve_fit()); fmt_bp(curve_fit()$rmse_bp) })
  output$diag_points <- renderText({ req(applied_curve()); nrow(applied_curve()$points) })
  output$diagnostics_table <- renderDT(datatable(round_numeric_df(curve_fit()$diagnostics), options = list(pageLength = 10), rownames = FALSE))
  output$input_points <- renderDT({ req(applied_curve()); datatable(round_numeric_df(transform(applied_curve()$points, rate_percent = decimal_to_percent(rate))), options = list(pageLength = 10), rownames = FALSE) })
}

shinyApp(ui, server)
