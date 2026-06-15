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

theme <- bs_theme(version = 5, bootswatch = "flatly", primary = "#1f4e78")
positive_color <- "#198754"
negative_color <- "#d9534f"
neutral_color <- "#6c757d"

fmt_num <- function(x) format(round(as.numeric(x), 2), nsmall = 0, trim = TRUE, scientific = FALSE, big.mark = ",")
fmt_pct <- function(x) paste0(fmt_num(x), "%")
fmt_bp <- function(x) paste0(fmt_num(x), " bp")
round_numeric_df <- function(x) {
  numeric_columns <- vapply(x, is.numeric, logical(1))
  x[numeric_columns] <- lapply(x[numeric_columns], round, 2)
  x
}
value_color <- function(x) ifelse(x > 0, positive_color, ifelse(x < 0, negative_color, neutral_color))
plotly_finish <- function(plot, tooltip = "text") {
  plotly::config(
    plotly::layout(ggplotly(plot, tooltip = tooltip), margin = list(t = 90)),
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines")
  )
}
metric_card <- function(title, output_id) div(class = "metric-card", tags$h4(title), div(class = "value", textOutput(output_id)))
explanation_card <- function(title, output_id) div(class = "explanation-card", tags$h4(title), uiOutput(output_id))
progress_box <- function(prefix) uiOutput(paste0(prefix, "_progress"))
source_controls <- function(prefix, title) {
  tagList(
    h4(title),
    radioButtons(paste0(prefix, "_source_mode"), "Analytics source",
      choices = c("Zero-rate snapshot" = "zero", "Historical quotes (Proxy)" = "historical"), selected = "zero"),
    selectInput(paste0(prefix, "_curve_name"), "Curve", choices = NULL),
    conditionalPanel(sprintf("input.%s_source_mode == 'historical'", prefix), dateInput(paste0(prefix, "_curve_date"), "Historical date")),
    selectInput(paste0(prefix, "_fit_method"), "Fit method",
      choices = c("Nelson-Siegel" = "nelson_siegel", "Spline" = "spline"), selected = "nelson_siegel")
  )
}

ui <- navbarPage(
  title = "YieldCurve Trader", theme = theme,
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  tabPanel("Curve Explorer", fluidPage(br(), fluidRow(
    column(3, div(class = "sidebar-panel",
      radioButtons("source_mode", "Analytics source",
        choices = c("Zero-rate snapshot (official analytics)" = "zero", "Historical market quotes (proxy)" = "historical"), selected = "zero"),
      selectInput("curve_name", "Curve", choices = NULL),
      conditionalPanel("input.source_mode == 'historical'", dateInput("curve_date", "Date")),
      checkboxGroupInput("fit_methods", "Curve fits", choices = c("Nelson-Siegel" = "nelson_siegel", "Spline" = "spline"),
        selected = c("nelson_siegel", "spline")),
      actionButton("apply_curve", "Apply Curve", class = "btn-primary"),
      progress_box("curve"), hr(), actionButton("refresh_data", "Refresh local RDS"),
      div(class = "small-note", textOutput("loaded_at")), div(class = "small-note", textOutput("effective_curve_date"))
    )),
    column(9, uiOutput("source_banner"), plotlyOutput("curve_plot", height = "500px"),
      explanation_card("How to read this curve", "curve_explanation"),
      fluidRow(column(6, DTOutput("fit_summary")), column(6, DTOutput("ns_parameters"))))
  ))),
  tabPanel("History & Changes", fluidPage(br(), fluidRow(
    column(3, div(class = "sidebar-panel",
      selectizeInput("history_curves", "Historical curves", choices = NULL, multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Type to search curves")),
      dateInput("history_base_date", "Base date"),
      dateInput("history_compare_date", "Add comparison date"),
      actionButton("add_history_date", "Add comparison date"),
      uiOutput("history_date_tags"),
      actionButton("run_history", "Run History Comparison", class = "btn-primary"),
      progress_box("history")
    )),
    column(9, div(class = "proxy-banner", strong("Proxy analysis: "), "maximum 30 Curve x Date combinations."),
      explanation_card("How to read history comparison", "history_explanation"),
      plotlyOutput("history_absolute_plot", height = "430px"), plotlyOutput("history_change_plot", height = "430px"),
      DTOutput("history_comparison_table"))
  ))),
  tabPanel("Forward Calculator", fluidPage(br(), fluidRow(
    column(3, div(class = "sidebar-panel", source_controls("forward", "Forward curve"), hr(),
      numericInput("forward_start", "Forward start (years)", 1, min = 0, step = 0.25),
      numericInput("forward_end", "Forward end (years)", 5, min = 0.01, step = 0.25),
      selectInput("forward_compounding", "Compounding", choices = c("Annual" = "annual", "Continuous" = "continuous", "Simple" = "simple")),
      actionButton("calculate_forward", "Calculate Forward", class = "btn-primary"), progress_box("forward")
    )),
    column(9, uiOutput("forward_banner"), fluidRow(column(4, metric_card("Forward Rate", "forward_value")), column(8, DTOutput("forward_result"))),
      explanation_card("What this forward means", "forward_explanation"), plotlyOutput("forward_curve_plot", height = "430px"))
  ))),
  tabPanel("Carry & Roll", fluidPage(br(), tabsetPanel(
    tabPanel("Single Trade", br(), fluidRow(
      column(3, div(class = "sidebar-panel", source_controls("carry", "Carry curve"), hr(),
        numericInput("carry_start", "Trade start (years)", 0, min = 0, step = 0.25),
        numericInput("carry_end", "Trade end (years)", 5, min = 0.25, step = 0.25),
        selectInput("carry_hold", "Hold period", choices = c("1M" = 1/12, "3M" = 0.25, "6M" = 0.5, "1Y" = 1), selected = 0.25),
        selectInput("carry_direction", "Direction", choices = c("Receive Fixed", "Pay Fixed")),
        numericInput("dv01", "DV01 per bp", 10000, min = 0, step = 1000),
        actionButton("calculate_carry", "Calculate Carry & Roll", class = "btn-primary"), progress_box("carry")
      )),
      column(9, uiOutput("carry_banner"),
        fluidRow(column(4, metric_card("Carry", "carry_value")), column(4, metric_card("Roll", "roll_value")), column(4, metric_card("Total / P&L", "total_value"))),
        explanation_card("How to interpret Carry and Roll", "carry_explanation"),
        fluidRow(column(6, plotlyOutput("carry_component_plot", height = "390px")), column(6, plotlyOutput("carry_spot_plot", height = "390px"))),
        plotlyOutput("carry_stacked_plot", height = "760px"),
        fluidRow(column(7, plotlyOutput("carry_heatmap", height = "480px")), column(5, DTOutput("carry_matrix"))))
    )),
    tabPanel("Curve Trade", br(), fluidRow(
      column(3, div(class = "sidebar-panel", source_controls("trade", "Curve-trade curve"), hr(),
        selectInput("trade_structure", "Structure", choices = c("Steepener" = "steepener", "Flattener" = "flattener",
          "Long-belly Fly" = "long_belly_fly", "Short-belly Fly" = "short_belly_fly")),
        numericInput("trade_short_tenor", "Short tenor", 2, min = 0.25, step = 0.25),
        numericInput("trade_belly_tenor", "Belly tenor", 5, min = 0.5, step = 0.25),
        numericInput("trade_long_tenor", "Long tenor", 10, min = 1, step = 0.25),
        selectInput("trade_hold", "Hold period", choices = c("1M" = 1/12, "3M" = 0.25, "6M" = 0.5, "1Y" = 1), selected = 0.25),
        numericInput("trade_risk_budget", "DV01-neutral risk budget", 10000, min = 1, step = 1000),
        numericInput("trade_short_dv01", "Short leg DV01", 10000, min = 0, step = 1000),
        numericInput("trade_belly_dv01", "Belly leg DV01", 10000, min = 0, step = 1000),
        numericInput("trade_long_dv01", "Long leg DV01", 10000, min = 0, step = 1000),
        actionButton("load_neutral_dv01", "Load DV01-neutral defaults"),
        actionButton("calculate_curve_trade", "Calculate Curve Trade", class = "btn-primary"), progress_box("trade")
      )),
      column(9, uiOutput("trade_banner"),
        fluidRow(column(4, metric_card("Portfolio Carry P&L", "trade_carry_pnl")), column(4, metric_card("Portfolio Roll P&L", "trade_roll_pnl")),
          column(4, metric_card("Total P&L / Eq. bp", "trade_total_pnl"))),
        explanation_card("How this curve trade is constructed", "trade_explanation"), DTOutput("trade_leg_table"),
        fluidRow(column(6, plotlyOutput("trade_leg_pnl_plot", height = "450px")), column(6, plotlyOutput("trade_component_plot", height = "450px"))))
    ))
  ))),
  tabPanel("Diagnostics", fluidPage(br(), fluidRow(
    column(4, div(class = "sidebar-panel", h4("Model policy"),
      p("Diagnostics follows the last successfully applied Curve Explorer result."),
      p("Historical OIS/IRS quotes are Proxy analytics; no strict multi-curve bootstrap."),
      p("Displayed numeric values use at most two decimal places."))),
    column(8, explanation_card("How to read diagnostics", "diagnostics_explanation"), DTOutput("diagnostics_table"), br(), DTOutput("input_points"))
  )))
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
  status <- reactiveValues(
    curve = list(type = "pending", pct = 0, message = "Click Apply Curve to calculate."),
    history = list(type = "pending", pct = 0, message = "Click Run History Comparison to calculate."),
    forward = list(type = "pending", pct = 0, message = "Click Calculate Forward to calculate."),
    carry = list(type = "pending", pct = 0, message = "Click Calculate Carry & Roll to calculate."),
    trade = list(type = "pending", pct = 0, message = "Click Calculate Curve Trade to calculate.")
  )

  set_status <- function(page, type, pct, message) {
    status[[page]] <- list(type = type, pct = pct, message = message)
    session$flushReact()
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
    updateSelectInput(session, curve_id, choices = choices, selected = selected)
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

  observeEvent(list(input$source_mode, input$curve_name, input$curve_date, input$fit_methods), mark_pending("curve"), ignoreInit = TRUE)
  observeEvent(list(input$history_curves, input$history_base_date), mark_pending("history"), ignoreInit = TRUE)
  observeEvent(list(input$forward_source_mode, input$forward_curve_name, input$forward_curve_date, input$forward_fit_method, input$forward_start, input$forward_end, input$forward_compounding), mark_pending("forward"), ignoreInit = TRUE)
  observeEvent(list(input$carry_source_mode, input$carry_curve_name, input$carry_curve_date, input$carry_fit_method, input$carry_start, input$carry_end, input$carry_hold, input$carry_direction, input$dv01), mark_pending("carry"), ignoreInit = TRUE)
  observeEvent(list(input$trade_source_mode, input$trade_curve_name, input$trade_curve_date, input$trade_fit_method, input$trade_structure, input$trade_short_tenor, input$trade_belly_tenor, input$trade_long_tenor, input$trade_hold, input$trade_risk_budget, input$trade_short_dv01, input$trade_belly_dv01, input$trade_long_dv01), mark_pending("trade"), ignoreInit = TRUE)

  observeEvent(input$apply_curve, {
    result <- run_page("curve", function() {
      req(input$curve_name)
      set_status("curve", "running", 28, "Resolving selected date")
      points <- if (identical(input$source_mode, "zero")) extract_zero_curve(market()$zero_curve, input$curve_name) else extract_historical_curve(market()$wide_rates, input$curve_name, input$curve_date)
      methods <- if (length(input$fit_methods)) input$fit_methods else "nelson_siegel"
      set_status("curve", "running", 58, "Fitting curve")
      source <- curve_source_label(input$source_mode, input$curve_name, input$curve_date)
      fits <- lapply(methods, function(method) fit_curve(points$tenor, points$rate, method, source, identical(input$source_mode, "historical")))
      set_status("curve", "running", 86, "Generating charts")
      list(points = points, fits = fits, mode = input$source_mode, curve_name = input$curve_name,
        requested_date = attr(points, "requested_date"), effective_date = attr(points, "effective_date"))
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
  output$effective_curve_date <- renderText({ req(applied_curve()); x <- applied_curve(); if (x$mode == "zero") "Snapshot date unavailable" else paste("Requested:", x$requested_date, "| Effective:", x$effective_date) })
  output$source_banner <- renderUI({ req(applied_curve()); x <- applied_curve(); if (x$mode == "historical") div(class = "proxy-banner", paste("Historical Proxy | Effective:", x$effective_date)) else div(class = "official-banner", "Official zero-rate snapshot | Snapshot date unavailable") })
  output$curve_plot <- renderPlotly({
    x <- applied_curve(); req(x); points <- x$points; grid <- seq(max(.01, min(points$tenor)), max(points$tenor), length.out = 300)
    plot_data <- do.call(rbind, lapply(x$fits, function(fit) data.frame(tenor = grid, rate = decimal_to_percent(curve_rate(fit, grid)), series = fit$method)))
    plot_data$text <- paste0(plot_data$series, "<br>Tenor: ", fmt_num(plot_data$tenor), "Y<br>Rate: ", fmt_pct(plot_data$rate))
    plotly_finish(ggplot(plot_data, aes(tenor, rate, color = series, text = text)) + geom_line(linewidth = 1) +
      geom_point(data = transform(points, rate = decimal_to_percent(rate)), aes(tenor, rate), inherit.aes = FALSE, color = negative_color, size = 2) +
      labs(title = x$curve_name, x = "Tenor (years)", y = "Rate (%)", color = NULL) + theme_minimal(base_size = 12) + theme(plot.title = element_text(margin = margin(b = 15))))
  })
  output$fit_summary <- renderDT(datatable(round_numeric_df(data.frame(Method = vapply(applied_curve()$fits, `[[`, character(1), "method"), RMSE_bp = vapply(applied_curve()$fits, `[[`, numeric(1), "rmse_bp"))), options = list(dom = "t"), rownames = FALSE))
  output$ns_parameters <- renderDT({ ns <- applied_curve()$fits[vapply(applied_curve()$fits, function(x) x$method == "nelson_siegel", logical(1))]; datatable(if (!length(ns)) data.frame(Message = "Nelson-Siegel not selected") else round_numeric_df(data.frame(Parameter = names(ns[[1]]$parameters), Value = ns[[1]]$parameters)), options = list(dom = "t"), rownames = FALSE) })
  output$curve_explanation <- renderUI(p(sprintf("The last applied %s fit has RMSE %.2f bp. Inputs do not affect this result until Apply Curve is clicked.", curve_fit()$method, curve_fit()$rmse_bp)))

  output$history_absolute_plot <- renderPlotly({ x <- applied_history()$data; x <- x[order(x$series, x$tenor), ]; x$text <- paste0("Curve: ", x$curve, "<br>Requested: ", x$requested_date, "<br>Effective: ", x$effective_date, "<br>Tenor: ", fmt_num(x$tenor), "Y<br>Rate: ", fmt_pct(x$rate_percent)); plotly_finish(ggplot(x, aes(tenor, rate_percent, color = series, linetype = series, group = series, text = text)) + geom_line(linewidth = 1) + labs(title = "Absolute Curves", x = "Tenor", y = "Rate (%)", color = "Curve | Date", linetype = "Curve | Date") + theme_minimal(base_size = 11) + theme(plot.title = element_text(margin = margin(b = 15)))) })
  output$history_change_plot <- renderPlotly({ x <- applied_history()$data; x <- x[order(x$series, x$tenor), ]; x$text <- paste0("Curve: ", x$curve, "<br>Requested: ", x$requested_date, "<br>Effective: ", x$effective_date, "<br>Tenor: ", fmt_num(x$tenor), "Y<br>Change: ", fmt_bp(x$change_bp)); plotly_finish(ggplot(x, aes(tenor, change_bp, color = series, linetype = series, group = series, text = text)) + geom_hline(yintercept = 0, color = "grey65") + geom_line(linewidth = 1) + labs(title = paste("Change vs Base Date", applied_history()$base_date), x = "Tenor", y = "Change (bp)", color = "Curve | Date", linetype = "Curve | Date") + theme_minimal(base_size = 11) + theme(plot.title = element_text(margin = margin(b = 15)))) })
  output$history_comparison_table <- renderDT(datatable(round_numeric_df(applied_history()$data), options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE))
  output$history_explanation <- renderUI({ req(applied_history()); p(paste("Last run:", applied_history()$combinations, "Curve x Date combinations. Every Curve + Date has its own color and line type.")) })

  curve_banner <- function(bundle) if (bundle$proxy) div(class = "proxy-banner", paste(bundle$source, "| Requested:", bundle$requested_date, "| Effective:", bundle$effective_date)) else div(class = "official-banner", paste(bundle$source, "| Snapshot date unavailable"))
  output$forward_banner <- renderUI(curve_banner(applied_forward()$bundle))
  output$forward_value <- renderText(fmt_pct(applied_forward()$result$forward_percent))
  output$forward_result <- renderDT(datatable(round_numeric_df(applied_forward()$result), options = list(dom = "t", scrollX = TRUE), rownames = FALSE))
  output$forward_explanation <- renderUI({ x <- applied_forward(); p(sprintf("Last applied result: %.2fY to %.2fY on %s. Historical calculations show requested and effective dates; zero-rate snapshots have no date field.", x$start, x$end, x$bundle$curve_name)) })
  output$forward_curve_plot <- renderPlotly({ x <- applied_forward(); fit <- x$bundle$fit; grid <- seq(max(.01, min(fit$points$tenor)), max(fit$points$tenor), length.out = 250); data <- data.frame(tenor = grid, rate = decimal_to_percent(curve_rate(fit, grid))); data$text <- paste0("Tenor: ", fmt_num(data$tenor), "Y<br>Rate: ", fmt_pct(data$rate)); marks <- data.frame(tenor = c(x$start, x$end), label = c("Start", "End")); marks$rate <- decimal_to_percent(curve_rate(fit, pmax(marks$tenor, min(fit$points$tenor)))); plotly_finish(ggplot(data, aes(tenor, rate, text = text)) + geom_line(color = "#1f4e78", linewidth = 1) + geom_point(data = marks, aes(tenor, rate, color = label), inherit.aes = FALSE, size = 3) + labs(title = "Selected Curve and Forward Endpoints", x = "Tenor", y = "Rate (%)", color = NULL) + theme_minimal(base_size = 12) + theme(plot.title = element_text(margin = margin(b = 15)))) })

  output$carry_banner <- renderUI(curve_banner(applied_carry()$bundle))
  output$carry_value <- renderText(fmt_bp(applied_carry()$single$carry_bp))
  output$roll_value <- renderText(fmt_bp(applied_carry()$single$roll_bp))
  output$total_value <- renderText(paste(fmt_bp(applied_carry()$single$total_bp), "/", fmt_num(calculate_dv01_pnl(applied_carry()$single$total_bp, applied_carry()$dv01))))
  output$carry_explanation <- renderUI({ x <- applied_carry()$single; p(sprintf("Last applied trade: Carry %.2f bp + Roll %.2f bp = Total %.2f bp.", x$carry_bp, x$roll_bp, x$total_bp)) })
  output$carry_component_plot <- renderPlotly({ x <- data.frame(component = c("Carry", "Roll", "Total"), bp = c(applied_carry()$single$carry_bp, applied_carry()$single$roll_bp, applied_carry()$single$total_bp)); x$sign <- ifelse(x$bp > 0, "Positive", ifelse(x$bp < 0, "Negative", "Zero")); x$text <- paste0(x$component, ": ", fmt_bp(x$bp)); plotly_finish(ggplot(x, aes(component, bp, fill = sign, text = text)) + geom_col() + geom_hline(yintercept = 0, color = "grey50") + geom_text(aes(label = fmt_num(bp)), vjust = ifelse(x$bp >= 0, -0.5, 1.2)) + scale_fill_manual(values = c(Positive = positive_color, Negative = negative_color, Zero = neutral_color)) + labs(title = "Single Trade Carry / Roll Decomposition", x = NULL, y = "bp", fill = NULL) + theme_minimal(base_size = 12) + theme(plot.title = element_text(margin = margin(b = 15)))) })
  output$carry_spot_plot <- renderPlotly({ fit <- applied_carry()$bundle$fit; tenors <- sort(c(1, 2, 3, 5, 7, 10, 15, 20, 30)); x <- data.frame(tenor = tenors, spot = decimal_to_percent(curve_rate(fit, tenors))); x$text <- paste0(fmt_num(x$tenor), "Y: ", fmt_pct(x$spot)); plotly_finish(ggplot(x, aes(tenor, spot, text = text, group = 1)) + geom_line(color = "#1f4e78", linewidth = 1.1) + geom_point(color = "#1f4e78", size = 2) + geom_text(aes(label = fmt_num(spot)), vjust = -0.7) + labs(title = "Spot Curve: Shape Drives Roll", x = "Tenor", y = "Rate (%)") + theme_minimal(base_size = 12) + theme(plot.title = element_text(margin = margin(b = 15)))) })
  output$carry_stacked_plot <- renderPlotly({ x <- applied_carry()$matrix; long <- rbind(data.frame(x, component = "Carry", value = x$carry_bp), data.frame(x, component = "Roll", value = x$roll_bp)); long$sign <- ifelse(long$value > 0, "Positive", ifelse(long$value < 0, "Negative", "Zero")); plotly_finish(ggplot(long, aes(y = tenor_label, x = value, fill = sign, text = paste0(component, ": ", fmt_bp(value)))) + geom_col(position = "stack") + geom_point(data = x, aes(y = tenor_label, x = total_bp), inherit.aes = FALSE, color = "#1f4e78", size = 2.5) + geom_text(data = x, aes(y = tenor_label, x = total_bp, label = fmt_num(total_bp)), inherit.aes = FALSE, hjust = -0.2, size = 3) + facet_wrap(~hold_label, ncol = 1, scales = "free_x", drop = FALSE) + scale_fill_manual(values = c(Positive = positive_color, Negative = negative_color, Zero = neutral_color)) + labs(title = "Carry + Roll by Tenor and Hold Period", x = "bp", y = "Tenor", fill = NULL) + theme_minimal(base_size = 11) + theme(plot.title = element_text(margin = margin(b = 15)))) })
  output$carry_matrix <- renderDT(datatable(round_numeric_df(applied_carry()$matrix[, c("tenor_label", "hold_label", "carry_bp", "roll_bp", "total_bp", "pnl")]), options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE))
  output$carry_heatmap <- renderPlotly({ x <- applied_carry()$matrix; plotly_finish(ggplot(x, aes(tenor_label, hold_label, fill = total_bp, text = paste0("Total: ", fmt_bp(total_bp), "<br>P&L: ", fmt_num(pnl)))) + geom_tile(color = "white") + geom_text(aes(label = fmt_num(total_bp)), size = 3) + scale_fill_gradient2(low = negative_color, mid = "white", high = positive_color) + labs(title = "Total Carry + Roll Heatmap", x = "Tenor", y = "Hold", fill = "Total bp") + theme_minimal(base_size = 12) + theme(plot.title = element_text(margin = margin(b = 15)))) })

  output$trade_banner <- renderUI(curve_banner(applied_trade()$bundle))
  output$trade_carry_pnl <- renderText(fmt_num(applied_trade()$calculation$summary$carry_pnl))
  output$trade_roll_pnl <- renderText(fmt_num(applied_trade()$calculation$summary$roll_pnl))
  output$trade_total_pnl <- renderText(paste(fmt_num(applied_trade()$calculation$summary$total_pnl), "/", fmt_bp(applied_trade()$calculation$summary$equivalent_total_bp)))
  output$trade_explanation <- renderUI({ x <- applied_trade(); p(paste("Last applied structure:", x$structure, ". Each leg table and chart show its own Carry and Roll.")) })
  output$trade_leg_table <- renderDT(datatable(round_numeric_df(applied_trade()$calculation$detail[, c("leg", "tenor", "direction", "dv01", "carry_bp", "roll_bp", "total_bp", "carry_pnl", "roll_pnl", "total_pnl")]), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE))
  output$trade_leg_pnl_plot <- renderPlotly({ x <- applied_trade()$calculation$detail; long <- rbind(data.frame(leg = x$leg, component = "Carry", value = x$carry_bp, total = x$total_bp), data.frame(leg = x$leg, component = "Roll", value = x$roll_bp, total = x$total_bp)); long$sign <- ifelse(long$value > 0, "Positive", ifelse(long$value < 0, "Negative", "Zero")); plotly_finish(ggplot(long, aes(leg, value, fill = sign, text = paste0(component, ": ", fmt_bp(value)))) + geom_col(position = "stack") + geom_point(data = x, aes(leg, total_bp), inherit.aes = FALSE, color = "#1f4e78", size = 3) + geom_text(data = x, aes(leg, total_bp, label = fmt_num(total_bp)), inherit.aes = FALSE, vjust = -0.5) + scale_fill_manual(values = c(Positive = positive_color, Negative = negative_color, Zero = neutral_color)) + labs(title = "Carry and Roll by Leg", x = NULL, y = "bp", fill = NULL) + theme_minimal(base_size = 12) + theme(plot.title = element_text(margin = margin(b = 15)))) })
  output$trade_component_plot <- renderPlotly({ s <- applied_trade()$calculation$summary; x <- data.frame(component = c("Carry", "Roll", "Total"), pnl = c(s$carry_pnl, s$roll_pnl, s$total_pnl)); x$sign <- ifelse(x$pnl > 0, "Positive", ifelse(x$pnl < 0, "Negative", "Zero")); plotly_finish(ggplot(x, aes(component, pnl, fill = sign, text = paste0(component, ": ", fmt_num(pnl)))) + geom_col() + geom_hline(yintercept = 0, color = "grey50") + geom_text(aes(label = fmt_num(pnl)), vjust = ifelse(x$pnl >= 0, -0.4, 1.2)) + scale_fill_manual(values = c(Positive = positive_color, Negative = negative_color, Zero = neutral_color)) + labs(title = "Portfolio Carry / Roll P&L", x = NULL, y = "P&L", fill = NULL) + theme_minimal(base_size = 12) + theme(plot.title = element_text(margin = margin(b = 15)))) })

  output$diagnostics_explanation <- renderUI(p(sprintf("Diagnostics uses the last applied Curve Explorer fit. Current RMSE: %.2f bp.", curve_fit()$rmse_bp)))
  output$diagnostics_table <- renderDT(datatable(round_numeric_df(curve_fit()$diagnostics), options = list(pageLength = 10), rownames = FALSE))
  output$input_points <- renderDT(datatable(round_numeric_df(transform(applied_curve()$points, rate_percent = decimal_to_percent(rate))), options = list(pageLength = 10), rownames = FALSE))
}

shinyApp(ui, server)
