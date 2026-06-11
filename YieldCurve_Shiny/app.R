# ============================================================
# YieldCurve Trader Dashboard：UI 与 server
# ============================================================
# UI 定义交易员看到的页面；server 将每个页面自己的曲线选择传给 data_loader.R，
# 再调用 curve_engine.R 完成拟合、forward、carry/roll 和 curve trade 计算。

project_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
source(file.path(project_dir, "R", "curve_engine.R"))
source(file.path(project_dir, "R", "data_loader.R"))

required <- c("shiny", "bslib", "DT", "ggplot2", "plotly")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) stop("Missing UI packages. Start with run_app.R. Missing: ", paste(missing, collapse = ", "), call. = FALSE)

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)

theme <- bs_theme(version = 5, bootswatch = "flatly", primary = "#1f4e78")

metric_card <- function(title, output_id) {
  div(class = "metric-card", tags$h4(title), div(class = "value", textOutput(output_id)))
}

# explanation_card() 把“结果是什么”和“应该怎样读”放在图表旁边，避免页面只给数字不讲交易含义。
explanation_card <- function(title, output_id) {
  div(class = "explanation-card", tags$h4(title), uiOutput(output_id))
}

# 同一个控件模板分别生成 forward_*、carry_* 和 trade_* 输入。
# prefix 让三个页面拥有独立曲线，切换 Carry 曲线不会改变 Forward 结果。
source_controls <- function(prefix, title) {
  tagList(
    h4(title),
    radioButtons(paste0(prefix, "_source_mode"), "Analytics source",
      choices = c("Zero-rate snapshot" = "zero", "Historical quotes (Proxy)" = "historical"),
      selected = "zero"
    ),
    selectInput(paste0(prefix, "_curve_name"), "Curve", choices = NULL),
    conditionalPanel(
      sprintf("input.%s_source_mode == 'historical'", prefix),
      dateInput(paste0(prefix, "_curve_date"), "Historical date")
    ),
    selectInput(paste0(prefix, "_fit_method"), "Fit method",
      choices = c("Nelson-Siegel" = "nelson_siegel", "Spline" = "spline"),
      selected = "nelson_siegel"
    )
  )
}

ui <- navbarPage(
  title = "YieldCurve Trader",
  theme = theme,
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  tabPanel(
    "Curve Explorer",
    fluidPage(
      br(),
      fluidRow(
        column(3, div(class = "sidebar-panel",
          radioButtons("source_mode", "Analytics source",
            choices = c("Zero-rate snapshot (official analytics)" = "zero", "Historical market quotes (proxy)" = "historical"),
            selected = "zero"
          ),
          selectInput("curve_name", "Curve", choices = NULL),
          conditionalPanel("input.source_mode == 'historical'", dateInput("curve_date", "Date")),
          checkboxGroupInput("fit_methods", "Curve fits",
            choices = c("Nelson-Siegel" = "nelson_siegel", "Spline" = "spline"),
            selected = c("nelson_siegel", "spline")
          ),
          actionButton("refresh_data", "Refresh local RDS", class = "btn-primary"),
          hr(), div(class = "small-note", textOutput("loaded_at")),
          div(class = "small-note", textOutput("effective_curve_date"))
        )),
        column(9,
          uiOutput("source_banner"),
          plotlyOutput("curve_plot", height = "500px"),
          explanation_card("How to read this curve", "curve_explanation"),
          fluidRow(column(6, DTOutput("fit_summary")), column(6, DTOutput("ns_parameters")))
        )
      )
    )
  ),
  tabPanel(
    "History & Changes",
    fluidPage(
      br(),
      fluidRow(
        column(3, div(class = "sidebar-panel",
          selectizeInput("history_curves", "Historical curves", choices = NULL, multiple = TRUE),
          selectizeInput("history_dates", "Comparison dates", choices = NULL, multiple = TRUE),
          selectInput("history_base_date", "Base date", choices = NULL)
        )),
        column(9,
          div(class = "proxy-banner", strong("Proxy analysis: "), "each curve is compared with its own selected base date."),
          explanation_card("How to read history comparison", "history_explanation"),
          plotlyOutput("history_absolute_plot", height = "420px"),
          plotlyOutput("history_change_plot", height = "420px"),
          DTOutput("history_comparison_table")
        )
      )
    )
  ),
  tabPanel(
    "Forward Calculator",
    fluidPage(
      br(),
      fluidRow(
        column(3, div(class = "sidebar-panel",
          source_controls("forward", "Forward curve"),
          hr(),
          numericInput("forward_start", "Forward start (years)", 1, min = 0, step = 0.25),
          numericInput("forward_end", "Forward end (years)", 5, min = 0.01, step = 0.25),
          selectInput("forward_compounding", "Compounding",
            choices = c("Annual" = "annual", "Continuous" = "continuous", "Simple" = "simple")
          )
        )),
        column(9,
          uiOutput("forward_banner"),
          fluidRow(column(4, metric_card("Forward Rate", "forward_value")), column(8, DTOutput("forward_result"))),
          explanation_card("What this forward means", "forward_explanation"),
          plotlyOutput("forward_curve_plot", height = "430px")
        )
      )
    )
  ),
  tabPanel(
    "Carry & Roll",
    fluidPage(
      br(),
      tabsetPanel(
        tabPanel(
          "Single Trade",
          br(),
          fluidRow(
            column(3, div(class = "sidebar-panel",
              source_controls("carry", "Carry curve"),
              hr(),
              numericInput("carry_start", "Trade start (years)", 0, min = 0, step = 0.25),
              numericInput("carry_end", "Trade end (years)", 5, min = 0.25, step = 0.25),
              selectInput("carry_hold", "Hold period", choices = c("1M" = 1/12, "3M" = 0.25, "6M" = 0.5, "1Y" = 1), selected = 0.25),
              selectInput("carry_direction", "Direction", choices = c("Receive Fixed", "Pay Fixed")),
              numericInput("dv01", "DV01 per bp", 10000, min = 0, step = 1000)
            )),
            column(9,
              uiOutput("carry_banner"),
              fluidRow(
                column(4, metric_card("Carry", "carry_value")),
                column(4, metric_card("Roll", "roll_value")),
                column(4, metric_card("Total / P&L", "total_value"))
              ),
              explanation_card("How to interpret Carry and Roll", "carry_explanation"),
              fluidRow(
                column(6, plotlyOutput("carry_component_plot", height = "380px")),
                column(6, plotlyOutput("carry_spot_plot", height = "380px"))
              ),
              plotlyOutput("carry_stacked_plot", height = "720px"),
              fluidRow(
                column(7, plotlyOutput("carry_heatmap", height = "480px")),
                column(5, DTOutput("carry_matrix"))
              )
            )
          )
        ),
        tabPanel(
          "Curve Trade",
          br(),
          fluidRow(
            column(3, div(class = "sidebar-panel",
              source_controls("trade", "Curve-trade curve"),
              hr(),
              selectInput("trade_structure", "Structure",
                choices = c(
                  "Steepener" = "steepener", "Flattener" = "flattener",
                  "Long-belly Fly" = "long_belly_fly", "Short-belly Fly" = "short_belly_fly"
                )
              ),
              numericInput("trade_short_tenor", "Short tenor", 2, min = 0.25, step = 0.25),
              numericInput("trade_belly_tenor", "Belly tenor", 5, min = 0.5, step = 0.25),
              numericInput("trade_long_tenor", "Long tenor", 10, min = 1, step = 0.25),
              selectInput("trade_hold", "Hold period", choices = c("1M" = 1/12, "3M" = 0.25, "6M" = 0.5, "1Y" = 1), selected = 0.25),
              numericInput("trade_risk_budget", "DV01-neutral risk budget", 10000, min = 1, step = 1000),
              numericInput("trade_short_dv01", "Short leg DV01", 10000, min = 0, step = 1000),
              numericInput("trade_belly_dv01", "Belly leg DV01", 10000, min = 0, step = 1000),
              numericInput("trade_long_dv01", "Long leg DV01", 10000, min = 0, step = 1000),
              actionButton("load_neutral_dv01", "Load DV01-neutral defaults"),
              actionButton("calculate_curve_trade", "Calculate Curve Trade", class = "btn-primary")
            )),
            column(9,
              uiOutput("trade_banner"),
              fluidRow(
                column(4, metric_card("Portfolio Carry P&L", "trade_carry_pnl")),
                column(4, metric_card("Portfolio Roll P&L", "trade_roll_pnl")),
                column(4, metric_card("Total P&L / Eq. bp", "trade_total_pnl"))
              ),
              explanation_card("How this curve trade is constructed", "trade_explanation"),
              DTOutput("trade_leg_table"),
              fluidRow(
                column(6, plotlyOutput("trade_leg_pnl_plot", height = "430px")),
                column(6, plotlyOutput("trade_component_plot", height = "430px"))
              )
            )
          )
        )
      )
    )
  ),
  tabPanel(
    "Diagnostics",
    fluidPage(
      br(),
      fluidRow(
        column(4, div(class = "sidebar-panel",
          h4("Model policy"),
          p("Zero-rate snapshot: official analytics for forward and carry/roll."),
          p("Historical OIS/IRS quotes: clearly marked proxy; no strict multi-curve bootstrap."),
          p("Internal rates are decimals. Market inputs and displays are percentages."),
          p("P&L is an estimate: total bp multiplied by the user-provided DV01.")
        )),
        column(8, explanation_card("How to read diagnostics", "diagnostics_explanation"), DTOutput("diagnostics_table"), br(), DTOutput("input_points"))
      )
    )
  )
)

server <- function(input, output, session) {
  # market() 是当前浏览器连接共享的数据快照；点击 Refresh 后替换它会触发所有依赖页面重算。
  market <- reactiveVal(load_market_data(project_dir))

  observeEvent(input$refresh_data, {
    market(load_market_data(project_dir))
    showNotification("Local RDS files refreshed.", type = "message")
  })

  curve_choices <- reactive(list(
    zero = zero_curve_names(market()$zero_curve),
    historical = historical_curve_names(market()$wide_rates)
  ))
  available_dates <- reactive(sort(unique(market()$wide_rates$date)))

  update_curve_selector <- function(prefix, mode, preferred_zero = "USD UNITED STATES OIS", preferred_historical = "USD SOFR OIS") {
    if (is.null(mode) || !mode %in% c("zero", "historical")) mode <- "zero"
    choices <- curve_choices()[[mode]]
    curve_id <- if (nzchar(prefix)) paste0(prefix, "_curve_name") else "curve_name"
    date_id <- if (nzchar(prefix)) paste0(prefix, "_curve_date") else "curve_date"
    current <- input[[curve_id]]
    preferred <- if (mode == "zero") preferred_zero else preferred_historical
    selected <- if (!is.null(current) && current %in% choices) current else if (preferred %in% choices) preferred else choices[[1]]
    updateSelectInput(session, curve_id, choices = choices, selected = selected)
    updateDateInput(session, date_id, value = max(available_dates()), min = min(available_dates()), max = max(available_dates()))
  }

  observe({
    # 页面初次打开或数据刷新后，为四组独立曲线选择器补齐 choices，同时尽量保留用户当前选择。
    update_curve_selector("", input$source_mode)
    update_curve_selector("forward", input$forward_source_mode)
    update_curve_selector("carry", input$carry_source_mode)
    update_curve_selector("trade", input$trade_source_mode)
    dates <- available_dates()
    history_curves <- curve_choices()$historical
    default_curves <- intersect(c("USD SOFR OIS", "EUR ESTR OIS"), history_curves)
    default_dates <- as.character(c(dates[max(1, length(dates) - 21)], max(dates)))
    updateSelectizeInput(session, "history_curves", choices = history_curves,
      selected = if (length(input$history_curves)) input$history_curves else default_curves, server = TRUE)
    updateSelectizeInput(session, "history_dates", choices = as.character(dates),
      selected = if (length(input$history_dates)) input$history_dates else default_dates, server = TRUE)
  })

  observeEvent(input$history_dates, {
    selected_dates <- sort(unique(as.character(input$history_dates)))
    if (length(selected_dates)) {
      current <- input$history_base_date
      updateSelectInput(session, "history_base_date", choices = selected_dates,
        selected = if (!is.null(current) && current %in% selected_dates) current else selected_dates[[1]])
    }
  }, ignoreInit = FALSE)

  make_page_curve <- function(prefix) {
    # 例如 prefix="forward" 时，读取 input$forward_*，返回 points + fit + source 的完整 bundle。
    # Forward、Carry 和 Curve Trade 后续只依赖自己的 bundle，因此互不串线。
    reactive({
      mode <- input[[paste0(prefix, "_source_mode")]]
      curve_name <- input[[paste0(prefix, "_curve_name")]]
      method <- input[[paste0(prefix, "_fit_method")]]
      req(mode, curve_name, method)
      date <- input[[paste0(prefix, "_curve_date")]]
      result <- tryCatch(prepare_curve_fit(market(), mode, curve_name, date, method), error = function(error) error)
      if (inherits(result, "error")) validate(need(FALSE, conditionMessage(result)))
      result
    })
  }

  forward_curve <- make_page_curve("forward")
  carry_curve <- make_page_curve("carry")
  trade_curve <- make_page_curve("trade")

  current_points <- reactive({
    req(input$curve_name)
    result <- tryCatch({
      if (identical(input$source_mode, "zero")) extract_zero_curve(market()$zero_curve, input$curve_name)
      else extract_historical_curve(market()$wide_rates, input$curve_name, input$curve_date)
    }, error = function(error) error)
    if (inherits(result, "error")) validate(need(FALSE, conditionMessage(result)))
    result
  })
  current_source <- reactive(curve_source_label(input$source_mode, input$curve_name, input$curve_date))
  current_fits <- reactive({
    methods <- input$fit_methods
    if (is.null(methods) || !length(methods)) methods <- "nelson_siegel"
    result <- tryCatch(lapply(methods, function(method) fit_curve(
      current_points()$tenor, current_points()$rate, method, current_source(), identical(input$source_mode, "historical")
    )), error = function(error) error)
    if (inherits(result, "error")) validate(need(FALSE, conditionMessage(result)))
    result
  })
  analytics_curve <- reactive({
    fits <- current_fits()
    ns <- fits[vapply(fits, function(x) x$method == "nelson_siegel", logical(1))]
    if (length(ns)) ns[[1]] else fits[[1]]
  })

  output$loaded_at <- renderText(paste("Loaded:", format(market()$loaded_at, "%Y-%m-%d %H:%M:%S")))
  output$effective_curve_date <- renderText({
    if (identical(input$source_mode, "zero")) return("Snapshot source: no historical date")
    paste0("Requested: ", attr(current_points(), "requested_date"), " | Effective: ", attr(current_points(), "effective_date"))
  })
  output$source_banner <- renderUI(if (identical(input$source_mode, "historical")) div(class = "proxy-banner", "Historical market quote Proxy") else div(class = "official-banner", "Official zero-rate snapshot"))

  output$curve_plot <- renderPlotly({
    points <- current_points()
    grid <- seq(max(0.01, min(points$tenor)), max(points$tenor), length.out = 300)
    plot_data <- do.call(rbind, lapply(current_fits(), function(fit) data.frame(
      tenor = grid, rate = decimal_to_percent(curve_rate(fit, grid)),
      series = if (fit$method == "nelson_siegel") "Nelson-Siegel" else "Spline"
    )))
    ggplotly(ggplot(plot_data, aes(tenor, rate, color = series)) +
      geom_line(linewidth = 1) +
      geom_point(data = transform(points, rate = decimal_to_percent(rate)), aes(tenor, rate), inherit.aes = FALSE, color = "#d62828", size = 2.5) +
      labs(x = "Tenor (years)", y = "Rate (%)", color = NULL, title = input$curve_name) + theme_minimal(base_size = 13))
  })
  output$fit_summary <- renderDT(datatable(data.frame(
    Method = vapply(current_fits(), `[[`, character(1), "method"),
    RMSE_bp = round(vapply(current_fits(), `[[`, numeric(1), "rmse_bp"), 3),
    Source = vapply(current_fits(), `[[`, character(1), "source"),
    Proxy = vapply(current_fits(), `[[`, logical(1), "proxy")
  ), options = list(dom = "t"), rownames = FALSE))
  output$ns_parameters <- renderDT({
    ns <- current_fits()[vapply(current_fits(), function(x) x$method == "nelson_siegel", logical(1))]
    if (!length(ns)) return(datatable(data.frame(Message = "Nelson-Siegel not selected"), options = list(dom = "t")))
    datatable(data.frame(Parameter = names(ns[[1]]$parameters), Value = round(ns[[1]]$parameters, 6)), options = list(dom = "t"), rownames = FALSE)
  })
  output$curve_explanation <- renderUI({
    fit <- analytics_curve()
    if (fit$method == "nelson_siegel") {
      p(sprintf("Nelson-Siegel summarizes the curve with level, slope and curvature. Current RMSE is %.2f bp; a lower RMSE means the fitted line is closer to observed market points.", fit$rmse_bp))
    } else p(sprintf("Spline prioritizes local fit to market points. Current RMSE is %.2f bp; use it for shape inspection rather than parameter interpretation.", fit$rmse_bp))
  })

  history_data <- reactive({
    # 多选曲线与多选日期组成 curve × date 组合；每条曲线分别减去自己的 base-date 曲线。
    req(input$history_curves, input$history_dates, input$history_base_date)
    result <- tryCatch(build_history_comparison(market()$wide_rates, input$history_curves, input$history_dates, input$history_base_date), error = function(error) error)
    if (inherits(result, "error")) validate(need(FALSE, conditionMessage(result)))
    result
  })
  output$history_absolute_plot <- renderPlotly({
    x <- history_data()
    ggplotly(ggplot(x, aes(tenor, rate_percent, color = curve, linetype = as.character(requested_date),
      text = paste0("Curve: ", curve, "<br>Requested: ", requested_date, "<br>Effective: ", effective_date, "<br>Rate: ", round(rate_percent, 3), "%"))) +
      geom_line(linewidth = 0.9) + geom_point(size = 1.4) +
      labs(title = "Absolute Curves", x = "Tenor", y = "Rate (%)", linetype = "Requested date") + theme_minimal(base_size = 12), tooltip = "text")
  })
  output$history_change_plot <- renderPlotly({
    x <- history_data()
    ggplotly(ggplot(x, aes(tenor, change_bp, color = curve, linetype = as.character(requested_date),
      text = paste0("Curve: ", curve, "<br>Date: ", requested_date, "<br>vs base: ", round(change_bp, 2), " bp"))) +
      geom_hline(yintercept = 0, color = "grey60") + geom_line(linewidth = 0.9) + geom_point(size = 1.4) +
      labs(title = paste("Change vs Base Date", input$history_base_date), x = "Tenor", y = "Change (bp)", linetype = "Requested date") + theme_minimal(base_size = 12), tooltip = "text")
  })
  output$history_comparison_table <- renderDT(datatable(history_data(), options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE))
  output$history_explanation <- renderUI(paste0(
    "Each line is one selected curve/date combination. The lower chart subtracts each curve's own base-date rate at the same tenor. ",
    "Positive bp means that tenor is higher than the base date. Requested and effective dates may differ when quotes are missing."
  ))

  curve_banner <- function(bundle) {
    if (bundle$proxy) div(class = "proxy-banner", paste0(bundle$source, " | Effective: ", bundle$effective_date))
    else div(class = "official-banner", bundle$source)
  }
  output$forward_banner <- renderUI(curve_banner(forward_curve()))
  forward_result <- reactive(calculate_forward(forward_curve()$fit, input$forward_start, input$forward_end, input$forward_compounding))
  output$forward_value <- renderText(sprintf("%.4f%%", forward_result()$forward_percent))
  output$forward_result <- renderDT(datatable(forward_result(), options = list(dom = "t"), rownames = FALSE))
  output$forward_explanation <- renderUI(p(sprintf(
    "This is the implied rate between %.2fY and %.2fY from %s. It is not a forecast; it is the rate embedded in today's selected curve under %s compounding.",
    input$forward_start, input$forward_end, forward_curve()$curve_name, input$forward_compounding
  )))
  output$forward_curve_plot <- renderPlotly({
    fit <- forward_curve()$fit
    grid <- seq(max(0.01, min(fit$points$tenor)), max(fit$points$tenor), length.out = 250)
    marks <- data.frame(tenor = c(input$forward_start, input$forward_end), label = c("Start", "End"))
    marks$rate <- decimal_to_percent(curve_rate(fit, pmax(marks$tenor, min(fit$points$tenor))))
    ggplotly(ggplot(data.frame(tenor = grid, rate = decimal_to_percent(curve_rate(fit, grid))), aes(tenor, rate)) +
      geom_line(color = "#1f4e78", linewidth = 1) + geom_point(data = marks, aes(tenor, rate, color = label), size = 3) +
      labs(title = "Selected Curve and Forward Endpoints", x = "Tenor", y = "Rate (%)", color = NULL) + theme_minimal(base_size = 12))
  })

  output$carry_banner <- renderUI(curve_banner(carry_curve()))
  carry_result <- reactive(calculate_carry_roll(carry_curve()$fit, input$carry_start, input$carry_end, as.numeric(input$carry_hold), input$carry_direction, "annual"))
  output$carry_value <- renderText(sprintf("%.2f bp", carry_result()$carry_bp))
  output$roll_value <- renderText(sprintf("%.2f bp", carry_result()$roll_bp))
  output$total_value <- renderText(sprintf("%.2f bp / %.0f", carry_result()$total_bp, calculate_dv01_pnl(carry_result()$total_bp, input$dv01)))
  carry_matrix_data <- reactive(build_carry_matrix(carry_curve()$fit, c(1, 2, 3, 5, 7, 10, 15, 20, 30), c(1/12, 0.25, 0.5, 1), input$carry_direction, input$dv01, "annual"))
  output$carry_explanation <- renderUI(p(sprintf(
    "Carry (%.2f bp) is the selected trade rate earned over the hold period after short-end funding. Roll (%.2f bp) is the benefit or cost from moving down the unchanged curve. Total (%.2f bp) multiplied by DV01 gives the estimated P&L.",
    carry_result()$carry_bp, carry_result()$roll_bp, carry_result()$total_bp
  )))
  output$carry_component_plot <- renderPlotly({
    x <- data.frame(component = c("Carry", "Roll", "Total"), bp = c(carry_result()$carry_bp, carry_result()$roll_bp, carry_result()$total_bp))
    ggplotly(ggplot(x, aes(component, bp, fill = component, text = paste0(component, ": ", round(bp, 2), " bp"))) +
      geom_col() + geom_hline(yintercept = 0, color = "grey50") + geom_text(aes(label = round(bp, 1)), vjust = ifelse(x$bp >= 0, -0.5, 1.2)) +
      labs(title = "Single Trade Carry / Roll Decomposition", x = NULL, y = "bp") + theme_minimal(base_size = 12) + theme(legend.position = "none"), tooltip = "text")
  })
  output$carry_spot_plot <- renderPlotly({
    fit <- carry_curve()$fit
    tenors <- c(1, 2, 3, 5, 7, 10, 15, 20, 30)
    x <- data.frame(tenor = tenors, spot = decimal_to_percent(curve_rate(fit, tenors)))
    ggplotly(ggplot(x, aes(tenor, spot, text = paste0(tenor, "Y: ", round(spot, 3), "%"))) +
      geom_line(color = "#37474f", linewidth = 1) + geom_point(color = "#37474f", size = 2) + geom_text(aes(label = round(spot, 2)), vjust = -0.7) +
      labs(title = "Spot Curve: Shape Drives Roll", x = "Tenor", y = "Rate (%)") + theme_minimal(base_size = 12), tooltip = "text")
  })
  output$carry_stacked_plot <- renderPlotly({
    x <- carry_matrix_data()
    long <- rbind(
      data.frame(tenor_label = x$tenor_label, hold_label = x$hold_label, component = "Carry", value = x$carry_bp, total = x$total_bp),
      data.frame(tenor_label = x$tenor_label, hold_label = x$hold_label, component = "Roll", value = x$roll_bp, total = x$total_bp)
    )
    chart <- ggplot(long, aes(y = tenor_label, x = value, fill = component)) +
      geom_col(position = "stack") +
      geom_point(data = x, aes(y = tenor_label, x = total_bp), inherit.aes = FALSE, color = "red", size = 2.5) +
      geom_text(data = x, aes(y = tenor_label, x = total_bp, label = round(total_bp, 1)), inherit.aes = FALSE, hjust = -0.2, size = 3) +
      facet_wrap(~hold_label, ncol = 1, scales = "free_x") +
      scale_fill_manual(values = c(Carry = "#3E8ED0", Roll = "#E8A317")) +
      labs(title = "Carry + Roll by Tenor and Hold Period", x = "bp", y = "Tenor", fill = NULL) + theme_minimal(base_size = 11)
    ggplotly(chart)
  })
  output$carry_matrix <- renderDT(datatable(carry_matrix_data()[, c("tenor_label", "hold_label", "carry_bp", "roll_bp", "total_bp", "pnl")], options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE))
  output$carry_heatmap <- renderPlotly({
    x <- carry_matrix_data()
    ggplotly(ggplot(x, aes(tenor_label, hold_label, fill = total_bp, text = paste0("Total: ", round(total_bp, 2), " bp<br>P&L: ", round(pnl)))) +
      geom_tile(color = "white") + geom_text(aes(label = round(total_bp, 1)), size = 3) +
      scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac") +
      labs(title = "Total Carry + Roll Heatmap", x = "Tenor", y = "Hold", fill = "Total bp") + theme_minimal(base_size = 12), tooltip = "text")
  })

  observeEvent(list(input$trade_structure, input$trade_risk_budget), {
    # 改变结构或风险预算时，先把推荐的 DV01-neutral 权重填入三条腿输入框；用户仍可手动覆盖。
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
  trade_result <- eventReactive(input$calculate_curve_trade, {
    # eventReactive 表示只有点击 Calculate Curve Trade 才锁定当前腿部 DV01 并计算，便于先调整结构再成交前复核。
    req(
      input$trade_structure, input$trade_short_tenor, input$trade_belly_tenor,
      input$trade_long_tenor, input$trade_hold, input$trade_risk_budget,
      input$trade_short_dv01, input$trade_belly_dv01, input$trade_long_dv01
    )
    legs <- curve_trade_legs(input$trade_structure, input$trade_short_tenor, input$trade_belly_tenor, input$trade_long_tenor, input$trade_risk_budget)
    if (nrow(legs) == 2) legs$dv01 <- c(input$trade_short_dv01, input$trade_long_dv01)
    else legs$dv01 <- c(input$trade_short_dv01, input$trade_belly_dv01, input$trade_long_dv01)
    calculate_curve_trade(trade_curve()$fit, legs, as.numeric(input$trade_hold), "annual", input$trade_risk_budget)
  }, ignoreInit = FALSE)
  output$trade_banner <- renderUI(curve_banner(trade_curve()))
  output$trade_carry_pnl <- renderText(sprintf("%.0f", trade_result()$summary$carry_pnl))
  output$trade_roll_pnl <- renderText(sprintf("%.0f", trade_result()$summary$roll_pnl))
  output$trade_total_pnl <- renderText(sprintf("%.0f / %.2f bp", trade_result()$summary$total_pnl, trade_result()$summary$equivalent_total_bp))
  output$trade_explanation <- renderUI({
    detail <- trade_result()$detail
    p(paste0(
      "Structure: ", input$trade_structure, ". Legs: ",
      paste(paste(detail$leg, detail$direction, paste0(detail$tenor, "Y"), paste0("DV01 ", round(detail$dv01))), collapse = "; "),
      ". Portfolio P&L is the sum of each leg's direction-adjusted carry and roll. Equivalent bp divides total P&L by the selected risk budget."
    ))
  })
  output$trade_leg_table <- renderDT(datatable(trade_result()$detail[, c("leg", "tenor", "direction", "dv01", "carry_bp", "roll_bp", "total_bp", "carry_pnl", "roll_pnl", "total_pnl")], options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE))
  output$trade_leg_pnl_plot <- renderPlotly({
    x <- trade_result()$detail
    ggplotly(ggplot(x, aes(leg, total_pnl, fill = direction, text = paste0(leg, "<br>", direction, "<br>Total P&L: ", round(total_pnl)))) +
      geom_col() + geom_hline(yintercept = 0, color = "grey50") + geom_text(aes(label = round(total_pnl)), vjust = ifelse(x$total_pnl >= 0, -0.4, 1.2)) +
      labs(title = "Curve Trade P&L by Leg", x = NULL, y = "P&L") + theme_minimal(base_size = 12), tooltip = "text")
  })
  output$trade_component_plot <- renderPlotly({
    x <- data.frame(component = c("Carry", "Roll", "Total"), pnl = c(trade_result()$summary$carry_pnl, trade_result()$summary$roll_pnl, trade_result()$summary$total_pnl))
    ggplotly(ggplot(x, aes(component, pnl, fill = component, text = paste0(component, ": ", round(pnl)))) +
      geom_col() + geom_hline(yintercept = 0, color = "grey50") + geom_text(aes(label = round(pnl)), vjust = ifelse(x$pnl >= 0, -0.4, 1.2)) +
      labs(title = "Portfolio Carry / Roll P&L", x = NULL, y = "P&L") + theme_minimal(base_size = 12) + theme(legend.position = "none"), tooltip = "text")
  })

  output$diagnostics_explanation <- renderUI(p(sprintf(
    "Observed is the market input, fitted is the selected model value, and residual is observed minus fitted in bp. Current model RMSE is %.2f bp.",
    analytics_curve()$rmse_bp
  )))
  output$diagnostics_table <- renderDT(datatable(analytics_curve()$diagnostics, options = list(pageLength = 10), rownames = FALSE))
  output$input_points <- renderDT(datatable(transform(current_points(), rate_percent = decimal_to_percent(rate)), options = list(pageLength = 10), rownames = FALSE))
}

shinyApp(ui, server)
