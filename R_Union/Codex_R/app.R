# ============================================================
# ECO Screener Shiny Dashboard
# ============================================================

required_packages <- c(
  "shiny", "bslib", "ggplot2", "plotly", "DT",
  "dplyr", "tidyr", "base64enc", "zoo"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_packages, collapse = ", "), "\n",
    "Install them with:\n",
    "install.packages(c(", paste(sprintf('\"%s\"', missing_packages), collapse = ", "), "))",
    call. = FALSE
  )
}

project_dir <- "C:/Users/PC/Desktop/R_git/R_Union"
optimized_script <- file.path(project_dir, "Codex_R", "Eco_screener_optimized.R")
wide_eco_file <- file.path(project_dir, "WIDE_ALLX")

wide_eco_data <- readRDS(wide_eco_file)
wide_eco_data$date <- as.Date(wide_eco_data$date)
available_dates <- sort(unique(wide_eco_data$date))
min_available_date <- min(available_dates, na.rm = TRUE)
max_available_date <- max(available_dates, na.rm = TRUE)

default_countries <- c("UNITED STATES", "EUROZONE", "JAPAN", "BRITAIN", "GERMANY")
default_indicators <- c("CPI", "Real GDP", "PMI", "Unemployment")
history_window_choices <- c("3 years" = 3, "5 years" = 5, "10 years" = 10, "All history" = 0)

resolve_effective_date <- function(selected_date, dates) {
  selected_date <- as.Date(selected_date)
  if (selected_date %in% dates) return(selected_date)
  earlier_dates <- dates[dates <= selected_date]
  if (length(earlier_dates) == 0) {
    stop("No data is available on or before ", selected_date, call. = FALSE)
  }
  max(earlier_dates)
}

scale_to_01 <- function(values) {
  if (all(is.na(values))) return(rep(NA_real_, length(values)))
  value_min <- min(values, na.rm = TRUE)
  value_max <- max(values, na.rm = TRUE)
  if (isTRUE(all.equal(value_min, value_max))) return(rep(0.5, length(values)))
  (values - value_min) / (value_max - value_min)
}

last_observation_carried_forward <- function(values) {
  zoo::na.locf(values, na.rm = FALSE)
}

last_non_missing <- function(values) {
  values <- values[!is.na(values)]
  if (length(values) == 0) return(NA_real_)
  dplyr::last(values)
}

img_data_uri <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  paste0("data:image/png;base64,", base64enc::base64encode(path))
}

build_country_indicator_map <- function(indicator_column_map, get_country_from_col) {
  out <- list()
  for (indicator in names(indicator_column_map)) {
    columns <- indicator_column_map[[indicator]]
    if (length(columns) == 0) next
    for (column in columns) {
      country <- get_country_from_col(column)
      if (is.na(country)) next
      out[[paste(country, indicator, sep = "::")]] <- list(
        country = country,
        indicator = indicator,
        column = column
      )
    }
  }
  out
}

history_start_date <- function(end_date, years) {
  if (is.null(years) || years == 0) return(min_available_date)
  as.Date(end_date) - round(as.numeric(years) * 365.25)
}

build_history_raw_data <- function(data, column_map, countries, indicators, start_date, end_date) {
  date_mask <- data$date >= start_date & data$date <= end_date
  date_values <- data$date[date_mask]
  rows <- list()

  for (country in countries) {
    for (indicator in indicators) {
      map_key <- paste(country, indicator, sep = "::")
      entry <- column_map[[map_key]]
      if (is.null(entry) || !entry$column %in% names(data)) next

      raw_values <- suppressWarnings(as.numeric(data[[entry$column]][date_mask]))
      rows[[length(rows) + 1]] <- data.frame(
        date = date_values,
        Country = country,
        Indicator = indicator,
        Value = last_observation_carried_forward(raw_values),
        Source = entry$column,
        check.names = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(date = as.Date(character()), Country = character(), Indicator = character(), Value = numeric()))
  }
  dplyr::bind_rows(rows)
}

build_history_score_data <- function(raw_history) {
  if (nrow(raw_history) == 0) {
    return(data.frame(date = as.Date(character()), Country = character(), Score = numeric()))
  }

  score_source <- raw_history |>
    dplyr::group_by(date, Indicator) |>
    dplyr::mutate(Scaled = scale_to_01(Value)) |>
    dplyr::ungroup()

  score_source |>
    dplyr::group_by(date, Country) |>
    dplyr::summarise(Score = mean(Scaled, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Score = ifelse(is.nan(Score), NA_real_, Score))
}

build_current_profile_data <- function(raw_history, effective_date) {
  if (nrow(raw_history) == 0) {
    return(data.frame(Country = character(), Indicator = character(), Score = numeric()))
  }

  latest_values <- raw_history |>
    dplyr::filter(date <= effective_date) |>
    dplyr::arrange(date) |>
    dplyr::group_by(Country, Indicator) |>
    dplyr::summarise(Value = last_non_missing(Value), .groups = "drop")

  latest_values |>
    dplyr::group_by(Indicator) |>
    dplyr::mutate(Score = scale_to_01(Value)) |>
    dplyr::ungroup()
}

run_eco_report <- function(selected_date) {
  effective_date <- resolve_effective_date(selected_date, available_dates)
  previous_global_date_exists <- exists("date", envir = .GlobalEnv, inherits = FALSE)
  previous_global_date <- if (previous_global_date_exists) get("date", envir = .GlobalEnv) else NULL
  assign("date", as.character(effective_date), envir = .GlobalEnv)

  on.exit({
    if (previous_global_date_exists) {
      assign("date", previous_global_date, envir = .GlobalEnv)
    } else if (exists("date", envir = .GlobalEnv, inherits = FALSE)) {
      rm("date", envir = .GlobalEnv)
    }
  }, add = TRUE)

  report_env <- new.env(parent = .GlobalEnv)
  source(optimized_script, local = report_env)

  column_map <- build_country_indicator_map(
    get("indicator_column_map", envir = report_env),
    get("get_country_from_col", envir = report_env)
  )

  list(
    requested_date = as.Date(selected_date),
    effective_date = as.Date(get("date", envir = report_env)),
    table = get("macro_score_table", envir = report_env),
    heatmap = get("heatmap_file", envir = report_env),
    bars = get("bar_file", envir = report_env),
    radar = get("radar_file", envir = report_env),
    html = get("html_file", envir = report_env),
    csv = get("csv_file", envir = report_env),
    countries = rownames(get("result_scaled", envir = report_env)),
    indicators = names(get("macro_indicator_specs", envir = report_env)),
    column_map = column_map
  )
}

theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2563eb"
)

ui <- shiny::fluidPage(
  theme = theme,
  shiny::tags$head(
    shiny::tags$title("ECO Screener Dashboard"),
    shiny::tags$style(shiny::HTML("
      body { background: #f4f7fb; color: #18212f; }
      .app-shell { max-width: 1500px; margin: 0 auto; padding: 24px; }
      .hero { display: flex; justify-content: space-between; gap: 18px; align-items: flex-end; margin-bottom: 18px; }
      .hero h1 { margin: 0; font-weight: 760; letter-spacing: 0; }
      .hero p { margin: 6px 0 0; color: #5d6b7c; }
      .control-panel, .panel, .metric-card {
        background: #ffffff; border: 1px solid #dbe4ef; border-radius: 8px;
        box-shadow: 0 8px 22px rgba(15, 23, 42, 0.05);
      }
      .control-panel { padding: 16px; position: sticky; top: 12px; }
      .metric-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 12px; margin-bottom: 16px; }
      .metric-card { padding: 14px 16px; min-height: 92px; }
      .metric-label { color: #687789; font-size: 12px; text-transform: uppercase; letter-spacing: .04em; }
      .metric-value { font-size: 24px; font-weight: 760; line-height: 1.2; margin-top: 8px; word-break: break-word; }
      .panel { padding: 16px; margin-bottom: 16px; }
      .panel-title { font-size: 18px; font-weight: 720; margin: 0 0 12px; }
      .plot-img { display: block; width: 100%; height: auto; border-radius: 6px; border: 1px solid #e3eaf3; }
      .muted { color: #687789; }
      .report-link { word-break: break-all; }
      .nav-tabs { margin-bottom: 16px; }
      .form-label, label { font-weight: 650; color: #2c3a4b; }
      @media (max-width: 1000px) {
        .hero { display: block; }
        .metric-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        .control-panel { position: static; margin-bottom: 16px; }
      }
      @media (max-width: 620px) {
        .app-shell { padding: 14px; }
        .metric-grid { grid-template-columns: 1fr; }
      }
    "))
  ),
  shiny::div(
    class = "app-shell",
    shiny::div(
      class = "hero",
      shiny::div(
        shiny::h1("ECO Screener Dashboard"),
        shiny::p("Developed-market macro scores, indicator history, and generated report assets.")
      ),
      shiny::actionButton("run", "Refresh Dashboard", class = "btn-primary")
    ),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::div(
          class = "control-panel",
          shiny::dateInput(
            inputId = "date",
            label = "Requested date",
            value = max_available_date,
            min = min_available_date,
            max = max_available_date,
            format = "yyyy-mm-dd"
          ),
          shiny::uiOutput("country_selector"),
          shiny::uiOutput("indicator_selector"),
          shiny::selectInput("history_years", "History window", choices = history_window_choices, selected = 5),
          shiny::hr(),
          shiny::p(class = "muted", "If the requested date has no data, the dashboard uses the nearest earlier available date.")
        )
      ),
      shiny::column(
        width = 9,
        shiny::uiOutput("metric_cards"),
        shiny::tabsetPanel(
          id = "main_tabs",
          shiny::tabPanel(
            "Overview",
            shiny::div(class = "panel", shiny::div(class = "panel-title", "Scoreboard Heatmap"), shiny::uiOutput("heatmap_img")),
            shiny::div(class = "panel", shiny::div(class = "panel-title", "Indicator Bars"), shiny::uiOutput("bar_img")),
            shiny::div(class = "panel", shiny::div(class = "panel-title", "All Countries Radar"), shiny::uiOutput("radar_img"))
          ),
          shiny::tabPanel(
            "History",
            shiny::div(class = "panel", shiny::div(class = "panel-title", "Raw Indicator Trends"), plotly::plotlyOutput("raw_trend_plot", height = "440px")),
            shiny::div(class = "panel", shiny::div(class = "panel-title", "Normalized Macro Score Trend"), plotly::plotlyOutput("score_trend_plot", height = "420px")),
            shiny::div(class = "panel", shiny::div(class = "panel-title", "Current Strengths and Weaknesses"), plotly::plotlyOutput("profile_plot", height = "420px"))
          ),
          shiny::tabPanel(
            "Scoreboard",
            shiny::div(class = "panel", DT::DTOutput("score_table"))
          ),
          shiny::tabPanel(
            "Generated Report",
            shiny::div(class = "panel", shiny::uiOutput("report_summary"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  report_result <- shiny::eventReactive(input$run, {
    run_eco_report(input$date)
  }, ignoreInit = FALSE)

  shiny::observeEvent(report_result(), {
    res <- report_result()
    country_choices <- sort(res$countries)
    indicator_choices <- res$indicators
    selected_countries <- intersect(default_countries, country_choices)
    selected_indicators <- intersect(default_indicators, indicator_choices)

    shiny::updateSelectizeInput(session, "countries", choices = country_choices, selected = selected_countries, server = TRUE)
    shiny::updateSelectizeInput(session, "indicators", choices = indicator_choices, selected = selected_indicators, server = TRUE)
  })

  output$country_selector <- shiny::renderUI({
    shiny::selectizeInput(
      "countries",
      "Countries",
      choices = default_countries,
      selected = default_countries,
      multiple = TRUE,
      options = list(plugins = list("remove_button"), maxItems = 8)
    )
  })

  output$indicator_selector <- shiny::renderUI({
    shiny::selectizeInput(
      "indicators",
      "Indicators",
      choices = default_indicators,
      selected = default_indicators,
      multiple = TRUE,
      options = list(plugins = list("remove_button"), maxItems = 6)
    )
  })

  selected_history <- shiny::reactive({
    res <- report_result()
    countries <- input$countries %||% intersect(default_countries, res$countries)
    indicators <- input$indicators %||% intersect(default_indicators, res$indicators)
    years <- as.numeric(input$history_years %||% 5)
    start_date <- history_start_date(res$effective_date, years)

    build_history_raw_data(
      data = wide_eco_data,
      column_map = res$column_map,
      countries = countries,
      indicators = indicators,
      start_date = start_date,
      end_date = res$effective_date
    )
  })

  output$metric_cards <- shiny::renderUI({
    res <- report_result()
    score_table <- res$table
    top_country <- score_table$Country[which.max(score_table$Total_Score)]
    low_country <- score_table$Country[which.min(score_table$Total_Score)]
    date_caption <- if (res$requested_date == res$effective_date) {
      as.character(res$effective_date)
    } else {
      paste0(as.character(res$effective_date), " (fallback from ", as.character(res$requested_date), ")")
    }

    shiny::div(
      class = "metric-grid",
      shiny::div(class = "metric-card", shiny::div(class = "metric-label", "Effective Date"), shiny::div(class = "metric-value", date_caption)),
      shiny::div(class = "metric-card", shiny::div(class = "metric-label", "Top Score"), shiny::div(class = "metric-value", top_country)),
      shiny::div(class = "metric-card", shiny::div(class = "metric-label", "Lowest Score"), shiny::div(class = "metric-value", low_country)),
      shiny::div(class = "metric-card", shiny::div(class = "metric-label", "Countries"), shiny::div(class = "metric-value", nrow(score_table)))
    )
  })

  output$heatmap_img <- shiny::renderUI({
    src <- img_data_uri(report_result()$heatmap)
    if (is.null(src)) return(shiny::p(class = "muted", "Heatmap file is not available."))
    shiny::tags$img(class = "plot-img", src = src, alt = "ECO macro scoreboard heatmap")
  })

  output$bar_img <- shiny::renderUI({
    src <- img_data_uri(report_result()$bars)
    if (is.null(src)) return(shiny::p(class = "muted", "Bar chart file is not available."))
    shiny::tags$img(class = "plot-img", src = src, alt = "Indicator bar charts")
  })

  output$radar_img <- shiny::renderUI({
    src <- img_data_uri(report_result()$radar)
    if (is.null(src)) return(shiny::p(class = "muted", "Radar chart file is not available."))
    shiny::tags$img(class = "plot-img", src = src, alt = "All countries radar chart")
  })

  output$raw_trend_plot <- plotly::renderPlotly({
    raw_history <- selected_history()
    shiny::validate(shiny::need(nrow(raw_history) > 0, "No history data is available for the selected countries and indicators."))

    plot_data <- raw_history |>
      dplyr::filter(!is.na(Value)) |>
      dplyr::mutate(Series = paste(Country, Indicator, sep = " - "))

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = Value, color = Series, text = Source)) +
      ggplot2::geom_line(linewidth = 0.7, alpha = 0.9) +
      ggplot2::facet_wrap(ggplot2::vars(Indicator), scales = "free_y", ncol = 2) +
      ggplot2::labs(x = NULL, y = "Raw value", color = NULL) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())

    plotly::ggplotly(p, tooltip = c("x", "y", "colour", "text")) |>
      plotly::layout(legend = list(orientation = "h", y = -0.2))
  })

  output$score_trend_plot <- plotly::renderPlotly({
    score_history <- build_history_score_data(selected_history())
    shiny::validate(shiny::need(nrow(score_history) > 0, "No score history is available for the selected controls."))

    p <- ggplot2::ggplot(score_history, ggplot2::aes(x = date, y = Score, color = Country)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(x = NULL, y = "Normalized score", color = NULL) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())

    plotly::ggplotly(p, tooltip = c("x", "y", "colour")) |>
      plotly::layout(legend = list(orientation = "h", y = -0.2))
  })

  output$profile_plot <- plotly::renderPlotly({
    res <- report_result()
    profile_data <- build_current_profile_data(selected_history(), res$effective_date)
    shiny::validate(shiny::need(nrow(profile_data) > 0, "No current profile data is available for the selected controls."))

    p <- ggplot2::ggplot(profile_data, ggplot2::aes(x = Indicator, y = Score, fill = Country)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.68) +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::labs(x = NULL, y = "Current normalized score", fill = NULL) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
      )

    plotly::ggplotly(p, tooltip = c("x", "y", "fill")) |>
      plotly::layout(legend = list(orientation = "h", y = -0.25))
  })

  output$score_table <- DT::renderDT({
    DT::datatable(
      report_result()$table,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 12, scrollX = TRUE, dom = "tip")
    ) |>
      DT::formatRound(columns = names(report_result()$table)[vapply(report_result()$table, is.numeric, logical(1))], digits = 3)
  })

  output$report_summary <- shiny::renderUI({
    res <- report_result()
    shiny::tagList(
      shiny::p(shiny::strong("Requested date: "), as.character(res$requested_date)),
      shiny::p(shiny::strong("Effective data date: "), as.character(res$effective_date)),
      shiny::p(shiny::strong("HTML report: "), shiny::span(class = "report-link", res$html)),
      shiny::p(shiny::strong("CSV scoreboard: "), shiny::span(class = "report-link", res$csv)),
      shiny::p(class = "muted", "Generated image assets are embedded in the Overview tab. The standalone HTML report is written next to those assets in the output folder.")
    )
  })
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

shiny::shinyApp(ui, server)
