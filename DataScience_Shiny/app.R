# ============================================================
# Data Science Encyclopedia Shiny App
# ============================================================
# 中文说明：
# 这是一个全新的 Shiny 项目入口，只服务 DataScience_Shiny。
# 不读取或复用其他项目的 app.R 路径。

source(file.path("R", "packages.R"))
use_project_library(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
load_required_packages()

source(file.path("R", "data_loader.R"))
source(file.path("R", "catalog.R"))
source(file.path("R", "examples.R"))

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_dir, "data")

method_catalog <- get_method_catalog()
source_method_map <- get_source_method_map()
method_network <- get_method_network()
method_choices <- split(
  stats::setNames(method_catalog$method_id, method_catalog$method_name),
  method_catalog$category
)

data_bundle <- load_wide_data(data_dir)

ui <- bslib::page_sidebar(
  title = "Data Science Encyclopedia",
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#335C67"
  ),
  fillable = TRUE,
  sidebar = bslib::sidebar(
    width = 330,
    title = "Method Index",
    selectInput(
      "method_select",
      "Choose a method",
      choices = method_choices,
      selected = "linear_regression",
      width = "100%"
    ),
    tags$hr(),
    tags$p(
      class = "small-muted",
      "Use the navigator network to move from a data question to the matching method."
    ),
    actionButton("show_network_help", "Navigator note", class = "btn-outline-secondary")
  ),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  div(
    class = "app-shell",
    bslib::navset_tab(
      id = "main_tabs",
      bslib::nav_panel(
        "Method Navigator",
        bslib::card(
          bslib::card_header("Data question -> method network"),
          visNetwork::visNetworkOutput("method_network", height = "620px")
        )
      ),
      bslib::nav_panel(
        "Method Detail",
        uiOutput("method_header"),
        uiOutput("method_notes"),
        bslib::layout_columns(
          col_widths = c(7, 5),
          bslib::card(
            bslib::card_header("Case Output"),
            plotly::plotlyOutput("example_plot", height = "410px")
          ),
          bslib::card(
            bslib::card_header("Variable Meanings"),
            DT::DTOutput("variable_table")
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            bslib::card_header("Result Table"),
            DT::DTOutput("result_table")
          ),
          bslib::card(
            bslib::card_header("Model Summary"),
            verbatimTextOutput("model_summary")
          )
        ),
        bslib::card(
          bslib::card_header("Reusable R Code"),
          uiOutput("code_snippet")
        ),
        bslib::card(
          bslib::card_header("Mapped from DataScience.R"),
          DT::DTOutput("method_source_map")
        )
      ),
      bslib::nav_panel(
        "Catalog",
        bslib::card(
          bslib::card_header("Two-level method catalog"),
          DT::DTOutput("catalog_table")
        )
      ),
      bslib::nav_panel(
        "Source Map",
        bslib::card(
          bslib::card_header("DataScience.R method-to-tab mapping"),
          tags$p(
            class = "small-muted",
            "Every row maps a source-script method or code block to the Shiny category and method detail page where it can be found."
          ),
          DT::DTOutput("source_map_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  selected_method <- reactive({
    input$method_select %||% "linear_regression"
  })

  selected_catalog_row <- reactive({
    method_catalog[method_catalog$method_id == selected_method(), , drop = FALSE][1, ]
  })

  selected_example <- reactive({
    run_example(selected_catalog_row()$example_id, data_bundle)
  })

  observeEvent(input$show_network_help, {
    showModal(modalDialog(
      title = "How to use Method Navigator",
      "Click a method node in the network. The app will jump to the detail page for that method. Non-method nodes explain the decision path from data type and analysis goal.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$method_network_selected, {
    selected <- input$method_network_selected
    node_id <- if (is.list(selected)) selected$nodes[1] else selected[1]
    if (is.null(node_id) || is.na(node_id)) return()

    node_row <- method_network$nodes[method_network$nodes$id == node_id, , drop = FALSE]
    if (nrow(node_row) == 1 && !is.na(node_row$method_id)) {
      updateSelectInput(session, "method_select", selected = node_row$method_id)
      updateTabsetPanel(session, "main_tabs", selected = "Method Detail")
    }
  })

  output$method_network <- visNetwork::renderVisNetwork({
    nodes <- method_network$nodes
    nodes$title <- ifelse(
      is.na(nodes$method_id),
      nodes$label,
      paste0(nodes$label, "<br>Click to open method detail.")
    )
    nodes$shape <- ifelse(nodes$group == "method", "box", "ellipse")
    nodes$color.background <- c(
      question = "#F4A261",
      data_type = "#A8DADC",
      goal = "#E9C46A",
      method = "#B7E4C7"
    )[nodes$group]

    visNetwork::visNetwork(nodes, method_network$edges, width = "100%") |>
      visNetwork::visGroups(groupname = "method", shape = "box") |>
      visNetwork::visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = FALSE,
        selectedBy = "group"
      ) |>
      visNetwork::visLayout(randomSeed = 42) |>
      visNetwork::visPhysics(stabilization = TRUE)
  })

  output$method_header <- renderUI({
    row <- selected_catalog_row()
    example <- selected_example()
    tags$div(
      class = "method-header",
      tags$div(class = "method-title", row$method_name),
      tags$div(class = "method-subtitle", paste(row$category, "-", example$title)),
      tags$p(example$background)
    )
  })

  output$method_notes <- renderUI({
    notes <- get_method_notes(selected_method())
    tags$div(
      class = "note-grid",
      note_box("When to use", notes[["when"]]),
      note_box("Assumptions", notes[["assumptions"]]),
      note_box("Inputs", notes[["inputs"]]),
      note_box("Outputs", notes[["outputs"]]),
      note_box("Interpretation", notes[["interpretation"]])
    )
  })

  output$example_plot <- plotly::renderPlotly({
    example <- selected_example()
    if (is.null(example$plot)) {
      empty_plot <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "Live plot pending for this method.", size = 5) +
        ggplot2::theme_void()
      return(plotly::ggplotly(empty_plot))
    }
    plotly::ggplotly(example$plot)
  })

  output$variable_table <- DT::renderDT({
    DT::datatable(
      describe_variables(selected_method()),
      rownames = FALSE,
      options = list(pageLength = 9, dom = "tip")
    )
  })

  output$result_table <- DT::renderDT({
    example <- selected_example()
    DT::datatable(
      example$table,
      rownames = TRUE,
      options = list(pageLength = 8, scrollX = TRUE)
    )
  })

  output$model_summary <- renderText({
    summary_text <- selected_example()$model_summary
    if (length(summary_text) > 1) {
      paste(summary_text, collapse = "\n")
    } else {
      summary_text
    }
  })

  output$code_snippet <- renderUI({
    tags$div(
      class = "code-box",
      tags$pre(tags$code(selected_example()$code))
    )
  })

  output$catalog_table <- DT::renderDT({
    DT::datatable(
      method_catalog[, c("category", "method_name", "method_id")],
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 24, scrollX = TRUE)
    )
  })

  output$method_source_map <- DT::renderDT({
    mapped_rows <- source_method_map[source_method_map$method_id == selected_method(), , drop = FALSE]
    DT::datatable(
      mapped_rows[, c("source_lines", "source_section", "source_method", "mapping_note")],
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE)
    )
  })

  output$source_map_table <- DT::renderDT({
    DT::datatable(
      source_method_map[, c(
        "source_lines", "source_section", "source_method",
        "category", "method_name", "method_id", "mapping_note"
      )],
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })
}

note_box <- function(title, text) {
  tags$div(
    class = "note-box",
    tags$h4(title),
    tags$p(text)
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

shinyApp(ui, server)
