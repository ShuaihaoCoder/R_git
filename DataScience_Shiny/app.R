# ============================================================
# Data Science Encyclopedia Shiny App
# ============================================================
# 整体功能：
# app.R 定义网页长什么样（ui）以及用户操作后程序做什么（server）。
# 数据计算和方法定义放在 R/ 文件夹，app.R 主要负责把这些结果连接到网页组件。
#
# source() 执行另一个 R 文件，把其中定义的函数加载到当前 app session。
source(file.path("R", "packages.R"))
use_project_library(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
load_required_packages()

source(file.path("R", "data_loader.R"))
source(file.path("R", "catalog.R"))
source(file.path("R", "examples.R"))

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_dir, "data")

# 下面四个对象分别提供：方法目录、原脚本映射、导航网络、侧边栏选项。
method_catalog <- get_method_catalog()
source_method_map <- get_source_method_map()
method_network <- get_method_network()
# split(x, group) 按 category 分组，因此 selectInput 会显示两级目录。
# setNames(method_id, method_name) 表示网页显示 method_name，但选择后返回 method_id。
method_choices <- split(
  stats::setNames(method_catalog$method_id, method_catalog$method_name),
  method_catalog$category
)

# data_bundle 只在 app 启动时读取一次，所有案例共享，避免每次点击都重新读取大型文件。
data_bundle <- load_wide_data(data_dir)

# ============================================================
# UI: 定义浏览器中显示的页面结构
# ============================================================
# page_sidebar() 创建左侧栏 + 主内容区。
# title 是网页标题；theme 是页面主题；fillable 允许内容填充可用高度。
ui <- bslib::page_sidebar(
  # 设置网页标题、Bootstrap 主题和主色。
  title = "Data Science Encyclopedia",
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#335C67"
  ),
  fillable = TRUE,
  # 左侧栏提供两级方法选择器和网络图使用提示。
  sidebar = bslib::sidebar(
    width = 330,
    title = "Method Index",
    # selectInput() 创建下拉选择器。
    # inputId = "method_select" 会在 server 中通过 input$method_select 读取。
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
  # 将 www/styles.css 加载到网页中。
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  div(
    class = "app-shell",
    # navset_tab() 创建顶部 tab；id 用于 server 主动切换当前 tab。
    bslib::navset_tab(
      id = "main_tabs",
      # Method Navigator tab 展示从数据问题到统计方法的网络图。
      bslib::nav_panel(
        "Method Navigator",
        bslib::card(
          bslib::card_header("Data question -> method network"),
          # *Output() 只在 UI 预留位置；真正内容由 server 中对应 render*() 生成。
          visNetwork::visNetworkOutput("method_network", height = "620px")
        )
      ),
      # Method Detail tab 展示当前方法说明、案例结果和原脚本映射。
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
      # Catalog tab 提供可搜索的完整两级方法目录。
      bslib::nav_panel(
        "Catalog",
        bslib::card(
          bslib::card_header("Two-level method catalog"),
          DT::DTOutput("catalog_table")
        )
      ),
      # Source Map tab 展示 DataScience.R 每个代码段对应的网页方法。
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

# ============================================================
# Server: 定义用户操作后的计算与输出
# ============================================================
# input 读取网页输入；output 写入网页输出；session 控制当前浏览器连接。
server <- function(input, output, session) {
  # reactive() 创建会自动更新的值。
  # input$method_select 改变时，依赖 selected_method() 的输出会自动重新计算。
  selected_method <- reactive({
    input$method_select %||% "linear_regression"
  })

  selected_catalog_row <- reactive({
    # 使用当前 method_id 从目录中取出唯一对应行。
    method_catalog[method_catalog$method_id == selected_method(), , drop = FALSE][1, ]
  })

  selected_example <- reactive({
    # run_example() 是案例统一入口，返回 title/background/table/plot/model_summary。
    run_example(selected_catalog_row()$example_id, data_bundle)
  })

  # observeEvent(event, handler) 监听一次用户事件。
  # 点击 Navigator note 按钮后，showModal() 弹出说明窗口。
  observeEvent(input$show_network_help, {
    showModal(modalDialog(
      title = "How to use Method Navigator",
      "Click a method node in the network. The app will jump to the detail page for that method. Non-method nodes explain the decision path from data type and analysis goal.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$method_network_selected, {
    # visNetwork 点击节点后，会把所选节点 ID 写入 input$method_network_selected。
    selected <- input$method_network_selected
    node_id <- if (is.list(selected)) selected$nodes[1] else selected[1]
    if (is.null(node_id) || is.na(node_id)) return()

    node_row <- method_network$nodes[method_network$nodes$id == node_id, , drop = FALSE]
    if (nrow(node_row) == 1 && !is.na(node_row$method_id)) {
      # 更新左侧选择器，并跳到 Method Detail tab。
      updateSelectInput(session, "method_select", selected = node_row$method_id)
      updateTabsetPanel(session, "main_tabs", selected = "Method Detail")
    }
  })

  # renderVisNetwork() 生成 UI 中 method_network 对应的交互网络图。
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

    # |> 把左侧结果作为右侧函数的第一个参数，逐步添加网络图设置。
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

  # renderUI() 返回动态 HTML；方法改变时标题和背景会同步更新。
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
    # 取得当前方法的五类说明，并重复使用 note_box() 生成 HTML。
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

  # renderPlotly() 把 ggplot 转成浏览器可缩放、可悬停的交互图。
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

  # renderDT() 生成交互表格；pageLength 是每页行数，dom 控制表格工具布局。
  output$variable_table <- DT::renderDT({
    DT::datatable(
      describe_variables(selected_method()),
      rownames = FALSE,
      options = list(pageLength = 9, dom = "tip")
    )
  })

  output$result_table <- DT::renderDT({
    # selected_example()$table 可能是模型系数、相关矩阵或其他案例结果。
    example <- selected_example()
    DT::datatable(
      example$table,
      rownames = TRUE,
      options = list(pageLength = 8, scrollX = TRUE)
    )
  })

  # renderText() 输出纯文本；多行 character 先用 collapse 合并。
  output$model_summary <- renderText({
    summary_text <- selected_example()$model_summary
    if (length(summary_text) > 1) {
      paste(summary_text, collapse = "\n")
    } else {
      summary_text
    }
  })

  output$code_snippet <- renderUI({
    # 使用 pre/code 标签保留 R 代码的换行和等宽字体。
    tags$div(
      class = "code-box",
      tags$pre(tags$code(selected_example()$code))
    )
  })

  output$catalog_table <- DT::renderDT({
    # 目录表启用顶部筛选框，便于按分类或方法名称搜索。
    DT::datatable(
      method_catalog[, c("category", "method_name", "method_id")],
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 24, scrollX = TRUE)
    )
  })

  # 只筛选当前 method_id 对应的原 DataScience.R 代码映射。
  output$method_source_map <- DT::renderDT({
    mapped_rows <- source_method_map[source_method_map$method_id == selected_method(), , drop = FALSE]
    DT::datatable(
      mapped_rows[, c("source_lines", "source_section", "source_method", "mapping_note")],
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE)
    )
  })

  output$source_map_table <- DT::renderDT({
    # 展示全部原脚本映射，并允许在每一列顶部筛选。
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

# note_box() 是 UI helper。
# 参数 title/text 分别是说明卡片标题和内容；返回一个 HTML div。
note_box <- function(title, text) {
  tags$div(
    class = "note-box",
    tags$h4(title),
    tags$p(text)
  )
}

# 自定义空值默认操作符：x 为 NULL 时返回 y，否则返回 x。
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

shinyApp(ui, server)
