# ============================================================
# Data Science Encyclopedia Shiny App
# ============================================================
# 整体功能：连接左侧方法目录、Method Navigator 和完整 Case Study 页面。
# 统计计算放在 R/ 文件夹；本文件负责页面布局、案例缓存、进度显示和用户操作。

source(file.path("R", "packages.R"), encoding = "UTF-8")
# use_project_library() 将项目自己的 R_library 放到 package 查找路径最前面。
use_project_library(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
# load_required_packages() 检查并加载 packages.R 中列出的网页依赖。
load_required_packages()

# 下面四个 source() 不是立即运行案例，而是把其他文件定义的函数加载到当前 app.R。
# 例如加载后，app.R 才能调用 load_wide_data() 读取数据，再把 data_bundle 传给 run_example("var", data_bundle)。
source(file.path("R", "data_loader.R"), encoding = "UTF-8")
source(file.path("R", "catalog.R"), encoding = "UTF-8")
source(file.path("R", "case_helpers.R"), encoding = "UTF-8")
source(file.path("R", "examples_complete.R"), encoding = "UTF-8")

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_dir, "data")
method_catalog <- get_method_catalog()
source_method_map <- get_source_method_map()
method_network <- get_method_network()

# data_cache 和 example_cache 保存在 app session 外部，因此同一 R 进程中的网页连接可以复用结果。
data_cache <- new.env(parent = emptyenv())
example_cache <- new.env(parent = emptyenv())

# preload_all_examples()
# 功能：网页打开前一次性读取数据并运行全部案例，让之后的侧栏和网络图点击直接显示结果。
# catalog 提供 method_id 和 example_id；每个完成的案例以 method_id 为名称放入 example_cache。
preload_all_examples <- function(catalog, data_dir, data_cache, example_cache) {
  message("Loading shared WIDE_* data before opening the webpage...")
  data_bundle <- load_wide_data(data_dir)
  assign("bundle", data_bundle, envir = data_cache)

  total_examples <- nrow(catalog)
  for (index in seq_len(total_examples)) {
    method_id <- catalog$method_id[index]
    method_name <- catalog$method_name[index]
    example_id <- catalog$example_id[index]

    message(sprintf("Precomputing case %02d/%02d: %s", index, total_examples, method_name))
    result <- run_example(example_id, data_bundle)
    assign(method_id, result, envir = example_cache)
  }

  message("All case studies are ready. Opening the webpage...")
  invisible(TRUE)
}

# 这一步在 shinyApp() 启动浏览器之前完成；网页出现后，24 个方法都能直接读取结果。
preload_all_examples(method_catalog, data_dir, data_cache, example_cache)

# build_method_search_index()
# 功能：把目录说明、原始代码映射、案例变量和图名合并成搜索文字，并记录匹配原因。
build_method_search_index <- function(catalog, source_map, example_cache) {
  rows <- lapply(seq_len(nrow(catalog)), function(index) {
    method_id <- catalog$method_id[index]
    case <- get(method_id, envir = example_cache, inherits = FALSE)
    notes <- get_method_notes(method_id)
    mapped <- source_map[source_map$method_id == method_id, , drop = FALSE]
    variables <- paste(case$variables$variable, case$variables$meaning, collapse = " ")
    plot_titles <- paste(names(case$plots), collapse = " ")
    source_text <- paste(mapped$source_method, mapped$mapping_note, collapse = " ")
    data.frame(
      method_id = method_id,
      method_name = catalog$method_name[index],
      category = catalog$category[index],
      variables = variables,
      plot_titles = plot_titles,
      source_text = source_text,
      searchable = tolower(paste(catalog$method_name[index], catalog$category[index], notes, variables, plot_titles, source_text, collapse = " ")),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# 预先建立搜索索引；用户输入关键词时只筛选文字，不会重新运行统计案例。
method_search_index <- build_method_search_index(method_catalog, source_method_map, example_cache)

# method_sidebar()
# 功能：把方法目录转换为可展开的一级分类和可直接点击的二级方法列表。
# 每个 actionLink 都是一个可点击的方法名称；例如点击 VAR 后，网页会记录 input$method_link_var 发生了变化，
# server 看到这个变化后会运行 open_method("var")，这就是这里说的“监听点击”。
method_sidebar <- function(catalog) {
  # unique() 保留分类第一次出现的顺序，左侧目录因此与 catalog.R 的定义顺序一致。
  categories <- unique(catalog$category)
  # lapply() 对每个一级分类重复生成一个 details 折叠区域，例如分别生成 Time Series 和 Regression Models。
  # tagList() 把这些分开的网页组件装成一个整体，方便一次放进 sidebar。
  tagList(lapply(seq_along(categories), function(index) {
    category <- categories[index]
    rows <- catalog[catalog$category == category, , drop = FALSE]
    # details/summary 是浏览器原生折叠组件；第一个分类默认展开。
    tags$details(
      class = "method-group",
      open = if (index == 1) NA else NULL,
      tags$summary(category),
      tags$div(
        class = "method-list",
        # actionLink() 创建看起来像文字链接的按钮；用户点击后，server 可以知道具体点了哪个方法。
        # 里面这层 lapply() 为当前分类的每一行生成一个链接，例如 Time Series 下依次生成 ARIMA、SARIMA、GARCH 等。
        lapply(seq_len(nrow(rows)), function(row_index) {
          actionLink(
            inputId = paste0("method_link_", rows$method_id[row_index]),
            label = rows$method_name[row_index],
            class = "method-link",
            `data-method` = rows$method_id[row_index]
          )
        })
      )
    )
  }))
}

# ============================================================
# UI: 定义浏览器中显示的页面结构
# ============================================================
ui <- bslib::page_sidebar(
  # page_sidebar() 将固定左侧目录和右侧主内容组合成一个响应式页面。
  title = "Data Science Encyclopedia",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#335C67"),
  fillable = TRUE,
  sidebar = bslib::sidebar(
    width = 330,
    title = "Method Index",
    textInput("method_search", NULL, placeholder = "Search VAR, forecast, USDCAD..."),
    uiOutput("search_results"),
    tags$div(class = "sidebar-index", method_sidebar(method_catalog)),
    tags$hr(),
    actionButton("show_network_help", "Navigator note", class = "btn-outline-secondary")
  ),
  # tags$*() 用 R 生成网页 HTML 元素：tags$div() 是一个内容区域，tags$hr() 是分隔线。
  # 这里 tags$div() 给方法目录加上 sidebar-index 样式，tags$hr() 将目录和帮助按钮分开。
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # 收到 server 发出的 active-method 消息后，高亮左侧当前方法。
    # active 样式就是 styles.css 中当前链接的背景色和左边框，让用户看出现在打开的是哪个方法。
    tags$script(HTML("
      Shiny.addCustomMessageHandler('active-method', function(methodId) {
        document.querySelectorAll('.method-link').forEach(function(link) {
          link.classList.toggle('active', link.dataset.method === methodId);
        });
      });
    "))
  ),
  div(
    class = "app-shell",
    # 固定状态栏始终显示当前运行步骤、百分比和进度条。
    uiOutput("runtime_status"),
    bslib::navset_tab(
      id = "main_tabs",
      bslib::nav_panel(
        title = "Method Navigator",
        value = "navigator",
        bslib::card(
          bslib::card_header("Data question -> method network"),
          visNetwork::visNetworkOutput("method_network", height = "650px")
        )
      ),
      bslib::nav_panel(
        title = "Method Detail",
        value = "detail",
        uiOutput("method_header"),
        uiOutput("method_notes"),
        bslib::card(
          bslib::card_header("Case Study Roadmap"),
          DT::DTOutput("step_table")
        ),
        uiOutput("plot_gallery"),
        bslib::card(
          bslib::card_header("Statistical Tests and Interpretation"),
          DT::DTOutput("test_table")
        ),
        uiOutput("table_gallery"),
        bslib::layout_columns(
          col_widths = c(5, 7),
          bslib::card(bslib::card_header("Variable Meanings"), DT::DTOutput("variable_table")),
          bslib::card(bslib::card_header("Model Summary"), verbatimTextOutput("model_summary"))
        ),
        bslib::card(bslib::card_header("Reusable R Code"), uiOutput("code_snippet")),
        bslib::card(bslib::card_header("Final Interpretation"), uiOutput("case_conclusion")),
        bslib::card(bslib::card_header("Mapped from DataScience.R"), DT::DTOutput("method_source_map"))
      ),
      bslib::nav_panel(
        title = "Catalog",
        value = "catalog",
        bslib::card(bslib::card_header("Complete method coverage"), DT::DTOutput("catalog_table"))
      ),
      bslib::nav_panel(
        title = "Source Map",
        value = "source",
        bslib::card(bslib::card_header("DataScience.R method-to-tab mapping"), DT::DTOutput("source_map_table"))
      )
    )
  )
)

# ============================================================
# Server: 定义点击方法、运行案例和展示结果后的行为
# ============================================================
server <- function(input, output, session) {
  # reactiveVal() 是“改了以后会通知相关代码重新运行”的单个值。
  # 例如 selected_method 从 "linear_regression" 改成 "var" 后，标题、案例、变量表和图都会跟着重新生成。
  selected_method <- reactiveVal("linear_regression")
  # rerun_counter 平时只保存一个数字；每点击一次 Re-run case 就加 1，用这个变化通知案例重新计算。
  rerun_counter <- reactiveVal(0)
  # reactiveValues() 与 reactiveVal() 类似，但可以一起保存多个有关联的值，这里统一保存状态栏内容。
  runtime <- reactiveValues(task = "Ready", detail = "All case studies are precomputed. Choose a method.", percent = 100, busy = FALSE)

  # set_runtime()
  # 功能：更新固定状态栏；相关 renderUI() 会在当前操作结束后把新状态发送给浏览器。
  set_runtime <- function(task, detail, percent, busy = TRUE) {
    runtime$task <- task
    runtime$detail <- detail
    runtime$percent <- percent
    runtime$busy <- busy
  }

  # open_method()
  # 功能：统一处理左侧目录和网络图点击，选择方法后自动跳转到Method Detail这个tab。
  # method_id 是用户选择的方法，例如 "var"；函数会修改 selected_method，并让浏览器打开详情页。
  open_method <- function(method_id) {
    # 不在目录中的 ID 直接忽略，避免网络图或输入异常使网页跳到不存在的方法。
    if (!method_id %in% method_catalog$method_id) return(invisible(NULL))
    selected_method(method_id)
    # session 代表当前这一个浏览器连接；sendCustomMessage() 把 method_id 发给该浏览器，用于高亮左侧链接。
    session$sendCustomMessage("active-method", method_id)
    # nav_select() 主动切换顶部 tab，因此点击左侧或网络图后会自动打开 Method Detail。
    bslib::nav_select("main_tabs", "detail", session = session)
    invisible(method_id)
  }

  # 下面为 24 个方法链接分别安排点击后的动作；例如 method_link_var 被点击时运行 open_method("var")。
  lapply(method_catalog$method_id, function(method_id) {
    # local() 为本轮循环单独保存 current_id，避免 24 个链接最后都错误地指向最后一个方法。
    # observeEvent() 等待指定 input 发生变化；这里就是等待对应的方法链接被点击。
    local({
      current_id <- method_id
      observeEvent(input[[paste0("method_link_", current_id)]], {
        open_method(current_id)
      })
    })
  })

  # filtered_search_results()
  # 功能：按关键词筛选方法，并说明关键词匹配到了方法名、变量、图片标题还是原始代码。
  filtered_search_results <- reactive({
    query <- trimws(tolower(input$method_search %||% ""))
    if (!nzchar(query)) return(method_search_index[0, , drop = FALSE])
    matches <- method_search_index[grepl(query, method_search_index$searchable, fixed = TRUE), , drop = FALSE]
    if (nrow(matches) == 0) return(matches)
    # 精确方法名最优先；之后依次考虑方法名包含、变量、图片标题、原始代码和普通说明。
    matches$score <- vapply(seq_len(nrow(matches)), function(index) {
      row <- matches[index, ]
      if (tolower(row$method_name) == query || tolower(row$method_id) == query) return(100)
      if (grepl(query, tolower(row$method_name), fixed = TRUE)) return(80)
      if (grepl(query, tolower(row$variables), fixed = TRUE)) return(60)
      if (grepl(query, tolower(row$plot_titles), fixed = TRUE)) return(50)
      if (grepl(query, tolower(row$source_text), fixed = TRUE)) return(40)
      10
    }, numeric(1))
    matches$reason <- vapply(seq_len(nrow(matches)), function(index) {
      row <- matches[index, ]
      if (grepl(query, tolower(row$method_name), fixed = TRUE)) return("Matched method name")
      if (grepl(query, tolower(row$variables), fixed = TRUE)) return("Matched variable name or meaning")
      if (grepl(query, tolower(row$plot_titles), fixed = TRUE)) return("Matched plot title")
      if (grepl(query, tolower(row$source_text), fixed = TRUE)) return("Matched original reference method")
      "Matched category or method explanation"
    }, character(1))
    head(matches[order(-matches$score, matches$method_name), , drop = FALSE], 8)
  })

  output$search_results <- renderUI({
    rows <- filtered_search_results()
    query <- trimws(input$method_search %||% "")
    if (!nzchar(query)) return(NULL)
    if (nrow(rows) == 0) return(tags$div(class = "search-empty", "No matching methods."))
    tags$div(
      class = "search-results",
      lapply(seq_len(nrow(rows)), function(index) {
        actionLink(
          inputId = paste0("search_result_", rows$method_id[index]),
          label = tagList(
            tags$strong(rows$method_name[index]),
            tags$span(rows$category[index]),
            tags$small(rows$reason[index])
          ),
          class = "search-result-link"
        )
      })
    )
  })

  # 每条搜索结果复用 open_method()，因此点击后会高亮目录并自动打开 Method Detail。
  lapply(method_catalog$method_id, function(method_id) {
    local({
      current_id <- method_id
      observeEvent(input[[paste0("search_result_", current_id)]], {
        open_method(current_id)
      })
    })
  })

  selected_catalog_row <- reactive({
    # 当前方法改变时，从目录中重新找到对应的名称、分类和 example_id。
    # reactive() 保存一段需要跟着输入变化重新计算的代码；例如 selected_method() 变成 "var"，
    # selected_catalog_row() 就会重新返回 VAR 对应的目录行，而不需要手动更新所有页面组件。
    method_catalog[method_catalog$method_id == selected_method(), , drop = FALSE][1, ]
  })

  # selected_case()
  # 功能：首次打开时加载数据并计算案例，之后直接读取缓存；点击重新运行会覆盖当前缓存。
  selected_case <- reactive({
    current_method <- selected_method()
    rerun_counter()

    if (exists(current_method, envir = example_cache, inherits = FALSE)) {
      # exists()/get() 从内存缓存读取已运行案例，第二次打开时不重复计算。
      set_runtime("Ready", paste("Loaded cached case:", selected_catalog_row()$method_name), 100, FALSE)
      return(get(current_method, envir = example_cache, inherits = FALSE))
    }
    # 数据只在第一次需要案例时读取：data_cache 没有 bundle 才调用 load_wide_data()。
    # 后续方法直接 get("bundle") 复用相同数据，因此点击新方法时不用重新读取十个 WIDE_* 文件。
    if (!exists("bundle", envir = data_cache, inherits = FALSE)) {
      set_runtime("Loading data", "Reading the ten WIDE_* databases from the project data folder.", 15)
      assign("bundle", load_wide_data(data_dir), envir = data_cache)
    }

    # Progress$new() 创建 Shiny 自带的运行提示；重新运行案例时会立即显示当前步骤。
    progress <- shiny::Progress$new(session, min = 0, max = 100)
    on.exit(progress$close(), add = TRUE)
    progress$set(value = 35, message = paste("Running", selected_catalog_row()$method_name), detail = "Preparing data and model.")

    set_runtime("Running case study", paste("Preparing", selected_catalog_row()$method_name, "data and model."), 35)
    result <- run_example(selected_catalog_row()$example_id, get("bundle", envir = data_cache))
    progress$set(value = 85, detail = "Preparing plots, tables, tests, and teaching explanations.")
    set_runtime("Building presentation", "Preparing plots, tables, tests, and teaching explanations.", 85)
    # assign() 使用 method_id 作为缓存名称，保存该方法的完整案例结果。
    assign(current_method, result, envir = example_cache)
    set_runtime("Ready", paste("Completed:", selected_catalog_row()$method_name), 100, FALSE)
    result
  })
  # observeEvent() 等待 Re-run case 按钮被点击。比如 VAR 已有缓存时，点击会先删除 "var" 缓存，
  # 再把 rerun_counter 从 0 改成 1；selected_case() 使用了 rerun_counter()，看到数字变化后就会重新运行 VAR。
  observeEvent(input$rerun_case, {
    current_method <- selected_method()
    if (exists(current_method, envir = example_cache, inherits = FALSE)) {
      # 这里只删除内存中的一个明确案例缓存，不会删除磁盘文件。
      rm(list = current_method, envir = example_cache)
    }
    rerun_counter(rerun_counter() + 1)
  }, ignoreInit = TRUE)

  output$runtime_status <- renderUI({
    # renderUI() 根据运行状态动态生成文字、百分比和进度条宽度。
    tags$div(
      class = paste("runtime-status", if (runtime$busy) "busy" else "ready"),
      tags$div(
        class = "runtime-copy",
        tags$strong(runtime$task),
        tags$span(runtime$detail)
      ),
      tags$div(class = "runtime-percent", paste0(runtime$percent, "%")),
      tags$div(
        class = "runtime-track",
        tags$div(class = "runtime-fill", style = paste0("width:", runtime$percent, "%"))
      )
    )
  })

  observeEvent(input$show_network_help, {
    showModal(modalDialog(
      title = "How to use Method Navigator",
      "Click a green method node to open its complete case study. Question, data-type, and goal nodes explain the decision path.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$method_network_node, {
    # 网络图传回节点 ID 后，从 nodes 表取得它对应的 method_id；非方法节点不会跳转。
    node_row <- method_network$nodes[method_network$nodes$id == input$method_network_node, , drop = FALSE]
    if (nrow(node_row) == 1 && !is.na(node_row$method_id)) {
      open_method(node_row$method_id)
    }
  })
  # 用户点击网络图中的 VAR 节点时，下面 visEvents() 中的 JavaScript 会把节点 ID "var"
  # 写入 input$method_network_node；这个输出函数再根据 nodes/edges 生成网页中的网络图。
  output$method_network <- visNetwork::renderVisNetwork({
    nodes <- method_network$nodes
    nodes$title <- ifelse(is.na(nodes$method_id), nodes$label, paste0(nodes$label, "<br>Click to open the case study."))
    nodes$shape <- ifelse(nodes$group == "method", "box", "ellipse")
    nodes$color.background <- c(question = "#F4A261", data_type = "#A8DADC", goal = "#E9C46A", method = "#B7E4C7")[nodes$group]

    # visNetwork(nodes, edges) 用节点表和连接表建立图；width = "100%" 让图使用完整可用宽度。
    # |> 将建立好的网络图继续传给后面的设置函数，不需要每一步重新保存变量。
    visNetwork::visNetwork(nodes, method_network$edges, width = "100%") |>
      # highlightNearest = TRUE 会高亮点击节点附近的连接；nodesIdSelection = FALSE 隐藏额外下拉选择框。
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) |>
      # selectNode 在节点被点击时，把 properties.nodes[0]（例如 "var"）发送为 input$method_network_node。
      visNetwork::visEvents(selectNode = "function(properties) { Shiny.setInputValue('method_network_node', properties.nodes[0], {priority: 'event'}); }") |>
      # randomSeed 固定初始布局，避免每次打开节点位置完全不同。
      visNetwork::visLayout(randomSeed = 42) |>
      # stabilization = TRUE 先让节点位置稳定下来，再显示给用户。
      visNetwork::visPhysics(stabilization = TRUE)
  })

  output$method_header <- renderUI({
    # selected_case() 会在首次打开时运行案例；缓存存在时则直接返回已有结果。
    row <- selected_catalog_row()
    example <- selected_case()
    tags$div(
      class = "method-header",
      tags$div(
        tags$div(class = "method-title", row$method_name),
        tags$div(class = "method-subtitle", paste(row$category, "-", example$title))
      ),
      actionButton("rerun_case", "Re-run case", class = "btn-outline-primary"),
      tags$p(example$background),
      tags$div(class = "question-strip", tags$strong("Research question: "), example$question),
      tags$div(class = "objective-strip", tags$strong("Learning objective: "), example$objective)
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

  output$step_table <- DT::renderDT({
    # dom = "t" 只显示步骤表本身，不增加搜索框和分页工具。
    DT::datatable(selected_case()$steps, rownames = FALSE, options = list(dom = "t", pageLength = 10))
  })

  # plot_gallery 为当前案例的每张图创建独立输出；标题下方提供教学解释。
  output$plot_gallery <- renderUI({
    current_case <- selected_case()
    plots <- current_case$plots
    plot_notes <- current_case$plot_notes
    tagList(
      tags$h3(class = "section-title", "Visual Analysis"),
      tags$div(
        class = "plot-gallery",
        lapply(seq_along(plots), function(index) {
          plot_id <- paste0("case_plot_", index)
          plot_object <- plots[[index]]
          local({
            current_id <- plot_id
            current_plot <- plot_object
            output[[current_id]] <- renderPlot(current_plot, res = 110)
          })
          # 每张图放进独立 card，并在图下显示案例创建时生成并校验过的专用说明。
          bslib::card(
            bslib::card_header(names(plots)[index]),
            plotOutput(plot_id, height = "360px"),
              tags$p(class = "plot-explanation", plot_notes[[names(plots)[index]]])
          )
        })
      )
    )
  })

  output$test_table <- DT::renderDT({
    # scrollX = TRUE 允许检验解释较长时横向滚动，避免挤压页面。
    DT::datatable(selected_case()$tests, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  # table_gallery 按案例实际返回的表格数量，逐一生成反馈结果表格。
  output$table_gallery <- renderUI({
    tables <- selected_case()$tables
    tagList(
      tags$h3(class = "section-title", "Detailed Results"),
      tags$div(
        class = "table-gallery",
        lapply(seq_along(tables), function(index) {
          table_id <- paste0("case_table_", index)
          table_object <- tables[[index]]
          local({
            current_id <- table_id
            current_table <- table_object
            output[[current_id]] <- DT::renderDT({
              DT::datatable(current_table, rownames = TRUE, options = list(pageLength = 8, scrollX = TRUE))
            })
          })
          bslib::card(bslib::card_header(names(tables)[index]), DT::DTOutput(table_id))
        })
      )
    )
  })

  output$variable_table <- DT::renderDT({
    DT::datatable(selected_case()$variables, rownames = FALSE, options = list(pageLength = 10, dom = "tip"))
  })

  output$model_summary <- renderText({
    # paste(..., collapse = "\n") 将模型摘要的多行 character 合并成网页中的多行文本。
    summary_text <- selected_case()$model_summary
    paste(summary_text, collapse = "\n")
  })

  output$code_snippet <- renderUI({
    tags$div(class = "code-box", tags$pre(tags$code(selected_case()$code)))
  })

  output$case_conclusion <- renderUI({
    tags$div(class = "conclusion-box", selected_case()$conclusion)
  })

  output$catalog_table <- DT::renderDT({
    coverage <- method_catalog[, c("category", "method_name", "method_id")]
    coverage$case_status <- "Complete runnable case"
    DT::datatable(coverage, rownames = FALSE, filter = "top", options = list(pageLength = 24, scrollX = TRUE))
  })

  output$method_source_map <- DT::renderDT({
    mapped <- source_method_map[source_method_map$method_id == selected_method(), , drop = FALSE]
    DT::datatable(mapped[, c("source_lines", "source_section", "source_method", "mapping_note")], rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$source_map_table <- DT::renderDT({
    DT::datatable(source_method_map, rownames = FALSE, filter = "top", options = list(pageLength = 20, scrollX = TRUE))
  })

  # onFlushed() 等 server 第一次把网页内容发送完成后再执行里面的函数。
  # once = TRUE 表示只执行一次；这样首次打开时只高亮默认方法，不会在以后每次刷新输出时重复发送。
  session$onFlushed(function() {
    # onFlushed(..., once = TRUE) 等首屏发送完成后，高亮默认的 Linear Regression 链接。
    # isolate() 允许这个普通回调读取 reactiveVal，而不会要求它处在 reactive 计算环境中。
    session$sendCustomMessage("active-method", isolate(selected_method()))
  }, once = TRUE)
}

# note_box()
# 功能：生成方法说明卡片，title 是小标题，text 是说明内容。
note_box <- function(title, text) {
  tags$div(class = "note-box", tags$h4(title), tags$p(text))
}

shinyApp(ui, server)
