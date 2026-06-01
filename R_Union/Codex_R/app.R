# ============================================================
# ECO Screener Shiny Dashboard
# 单独的互动版文件：日期用 dateInput，不再塞几千个下拉选项
# ============================================================

# Shiny 没装的话，先给一个直白的提示。
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("请先安装 shiny：install.packages('shiny')")
}

# 图片嵌入页面要用 base64enc，通常会跟 Shiny 一起装。
if (!requireNamespace("base64enc", quietly = TRUE)) {
  stop("请先安装 base64enc：install.packages('base64enc')")
}

# 只读 C 盘这份 R_Union，不再碰 G 盘。
project_dir <- "C:/Users/PC/Desktop/R_git/R_Union"

# WIDE_ALLX 也固定从 C 盘读。
wide_eco_data <- readRDS(file.path(project_dir, "WIDE_ALLX"))

# 日期统一成 Date，给 dateInput 设置范围。
wide_eco_data$date <- as.Date(wide_eco_data$date)

# 这里只拿最早和最晚日期，不再把 9000 多个日期放进 selectInput。
available_dates <- sort(unique(wide_eco_data$date))
min_available_date <- min(available_dates, na.rm = TRUE)
max_available_date <- max(available_dates, na.rm = TRUE)

# 页面布局：日期、按钮、三张图、结果表。
ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      body { background: #f6f8fb; color: #1f2933; font-family: Segoe UI, Arial, sans-serif; }
      .wrap { max-width: 1320px; margin: 0 auto; padding: 20px; }
      .box { background: white; border: 1px solid #dce3ec; border-radius: 8px; padding: 14px; margin: 12px 0; box-shadow: 0 8px 22px rgba(31,41,51,.05); }
      img { max-width: 100%; height: auto; border-radius: 6px; }
      table { font-size: 13px; }
    "))
  ),
  shiny::div(
    class = "wrap",
    shiny::h2("ECO Screener Dashboard"),
    shiny::div(
      class = "box",
      shiny::dateInput(
        inputId = "date",
        label = "选择日期",
        value = max_available_date,
        min = min_available_date,
        max = max_available_date,
        format = "yyyy-mm-dd",
        language = "zh-CN"
      ),
      shiny::actionButton("run", "生成 Dashboard")
    ),
    shiny::div(class = "box", shiny::htmlOutput("summary")),
    shiny::div(class = "box", shiny::uiOutput("heatmap_img")),
    shiny::div(class = "box", shiny::uiOutput("bar_img")),
    shiny::div(class = "box", shiny::uiOutput("radar_img")),
    shiny::div(class = "box", shiny::tableOutput("score_table"))
  )
)

# 后端逻辑：点按钮或打开页面时，source optimized 脚本生成最新结果。
server <- function(input, output, session) {
  report_result <- shiny::eventReactive(input$run, {
    date <<- as.character(input$date)
    source(file.path(project_dir, "Codex_R", "Eco_screener_optimized.R"), local = .GlobalEnv)
    list(
      date = date,
      table = macro_score_table,
      heatmap = heatmap_file,
      bars = bar_file,
      radar = radar_file,
      html = html_file
    )
  }, ignoreInit = FALSE)

  output$summary <- shiny::renderUI({
    res <- report_result()
    shiny::HTML(paste0(
      "<b>日期：</b>", res$date,
      "<br><b>HTML 报告：</b>", res$html,
      "<br><span style='color:#607080'>如果选择的是周末或没有数据的日期，脚本会自动往前找最近一个有效日期。</span>"
    ))
  })

  output$heatmap_img <- shiny::renderUI({
    res <- report_result()
    shiny::tags$img(src = paste0("data:image/png;base64,", base64enc::base64encode(res$heatmap)))
  })

  output$bar_img <- shiny::renderUI({
    res <- report_result()
    shiny::tags$img(src = paste0("data:image/png;base64,", base64enc::base64encode(res$bars)))
  })

  output$radar_img <- shiny::renderUI({
    res <- report_result()
    shiny::tags$img(src = paste0("data:image/png;base64,", base64enc::base64encode(res$radar)))
  })

  output$score_table <- shiny::renderTable({
    report_result()$table
  }, striped = TRUE, bordered = TRUE, digits = 3)
}

# 启动 dashboard。
shiny::shinyApp(ui, server)
