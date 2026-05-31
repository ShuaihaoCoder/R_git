library(data.table)
library(shiny)
library(ggplot2)
library(plotly)
library(bslib) # 用于美化界面

# ==========================================
# 1. 数据预处理 (Data Processing)
# ==========================================
prepare_data <- function(dt) {
  # 提取目标列
  all_cols <- names(dt)
  target_cols <- grep("North America USD", all_cols, value = TRUE)
  needed_cols <- c("date", target_cols)
  
  # 转换成长表
  dt_long <- melt(dt[, ..needed_cols], id.vars = "date", 
                  variable.name = "raw_name", value.name = "oas")
  
  # 解析 Rating 和 Sector
  setDT(dt_long)
  dt_long[, `:=`(
    Rating = fifelse(grepl(" IG ", raw_name), "IG", "HY"),
    Sector = sub("North America USD (IG|HY) (.*) OAS", "\\2", raw_name)
  )]
  dt_long[, Sector := trimws(Sector)]
  
  # 计算统计量 (Z-Score & 变动)
  setorder(dt_long, raw_name, date)
  dt_long[, `:=`(
    z_score = (oas - frollmean(oas, 252, align = "right",na.rm=T)) / frollsd(oas, 252, align = "right",na.rm=T),
    diff_5d = oas - shift(oas, 5),
    diff_20d = oas - shift(oas, 20)
  ), by = .(raw_name)]
  
  return(dt_long)
}

# 假设你的数据叫 cd，先进行处理
setDT(cd)
plot_dt <- prepare_data(cd)

# ==========================================
# 2. Shiny UI 设计
# ==========================================
ui <- page_sidebar(
  title = "US Sector OAS Analysis Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Controls",
    selectInput("rating_filter", "Rating Selection:", 
                choices = c("All", "IG", "HY"), selected = "All"),
    selectizeInput("sector_filter", "Select Sectors:", 
                   choices = NULL, multiple = TRUE), # 动态更新
    dateRangeInput("date_range", "Date Range:",
                   start = Sys.Date() - 365, end = Sys.Date()),
    hr(),
    helpText("Z-Score is based on a rolling 252-day window.")
  ),
  
  # 布局：上方看趋势，下方看相对价值和变动
  layout_column_wrap(
    width = 1,
    card(
      card_header("OAS Time Series Trend"),
      plotlyOutput("ts_plot")
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Relative Value: OAS vs Z-Score (Current)"),
        plotlyOutput("rv_plot")
      ),
      card(
        card_header("Weekly Change (5D bps)"),
        plotlyOutput("change_plot")
      )
    )
  )
)

# ==========================================
# 3. Shiny Server 逻辑
# ==========================================
server <- function(input, output, session) {
  
  # 模拟数据加载 (如果运行环境没用 cd，这里可以用这个逻辑测试)
  # plot_dt <- prepare_data(cd) 
  
  # 初始化选择器
  updateSelectizeInput(session, "sector_filter", 
                       choices = unique(plot_dt$Sector), 
                       selected = unique(plot_dt$Sector)[1:3])
  
  # 反应式过滤数据
  filtered_dt <- reactive({
    res <- plot_dt[date >= input$date_range[1] & date <= input$date_range[2]]
    if (input$rating_filter != "All") {
      res <- res[Rating == input$rating_filter]
    }
    if (length(input$sector_filter) > 0) {
      res <- res[Sector %in% input$sector_filter]
    }
    res
  })
  
  # 图表 1: 时间序列
  output$ts_plot <- renderPlotly({
    p <- ggplot(filtered_dt(), aes(x = date, y = oas, color = raw_name)) +
      geom_line(alpha = 0.8) +
      theme_minimal() +
      labs(y = "OAS (bps)", x = NULL, color = "Series") +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # 图表 2: 相对价值散点图 (最新日期)
  output$rv_plot <- renderPlotly({
    latest_dt <- plot_dt[date == max(date)]
    # 增加象限图示
    
    p <- ggplot(latest_dt, aes(x = oas, y = z_score, color = Rating, text = Sector)) +
      geom_point(size = 4, alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_vline(xintercept = median(latest_dt$oas), linetype = "dotted") +
      theme_minimal() +
      labs(x = "Current OAS (bps)", y = "Z-Score (1Y)")
    ggplotly(p, tooltip = "text")
  })
  
  # 图表 3: 5D 变化柱状图
  output$change_plot <- renderPlotly({
    latest_dt <- plot_dt[date == max(date)]
    p <- ggplot(latest_dt, aes(x = reorder(Sector, diff_5d), y = diff_5d, fill = Rating)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(x = NULL, y = "5-Day Change (bps)")
    ggplotly(p)
  })
}

# 运行
shinyApp(ui, server)
