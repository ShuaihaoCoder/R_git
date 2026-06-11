# 这个 reference 展示复杂 Shiny 代码的注释方式：
# 不只解释函数名称，还说明输入从哪里来、变量怎么变化、变化后网页哪里更新。

# lapply() 对每个一级分类重复生成一个 details 折叠区域，例如分别生成
# Time Series 和 Regression Models。tagList() 再把这些区域装成一个整体放进 sidebar。
tagList(lapply(categories, function(category) {
  tags$details(tags$summary(category))
}))

# reactiveVal() 是“改了以后会通知相关代码重新运行”的单个值。
# 例如从 "linear_regression" 改成 "var" 后，使用它的标题、案例、图和表都会跟着更新。
selected_method <- reactiveVal("linear_regression")

# observeEvent() 等待 VAR 链接被点击；点击使 input$method_link_var 变化，
# 然后运行 open_method("var")，把当前方法切换成 VAR。
observeEvent(input$method_link_var, {
  open_method("var")
})

# session 代表当前这一个浏览器连接；sendCustomMessage() 把 "var" 发给该浏览器，
# JavaScript 收到后给左侧 VAR 链接增加 active 样式，让它显示为当前选中项。
session$sendCustomMessage("active-method", "var")

# 数据只在 data_cache 中没有 bundle 时读取一次；后续案例直接 get("bundle") 复用，
# 因此点击其他方法不会重新读取全部 WIDE_* 文件。
if (!exists("bundle", envir = data_cache, inherits = FALSE)) {
  assign("bundle", load_wide_data(data_dir), envir = data_cache)
}
