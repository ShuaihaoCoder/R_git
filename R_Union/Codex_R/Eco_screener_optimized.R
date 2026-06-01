# ============================================================
# ECO Screener 优化版
# 保留脚本式运行方式：source 以后直接有表、有图、有 HTML
# ============================================================

# 固定用 C 盘这份 R_Union，不再碰 G 盘 Google Drive。
project_dir <- "C:/Users/PC/Desktop/R_git/R_Union"

# 输出都放进 Codex_R/output，原始 R_Union 里的 R 和 Rmd 不动。
output_dir <- file.path(project_dir, "Codex_R", "output")

# output 不存在就建一个，后面 CSV、PNG、HTML 都会写到这里。
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 主数据就是 C 盘的 WIDE_ALLX。
wide_eco_file <- file.path(project_dir, "WIDE_ALLX")

# 先把宽表读进来，后面所有筛选都基于这个对象。
wide_eco_data <- readRDS(wide_eco_file)

# 日期列统一转成 Date，避免 VS Code 里字符日期比较出问题。
wide_eco_data$date <- as.Date(wide_eco_data$date)

# 数据库里真实存在的日期，后面选日期会用。
available_dates <- sort(unique(wide_eco_data$date))

# 你可以在 source 前写 date <- "2024-10-10"，这里会优先用你给的日期。
if (exists("date", envir = .GlobalEnv, inherits = FALSE) && !is.function(get("date", envir = .GlobalEnv))) {
  selected_date <- as.Date(get("date", envir = .GlobalEnv))
} else {
  selected_date <- max(available_dates, na.rm = TRUE)
}

# 如果当天没数据，就往前找最近一个有数据的日期，这样不会因为周末/假期断掉。
if (!selected_date %in% available_dates) {
  earlier_dates <- available_dates[available_dates <= selected_date]
  if (length(earlier_dates) == 0) stop("这个日期之前没有数据：", selected_date)
  selected_date <- max(earlier_dates)
}

# 保留原来习惯用的 date 对象，Environment 里一眼能看到这次跑的是哪天。
date <- selected_date

# 保留原来 row 的概念，后面取当前日期这一行会用。
selected_row <- which(wide_eco_data$date == date)[1]

# 控制台先告诉你这次实际用了哪一天。
print(paste("Using row:", selected_row, "date:", date))

# DM 国家列表沿用你原来的范围，只是统一了大小写。
developed_markets <- toupper(c(
  "United States", "Britain", "Japan", "Norway", "Canada", "Australia",
  "Germany", "EUROZONE", "Singapore", "New Zealand", "Switzerland",
  "Hong Kong"
))

# 正则里会反复用国家列表，提前拼好。
country_pattern <- paste(developed_markets, collapse = "|")

# 指标统一放在这里管，后面不用一段一段手写 GDP/CPI/PMI。
macro_indicator_specs <- list(
  "Budget Bal %GDP" = list(pattern = "(?=.*Budget)(?=.*GDP)"),
  "CA Bal %GDP" = list(pattern = "(?=.*Current Account Balance)(?=.*GDP)"),
  "CPI" = list(pattern = "(?=.*CPI)(?=.*YoY)", manual = "EUROZONE Euro Area MUICP All Items YoY NSA"),
  "Real GDP" = list(pattern = "(?=.*GDP)(?=.*YoY)", exclude = "Nominal|Current USD|World Bank", manual = "EUROZONE Euro Area Gross Domestic Product Chained Prices YoY"),
  "IP" = list(pattern = "(?=.*Industrial Production)(?=.*YoY)"),
  "PMI" = list(pattern = "(?=.*PMI)", prefer = "(?=.*Manufacturing)(?=.*PMI)"),
  "PPI" = list(pattern = "(?=.*(PPI|Producer))", manual = c("CANADA STCA Industrial Product Price YoY NSA", "JAPAN Input-Output Price Index of Manufacturing Industry Output YoY (Base=2005)")),
  "FX Reserve" = list(pattern = "(?=.*Foreign)"),
  "Retail Sales" = list(pattern = "(?=.*Retail)(?=.*YoY)", manual = "UNITED STATES Adjusted Retail & Food Services Sales Total Yearly % Change SA"),
  "Unemployment" = list(pattern = "(?=.*Unemploy)")
)

# 从列名里抓国家名，抓不到就返回 NA。
get_country_from_col <- function(column_name) {
  hit <- regexpr(country_pattern, toupper(column_name), perl = TRUE)
  ifelse(hit > 0, regmatches(toupper(column_name), hit), NA_character_)
}

# 每个国家挑一个最合适的列：先按国家拆开，再按 prefer/manual/列名长度挑。
pick_indicator_columns <- function(all_col_names, spec) {
  matched_cols <- grep(paste0("(", country_pattern, ")", spec$pattern), all_col_names, value = TRUE, ignore.case = TRUE, perl = TRUE)
  manual_cols <- intersect(spec$manual %||% character(0), all_col_names)
  matched_cols <- unique(c(matched_cols, manual_cols))
  if (!is.null(spec$exclude)) matched_cols <- matched_cols[!grepl(spec$exclude, matched_cols, ignore.case = TRUE, perl = TRUE)]
  chosen_cols <- character(0)
  for (country in developed_markets) {
    country_cols <- matched_cols[grepl(country, toupper(matched_cols), fixed = TRUE)]
    if (length(country_cols) == 0) next
    if (!is.null(spec$prefer)) {
      preferred_cols <- grep(spec$prefer, country_cols, value = TRUE, ignore.case = TRUE, perl = TRUE)
      if (length(preferred_cols) > 0) country_cols <- preferred_cols
    }
    chosen_cols <- c(chosen_cols, country_cols[order(nchar(country_cols), country_cols)][1])
  }
  chosen_cols
}

# `%||%` 只是让配置里没写 manual 时返回空向量，代码看起来清爽一点。
`%||%` <- function(x, y) if (is.null(x)) y else x

# 当前日期没有值时，往前拿最近一个非 NA，效果类似原来 na.locf 后再取 row。
get_last_value <- function(values, row_id) {
  history_values <- values[seq_len(row_id)]
  history_values <- history_values[!is.na(history_values)]
  if (length(history_values) == 0) return(NA_real_)
  suppressWarnings(as.numeric(tail(history_values, 1)))
}

# 0-1 缩放用在热力图和雷达图，整列一样时放在 0.5。
scale_to_01 <- function(values) {
  if (all(is.na(values))) return(rep(NA_real_, length(values)))
  value_min <- min(values, na.rm = TRUE)
  value_max <- max(values, na.rm = TRUE)
  if (isTRUE(all.equal(value_min, value_max))) return(rep(0.5, length(values)))
  (values - value_min) / (value_max - value_min)
}

# 雷达图不能缺角；这里用每个指标的中位水平补 NA，表格本身仍保留 NA。
fill_missing_for_plot <- function(score_matrix) {
  filled_matrix <- score_matrix
  for (col_id in seq_len(ncol(filled_matrix))) {
    col_values <- filled_matrix[, col_id]
    fill_value <- median(col_values, na.rm = TRUE)
    if (is.na(fill_value)) fill_value <- 0.5
    col_values[is.na(col_values)] <- fill_value
    filled_matrix[, col_id] <- col_values
  }
  filled_matrix
}

# 这个给 source_combine_all.R 兼容用，也方便你自己单独画 fmsb radar。
make_radar_df <- function(x) {
  radar_df <- rbind(rep(1, length(x)), rep(0, length(x)), x)
  radar_df <- as.data.frame(radar_df)
  colnames(radar_df) <- names(macro_indicator_specs)
  radar_df
}

# 把所有指标匹配到的列集中生成出来，变量名比原来的 DMGDP/DMCPI 更直观。
indicator_column_map <- lapply(macro_indicator_specs, function(spec) {
  pick_indicator_columns(names(wide_eco_data), spec)
})

# 建一个国家 x 指标的空表，后面循环填数据。
macro_value_matrix <- matrix(
  NA_real_,
  nrow = length(developed_markets),
  ncol = length(macro_indicator_specs),
  dimnames = list(developed_markets, names(macro_indicator_specs))
)

# 按指标和国家把当前日期最近值填进去。
for (indicator_name in names(indicator_column_map)) {
  for (column_name in indicator_column_map[[indicator_name]]) {
    country_name <- get_country_from_col(column_name)
    macro_value_matrix[country_name, indicator_name] <- get_last_value(wide_eco_data[[column_name]], selected_row)
  }
}

# result 保留下来，和你原来脚本的查看习惯一致。
result <- macro_value_matrix

# df 是原始数值表，Country 放第一列，看起来更像最终表。
df <- as.data.frame(macro_value_matrix, check.names = FALSE)
df$Country <- rownames(df)
df <- df[, c("Country", names(macro_indicator_specs))]
 
# 按每个指标横向比较做一版缩放，适合 heatmap / radar 展示国家之间强弱。
metric_scaled_matrix <- apply(macro_value_matrix, 2, scale_to_01)
rownames(metric_scaled_matrix) <- rownames(macro_value_matrix)

# radar 用补完缺失值后的缩放矩阵，不然有些国家会缺角，看起来很散。
radar_scaled_matrix <- fill_missing_for_plot(metric_scaled_matrix)

# result_scaled 沿用原来的名字，source_combine_all.R 里也能继续拿来画。
result_scaled <- radar_scaled_matrix

# Total_Score 用补完后的缩放数据算，避免缺数据国家因为少几项而形状/分数不均衡。
total_score <- rowMeans(radar_scaled_matrix, na.rm = TRUE)

# df_scaled 是最终 scoreboard 表，直接给你看和导出。
df_scaled <- df
df_scaled$Total_Score <- total_score[df_scaled$Country]
df_scaled <- df_scaled[order(df_scaled$Total_Score, decreasing = TRUE), ]

# macro_score_table 名字更直白，后面 dashboard 和 HTML 都用它。
macro_score_table <- df_scaled

# 保留这些布局对象，和 source_combine_all.R 的写法更接近。
n <- nrow(result_scaled)
ncol <- 4
nrow <- ceiling(n / ncol)

# 文件名统一带日期，方便你比较不同日期的输出。
date_tag <- format(date, "%Y%m%d")
csv_file <- file.path(output_dir, paste0("eco_scoreboard_", date_tag, ".csv"))
html_file <- file.path(output_dir, paste0("eco_report_", date_tag, ".html"))
heatmap_file <- file.path(output_dir, paste0("eco_heatmap_", date_tag, ".png"))
bar_file <- file.path(output_dir, paste0("eco_bars_", date_tag, ".png"))
radar_file <- file.path(output_dir, paste0("eco_radar_", date_tag, ".png"))

# CSV 先写出去，Excel 里也能直接打开。
write.csv(macro_score_table, csv_file, row.names = FALSE, fileEncoding = "UTF-8")

# ============================================================
# 图 1：Scoreboard Heatmap
# ============================================================

# heatmap 用缩放后的值上色，用原始值写数字。
heatmap_values <- as.matrix(macro_score_table[, c(names(macro_indicator_specs), "Total_Score")])
heatmap_scaled <- apply(heatmap_values, 2, scale_to_01)
heatmap_scaled <- fill_missing_for_plot(heatmap_scaled)
heatmap_scaled <- heatmap_scaled[nrow(heatmap_scaled):1, , drop = FALSE]
heatmap_labels <- round(heatmap_values[nrow(heatmap_values):1, , drop = FALSE], 2)
heatmap_palette <- colorRampPalette(c("#19324A", "#86B6D9", "#F7F4EA", "#F0A35E", "#B8324A"))(100)

# 存一张 PNG，HTML 和 Shiny 都可以直接用。
png(heatmap_file, width = 1900, height = 980, res = 150)
par(mar = c(8, 12, 4, 2))
image(seq_len(ncol(heatmap_scaled)), seq_len(nrow(heatmap_scaled)), t(heatmap_scaled), col = heatmap_palette, axes = FALSE, xlab = "", ylab = "", main = paste("ECO Macro Scoreboard -", date))
axis(1, at = seq_len(ncol(heatmap_scaled)), labels = colnames(heatmap_scaled), las = 2, cex.axis = 0.82)
axis(2, at = seq_len(nrow(heatmap_scaled)), labels = rev(macro_score_table$Country), las = 2, cex.axis = 0.82)
for (i in seq_len(nrow(heatmap_scaled))) for (j in seq_len(ncol(heatmap_scaled))) text(j, i, labels = heatmap_labels[i, j], cex = 0.68, col = "#17212B")
box()
dev.off()

# ============================================================
# 图 2：指标柱状图
# ============================================================

# 这张图看每个指标下各国家的横向对比。
png(bar_file, width = 1900, height = 1200, res = 150)
par(mfrow = c(3, 4), mar = c(7, 4, 3, 1))
for (indicator_name in names(macro_indicator_specs)) {
  bar_values <- macro_score_table[[indicator_name]]
  names(bar_values) <- macro_score_table$Country
  barplot(bar_values, las = 2, col = "#2F80ED", border = NA, main = indicator_name, cex.names = 0.65, ylab = "Value")
  grid(nx = NA, ny = NULL, col = "#E2E8F0")
}
dev.off()

# ============================================================
# 图 3：Radar，参考 source_combine_all.R 的多面板风格
# ============================================================

# 这个小函数画出来更接近 fmsb::radarchart，但不强制安装 fmsb。
draw_one_radar <- function(radar_values, main_col, country_name, axis_labels, inner_radius = 0.22) {
  radar_angles <- seq(pi / 2, pi / 2 - 2 * pi, length.out = length(axis_labels) + 1)
  radar_radius <- inner_radius + radar_values * (1 - inner_radius)
  radar_radius <- c(radar_radius, radar_radius[1])
  
  plot(0, 0, type = "n", xlim = c(-1.34, 1.34), ylim = c(-1.34, 1.34), axes = FALSE, xlab = "", ylab = "")
  
  for (ring_value in seq(inner_radius, 1, length.out = 5)) {
    lines(cos(radar_angles) * ring_value, sin(radar_angles) * ring_value, col = "grey85", lty = 1, lwd = 0.6)
  }
  
  lines(cos(radar_angles) * inner_radius, sin(radar_angles) * inner_radius, col = "grey70", lty = 1, lwd = 1.1)
  segments(cos(radar_angles[-length(radar_angles)]) * inner_radius, sin(radar_angles[-length(radar_angles)]) * inner_radius, cos(radar_angles[-length(radar_angles)]), sin(radar_angles[-length(radar_angles)]), col = "grey85", lwd = 0.6)
  text(cos(radar_angles[-length(radar_angles)]) * 1.16, sin(radar_angles[-length(radar_angles)]) * 1.16, axis_labels, cex = 0.74, col = "#334E68")
  
  polygon(cos(radar_angles) * radar_radius, sin(radar_angles) * radar_radius, border = main_col, col = adjustcolor(main_col, 0.5), lwd = 1.5)
  points(cos(radar_angles[-length(radar_angles)]) * radar_radius[-length(radar_radius)], sin(radar_angles[-length(radar_angles)]) * radar_radius[-length(radar_radius)], pch = 16, col = main_col, cex = 0.55)
  title(country_name, cex.main = 1.3, font.main = 2)
}

# 尽量用你原来喜欢的 viridis(option = "C")；没装 viridis 就用 base R 近似色。
if (requireNamespace("viridis", quietly = TRUE)) {
  pal <- viridis::viridis(n, option = "C", begin = 0.1, end = 0.9)
} else {
  pal <- hcl.colors(n, palette = "Inferno", rev = TRUE)
}

# 每个国家一个 radar，小面板布局保留 source_combine_all.R 的感觉。
png(radar_file, width = 1900, height = 1350, res = 150)
par(mfrow = c(nrow, ncol), mar = c(1, 2, 2, 1))
for (i in 1:n) {
  main_col <- pal[i]
  country_name <- rownames(result)[i]
  vals_scaled <- as.numeric(result_scaled[i, ])
  radar_df <- make_radar_df(vals_scaled)
  draw_one_radar(
    radar_values = as.numeric(radar_df[3, ]),
    main_col = main_col,
    country_name = country_name,
    axis_labels = colnames(radar_df),
    inner_radius = 0.22
  )
}
dev.off()

# ============================================================
# HTML 报告
# ============================================================

# HTML 表格需要简单转义一下，避免特殊符号把页面弄乱。
html_escape <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text
}

# 把结果表拼成 HTML 的 tr/td。
table_rows <- apply(macro_score_table, 1, function(one_row) {
  paste0("<tr>", paste0("<td>", html_escape(as.character(one_row)), "</td>", collapse = ""), "</tr>")
})

# 表头也单独拼出来。
table_header <- paste0("<th>", html_escape(names(macro_score_table)), "</th>", collapse = "")

# 顶部卡片显示最高分和最低分。
top_country <- macro_score_table$Country[which.max(macro_score_table$Total_Score)]
low_country <- macro_score_table$Country[which.min(macro_score_table$Total_Score)]

# radar 里补 NA，这里写一句提醒，表格本身还是原始 NA。
missing_note <- "Radar 图为了让每个国家形状完整，缺失指标用该指标的中位水平补齐；表格里仍然保留原始 NA。"

# 拼一个直接双击能看的 HTML。
html <- paste0(
  "<!doctype html><html><head><meta charset='utf-8'><title>ECO Screener Report</title><style>",
  "body{font-family:Segoe UI,Arial,sans-serif;background:#f6f8fb;color:#1f2933;margin:0}.wrap{max-width:1320px;margin:0 auto;padding:28px}",
  "h1{margin:0 0 6px;font-size:30px}.sub{color:#607080;margin-bottom:18px}.note{background:#fff7e6;border:1px solid #f0c36d;border-radius:8px;padding:10px 12px;margin:12px 0;color:#594214}",
  ".cards{display:grid;grid-template-columns:repeat(4,minmax(160px,1fr));gap:12px;margin:18px 0}.card{background:white;border:1px solid #dce3ec;border-radius:8px;padding:14px;box-shadow:0 8px 22px rgba(31,41,51,.05)}",
  ".label{font-size:12px;color:#607080;text-transform:uppercase}.value{font-size:22px;font-weight:750;margin-top:4px}.panel{background:white;border:1px solid #dce3ec;border-radius:8px;padding:14px;margin:14px 0;box-shadow:0 8px 22px rgba(31,41,51,.04)}",
  "img{max-width:100%;height:auto;border-radius:6px}table{border-collapse:collapse;width:100%;font-size:13px;background:white}th,td{border:1px solid #dce3ec;padding:7px;text-align:right}th:first-child,td:first-child{text-align:left}th{background:#edf2f7;position:sticky;top:0}.scroll{overflow:auto;max-height:620px;border:1px solid #dce3ec;border-radius:8px}",
  "</style></head><body><div class='wrap'><h1>ECO Screener Report</h1>",
  "<div class='sub'>数据来自 C:/Users/PC/Desktop/R_git/R_Union/WIDE_ALLX，日期：", date, "</div>",
  "<div class='cards'><div class='card'><div class='label'>Selected Date</div><div class='value'>", date, "</div></div>",
  "<div class='card'><div class='label'>Top Score</div><div class='value'>", top_country, "</div></div>",
  "<div class='card'><div class='label'>Lowest Score</div><div class='value'>", low_country, "</div></div>",
  "<div class='card'><div class='label'>Countries</div><div class='value'>", nrow(macro_score_table), "</div></div></div>",
  "<div class='note'>", missing_note, "</div>",
  "<h2>Scoreboard Heatmap</h2><div class='panel'><img src='", basename(heatmap_file), "'></div>",
  "<h2>Indicator Bars</h2><div class='panel'><img src='", basename(bar_file), "'></div>",
  "<h2>All Countries Radar</h2><div class='panel'><img src='", basename(radar_file), "'></div>",
  "<h2>Score Table</h2><div class='scroll'><table><thead><tr>", table_header, "</tr></thead><tbody>", paste(table_rows, collapse = ""), "</tbody></table></div>",
  "</div></body></html>"
)

# 写出 HTML。
writeLines(html, html_file, useBytes = TRUE)

# 运行结束后，把关键路径都打印出来。
message("ECO Screener 完成，日期：", date)
message("HTML 报告：", html_file)
message("CSV 表格：", csv_file)
message("热力图：", heatmap_file)
message("柱状图：", bar_file)
message("雷达图：", radar_file)

# 交互式运行时，在 Plots 面板给一个完成提示。
if (interactive()) {
  par(mfrow = c(1, 1))
  plot.new()
  title(main = paste("ECO Screener finished:", date), sub = html_file)
}
