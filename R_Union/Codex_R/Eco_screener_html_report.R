# ============================================================
# ECO Screener HTML 入口
# 这个文件只负责从 C 盘启动报告，不再自动查找路径，也不会碰 G 盘
# ============================================================

# 现在所有数据都以 C 盘这份 R_Union 为准。
project_dir <- "C:/Users/PC/Desktop/R_git/R_Union"

# 真正负责算数据、画图、写 HTML 的脚本放在 Codex_R 里。
optimized_script <- file.path(project_dir, "Codex_R", "Eco_screener_optimized.R")

# 如果你在 source 前写了 date <- "2023-10-10"，optimized 脚本会自动读取这个日期。
source(optimized_script, local = .GlobalEnv)

# 跑完以后，html_file/csv_file/heatmap_file/bar_file/radar_file 都会留在 Environment 里。
message("Eco Screener HTML report completed.")
message("Date: ", date)
message("HTML: ", html_file)
message("CSV: ", csv_file)
message("Heatmap: ", heatmap_file)
message("Bars: ", bar_file)
message("Radar: ", radar_file)
