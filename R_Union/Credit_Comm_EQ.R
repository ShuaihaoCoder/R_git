library(Rblpapi)
library(PerformanceAnalytics)
library(quantmod)
library(data.table)
library(pacman)
library(stats)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(zoo)
library(scales)
library(tidyverse)
library(reshape2)
library(viridis)
library(YieldCurve)
library(plotly)
library(RQuantLib)
library(patchwork)

setwd("G:/我的云端硬盘/R_Union")
r=readRDS(file="WIDE_RATES")
f=readRDS(file="WIDE_FX") 
v=readRDS(file="WIDE_VOL")
eco=readRDS(file="WIDE_ECO")
c=readRDS(file="WIDE_CFTC")
m=readRDS(file="WIDE_MM")
eq=readRDS(file="WIDE_EQ")
comm=readRDS(file="WIDE_COMM")
cd=readRDS(file="WIDE_CREDIT")
x=readRDS(file="WIDE_ALLX")#economic data

#credit dashboard----
grep("China",names(cd),value=T)
grep("CHINA",names(x),value=T)
grep("United States",colnames(cd),value=T)
grep("America",colnames(cd),value=T)
#USA OAS----
US_keys=c("America","United States")
US_cols <- grep(paste(US_keys, collapse="|"), colnames(cd), value=TRUE)
US_Single=grep("D14",US_cols,value=T)
US_Single_1=US_Single[c(-3,-13)]
US_Sector_OAS=grep("OAS",US_cols,value=T)
US_Sector_OAS_HY=grep("HY",US_Sector_OAS,value=T)
US_Sector_OAS_IG=grep("IG",US_Sector_OAS,value=T)

#USA CDS classfication----
cols <- US_cols

# ---- 1. 只保留 NA America ----
cols <- cols[grepl("^NA America", cols)]

# ---- 2. 解析函数 for attribute ----
parse_col <- function(x){
  
  # Rating
  rating <- ifelse(grepl("\\bHY\\b", x), "HY",
                   ifelse(grepl("\\bIG\\b", x), "IG", NA))
  
  # Tenor (抓数字)
  tenor_num <- regmatches(x, regexpr("[0-9]+", x))
  tenor <- ifelse(length(tenor_num)==0, NA, paste0(tenor_num, "Y"))
  
  # Industry = NA America 和 IG/HY 之间
  tmp <- sub("^NA America\\s+", "", x)
  tmp <- sub("\\s+(IG|HY).*", "", tmp)
  
  # 去掉 Sr
  tmp <- gsub("\\bSr\\b", "", tmp)
  
  # 清理多余 space
  industry <- trimws(gsub("\\s+", " ", tmp))
  
  list(rating=rating, tenor=tenor, industry=industry)
}

parsed <- lapply(cols, parse_col)

ratings   <- sapply(parsed, `[[`, "rating")
tenors    <- sapply(parsed, `[[`, "tenor")
industries<- sapply(parsed, `[[`, "industry")

# ---- 3. 按 Rating + Industry 生成 vectors ----
u_ind <- unique(industries)

for(ind in u_ind){
  for(r in c("HY","IG")){
    vname <- paste(r, ind, sep="_")
    vname <- gsub(" ", "", vname)   # 变量名去空格（不影响列名）
    
    assign(
      vname,
      cols[ratings==r & industries==ind],
      envir = .GlobalEnv
    )
  }
}

# ---- 4. 按 Rating + Tenor 生成 vectors ----
u_tenor <- unique(tenors)

for(t in u_tenor){
  for(r in c("HY","IG")){
    vname <- paste(r, t, sep="_")
    
    assign(
      vname,
      cols[ratings==r & tenors==t],
      envir = .GlobalEnv
    )
  }
}



#build IG/HY CDS dashboard----
HY_FinSub
grep("HY",ls(),value=T)
setDT(cd)
# 只取 sector CDS (1-140)
US_credit_IG_HY_cols <- US_cols[1:140]

# melt
dt_long_IG_HY <- melt(
  cd,
  id.vars = "date",
  measure.vars = US_credit_IG_HY_cols,
  variable.name = "series",
  value.name = "spread"
)
setDT(dt_long_IG_HY)
# rating
dt_long_IG_HY[, rating :=
          fifelse(grepl(" HY ", series), "HY",
                  fifelse(grepl(" IG ", series) | grepl(" IG[0-9]", series), "IG", NA))
]

# tenor (抓数字)

dt_long_IG_HY[, tenor :=
          paste0(stringr::str_extract(series, "[0-9]+"), "Y")
]

# sector

dt_long_IG_HY[, sector := series]

dt_long_IG_HY[, sector := sub("^NA America ", "", sector)]

dt_long_IG_HY[, sector := sub(" (HY|IG).*", "", sector)]

dt_long_IG_HY[, sector := gsub(" Sr", "", sector)]

dt_long_IG_HY[, sector := trimws(sector)]

# 最终 dataset
US_credit_long <- dt_long_IG_HY[, .(
  date,
  sector,
  rating,
  tenor,
  spread
)]

#add SPX and US2y
spx <- eq[, .(
  date,
  SPX = `United States S&P 500 INDEX`
)]

r=readRDS(file="WIDE_RATES")
us2y <- r[, .(
  date,
  US2Y = `UNITED STATES 2Y Govt`
)]
macro <- merge(spx, us2y, by="date", all=TRUE)

US_credit_long_macro <- merge(
  US_credit_long,
  macro,
  by="date",
  all.x=TRUE
)

#plot
setDT(US_credit_long_macro)
# --------------------------
# 去掉 NA tenor 并设置因子
# --------------------------
US_credit_long_macro <- US_credit_long_macro[!is.na(tenor)]
US_credit_long_macro[, rating := factor(rating, levels=c("IG","HY"))]
US_credit_long_macro[, tenor := factor(tenor, levels=c("2Y","5Y","10Y"))]
US_credit_long_macro[, tenor := droplevels(tenor)]

# --------------------------
# 时间范围以 CDS TS 的最早日期为准
# --------------------------
start_dates <- US_credit_long[!is.na(spread), .(start_date = min(date)), by=.(sector, rating, tenor)]
overall_start_date <- min(start_dates$start_date)
start_date <- overall_start_date
end_date   <- max(US_credit_long_macro$date, na.rm=TRUE)

US_credit_long_macro <- US_credit_long_macro[date >= start_date & date <= end_date]
US_credit_long_macro_1=US_credit_long_macro
# 每个 panel 单独计算 CDS 上半部分和 macro 下半部分

panel_range <- US_credit_long_macro[, .(
  ymin_panel = min(spread, na.rm=TRUE),
  ymax_panel = max(spread, na.rm=TRUE)
), by=.(rating, tenor)]

# merge 回 CDS 数据
US_credit_long_macro <- merge(US_credit_long_macro, panel_range, by=c("rating","tenor"), all.x=TRUE)
macro_plot_long <- US_credit_long_macro[!is.na(SPX) & !is.na(US2Y)]
macro_plot_long[, bottom_offset := ymin_panel - 0.3*(ymax_panel - ymin_panel)]
macro_plot_long[, macro_height  := 0.3*(ymax_panel - ymin_panel)]

# Macro 左右轴映射
macro_plot_long[, SPX_plot := bottom_offset + (SPX - min(SPX, na.rm=TRUE)) / (max(SPX, na.rm=TRUE)-min(SPX, na.rm=TRUE)) * macro_height]
macro_plot_long[, US2Y_plot := bottom_offset + (US2Y - min(US2Y, na.rm=TRUE)) / (max(US2Y, na.rm=TRUE)-min(US2Y, na.rm=TRUE)) * macro_height]

# 绘图
# --------------------------
US_CDS_Plot=ggplot() +
  # CDS lines (上半部分)
  geom_rect(data=panel_range, 
            aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill=rating), 
            inherit.aes=FALSE, alpha=0.2) +
  geom_line(data=US_credit_long_macro, aes(x=date, y=spread, color=sector), size=0.8) +
  
  # Macro lines (下半部分)
  geom_line(data=macro_plot_long, aes(x=date, y=SPX_plot, color="SPX", linetype="SPX"), size=0.6) +
  geom_line(data=macro_plot_long, aes(x=date, y=US2Y_plot, color="US2Y", linetype="US2Y"), size=0.6) +
  
  # facet by rating × tenor
  facet_grid(rating ~ tenor, scales="free_y",drop=TRUE) +
  
  # x轴每一年
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  
  # labels
  labs(y="Spread (bp)", x="Date", color="Sector / Macro", linetype="Macro") +
  
  # colors & linetype manual
  scale_color_manual(values=c(
    # CDS sector
    "Comm"="#1b9e77", "Cons Disc"="#d95f02", "Cons Stap"="#7570b3", 
    "Energy"="#e7298a", "Fin Sr"="#66a61e", "Fin Sub"="#e6ab02",
    "Health"="#a6761d", "Industrials"="#666666", "Materials"="#1f78b4",
    "Tech"="#b2df8a", "Util"="#fb9a99",
    # macro
    "SPX"="black", "US2Y"="red"
  )) +
  scale_linetype_manual(values=c("SPX"="solid","US2Y"="dashed")) +
  
  # fill manual for panel background
  scale_fill_manual(values=c("IG"="lightblue", "HY"="lightpink")) +
  
  # legend 两行
  guides(color = guide_legend(nrow=2, byrow=TRUE),
         linetype = guide_legend(nrow=2, byrow=TRUE)) +
  
  # theme
  theme_minimal() +
  theme(
    strip.background = element_rect(fill="grey90", color="black", size=0.5),  # panel 背景颜色
    strip.text = element_text(face="bold"),
    legend.position="bottom",
    legend.direction="horizontal",
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(0.8, "lines"),
    legend.spacing.y = unit(0.2, "cm"),
    panel.spacing = unit(1, "lines")
  )
#plot HY 5Y density----
HY_5Y <- US_credit_long_macro_1[rating=="HY" & tenor=="5Y" & date >= start_date]
setDT(HY_5Y)

# 找每个 sector 的最大非 NA 日期作为 end_date
sector_end_dates <- HY_5Y[!is.na(spread), .(end_date = max(date)), by=sector]

# 过滤数据，只保留 start_date ~ end_date
HY_5Y_filtered <- HY_5Y[, .SD[date <= sector_end_dates[sector == .BY$sector]$end_date], by=sector]

#  计算 density，用于右边横向图
density_data <- HY_5Y_filtered[, {
  dens <- density(spread, na.rm=TRUE)
  data.table(spread = dens$x,
             density_value = dens$y)
}, by=sector]

#  映射 density 到 TS 图右侧 30% 区域，并预留 10% gap

gap_ratio <- 0.1       # 横向 gap 占比
density_ratio <- 0.3   # density 占比
x_min <- min(HY_5Y_filtered$date, na.rm=TRUE)
x_max <- max(HY_5Y_filtered$date, na.rm=TRUE)
x_range <- as.numeric(x_max - x_min)
gap_offset <- gap_ratio * x_range

density_data[, date_mapped := x_max + gap_offset + density_value/max(density_value) * (density_ratio*x_range), by=sector]

# 最新点横向 dash 指向 density
last_points <- HY_5Y_filtered[, .SD[which.max(date)], by=sector]

# 找对应 density 的 y 值 (spread)
density_last <- density_data[, .SD[which.min(abs(spread - last_points[sector==.BY$sector]$spread))], by=sector]

last_points <- merge(last_points, density_last[, .(sector, spread_density = spread, xend = date_mapped)], by="sector")
last_points[, yend := spread_density]

sector_colors <- c(
  "Comm"="#1b9e77", "Cons Disc"="#d95f02", "Cons Stap"="#7570b3", 
  "Energy"="#e7298a", "Fin Sr"="#66a61e", "Fin Sub"="#e6ab02",
  "Health"="#a6761d", "Industrials"="#666666", "Materials"="#1f78b4",
  "Tech"="#b2df8a", "Util"="#fb9a99"
)

#plot
HY_5Y_CDS_Density=ggplot() +
  # CDS TS lines (左边)
  geom_line(data = HY_5Y_filtered,
            aes(x = date, y = spread, color = sector),
            size=0.8) +
  
  # Density lines (右边横向)
  geom_line(data = density_data,
            aes(x = date_mapped, y = spread, color = sector),
            size=0.6, alpha=0.5) +
  
  # 虚线连接 TS 最后点到 density
  geom_segment(data = last_points,
               aes(x = date, xend = xend, y = spread, yend = yend),
               linetype="dashed", color="red",size=1.5) +
  
  # facet 按 sector
  facet_wrap(~sector, ncol=3, scales="free_y") +
  
  # labels
  labs(y = "Spread (bp)", x = "Date", title = "HY 5Y CDS TS + Spread Density") +
  
  # x 轴每两年显示
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  
  # colors
  scale_color_manual(values = sector_colors) +
  
  # theme
  theme_minimal() +
  theme(
    strip.text = element_text(face="bold"),
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = unit(1.5, "lines"),
    legend.key.height = unit(0.8, "lines")
  )


#plot IG 5Y TS Energy Sector Spiral plot-----
dt_test <- US_credit_long_macro_1[
  sector == "Energy" & tenor == "5Y" & rating == "IG" & !is.na(spread),
  .(date, spread)
]

# 假设 dt_test 已经准备好了
dt_test <- as.data.table(dt_test)
dt_test[, day_num := 1:.N]

# 计算每年起始点，用于标注年份
year_labels <- dt_test[, .SD[1], by = .(year = format(date, "%Y"))]  # 每年的第一天
year_labels[, y := 0.2*day_num]  # 放在底部

# 绘图
US_Energy_CDS_Spiral_Plot=ggplot(dt_test, aes(x = day_num %% 365,
                    y = 0.2*day_num + (spread*0.2)/2,  # 中心
                    height = spread*0.2,
                    fill = spread)) +
  geom_tile() +
  # 年份标注
  geom_text(data = year_labels, aes(x = 0, y = y, label = year),
            inherit.aes = FALSE, hjust = 0, vjust = -0.5, size = 4, color = "red") +
  scale_y_continuous(limits = c(-20, NA)) +
  scale_x_continuous(breaks = 30*0:11, minor_breaks = NULL, labels = month.abb) +
  coord_polar() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(x = "", y = "", fill = "Spread",
       title = "Energy 5Y IG CDS Spread Spiral")

#plot HY-IG spread for 5y, all sector----
# Filter 5Y data and date >= start_date
dt <- US_credit_long_macro_1[
  tenor == "5Y" & date >= start_date,
  .(date, sector, rating, spread)
]

# Reshape data to get HY and IG spreads side-by-side
dt_wide <- dcast(
  dt,
  date + sector ~ rating,
  value.var = "spread"
)

# Compute HY - IG spread difference
setDT(dt_wide)
dt_wide[, spread_diff := HY - IG]

# 剔除 HY/IG 全是 NA 的 sector
valid_sectors <- dt_wide[, .(has_data = any(!is.na(HY) & !is.na(IG))), by = sector][has_data == TRUE, sector]
dt_wide <- dt_wide[sector %in% valid_sectors]

# Convert back to long format for TS plotting
dt_long <- melt(
  dt_wide,
  id.vars = c("date", "sector", "spread_diff"),
  measure.vars = c("HY", "IG"),
  variable.name = "rating",
  value.name = "spread"
)

# Get list of sectors
sector_list <- unique(dt_long$sector)
n_sectors <- length(sector_list)

# Create empty list to store sector plots
plot_list <- list()
setDT(dt_long)
setDT(dt_wide)
for (i in seq_along(sector_list)) {
  sec <- sector_list[i]
  dt_ts  <- dt_long[sector == sec]
  dt_bar <- dt_wide[sector == sec]
  
  # HY vs IG spread time series
  p_ts <- ggplot(dt_ts, aes(x = date, y = spread, color = rating)) +
    geom_line(size = 0.8) +
    labs(
      title = sec,
      y = "5Y Spread (bp)",
      x = NULL,
      color = "Rating"
    ) +
    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      legend.position = ifelse(i == n_sectors, "right", "none")  # 只有最后一个显示HY/IG legend
    )
  
  # HY - IG spread bar chart
  p_bar <- ggplot(dt_bar, aes(x = date, y = spread_diff, fill = spread_diff)) +
    geom_col() +
    scale_fill_gradientn(colours = rainbow(7), guide = "none") + # 去掉彩虹legend
    labs(
      y = "5Y HY - IG",
      x = NULL
    ) +
    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y"
    ) +
    theme_minimal()
  
  # Combine TS and Bar vertically
  p_sector <- p_ts / p_bar + plot_layout(heights = c(2, 1))
  plot_list[[sec]] <- p_sector
}

# Arrange all sectors in a 3x3 grid
US_Credit_CDS_Spread <- wrap_plots(plot_list, ncol = 3)


#plot curve of HY-IG for given date----
date_US_date="2015-02-04"
# snapshot 数据，限制 tenor
tenor_keep <- c("1Y", "2Y", "3Y", "5Y", "7Y", "10Y")
dt_snapshot <- US_credit_long_macro_1[
  date == as.Date(date_US_date) & 
    !sector %in% c("Fin Sub", "Industrials") & 
    tenor %in% tenor_keep &
    !is.na(spread)
]

# 指定 tenor 顺序
dt_snapshot[, tenor := factor(tenor, levels = tenor_keep)]

# reshape 成 wide 方便 ribbon
dt_wide <- dcast(
  dt_snapshot,
  sector + tenor ~ rating,
  value.var = "spread"
)

# 删除 NA
setDT(dt_wide)
dt_wide <- dt_wide[!is.na(HY) & !is.na(IG)]

# 绘图
US_Credit_curve=ggplot(dt_wide, aes(x = tenor)) +
  # ribbon 放在折线下面
  geom_ribbon(aes(ymin = pmin(HY, IG), ymax = pmax(HY, IG)), 
              fill = "green", alpha = 0.3) +
  # HY / IG 折线和点
  geom_line(aes(y = HY, color = "HY", group = "HY"), size = 0.8) +
  geom_line(aes(y = IG, color = "IG", group = "IG"), size = 0.8) +
  geom_point(aes(y = HY, color = "HY", shape = "HY"), size = 3) +
  geom_point(aes(y = IG, color = "IG", shape = "IG"), size = 3) +
  # facet 按 sector
  facet_wrap(~sector, ncol = 3) +
  # 手动颜色和形状
  scale_color_manual(values = c("HY" = "#E41A1C", "IG" = "#377EB8")) +
  scale_shape_manual(values = c("HY" = 16, "IG" = 17)) +
  labs(
    title = paste0("Credit Spread Snapshot on ", date_US_date),
    x = "Tenor",
    y = "Spread (bp)",
    color = "Rating",
    shape = "Rating"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.text = element_text(hjust = 0.5, face = "bold", size = 12), # facet title 居中、稍大
    legend.position = "bottom",
    panel.grid = element_blank()
  )

#plot single-name CDS----
# 1 取美国 single name CDS

single_dt <- as.data.table(cd)[, c("date", US_Single_1), with = FALSE]

# 2 转成长表

single_long <- melt(
  single_dt,
  id.vars = "date",
  variable.name = "series",
  value.name = "value"
)

setDT(single_long)
single_long <- single_long[!is.na(value)]

# 3 提取公司名

single_long[, company := str_replace(series, "United States ", "")]
single_long[, company := str_replace(company, " CDS.*", "")]

# 4 每家公司单独缩放（增强形状）

single_long[, value_scaled :=
              (value - min(value, na.rm=TRUE)) /
              (max(value, na.rm=TRUE) - min(value, na.rm=TRUE)),
            by = company]

# 5 按平均 CDS 排序

single_order <- single_long[, .(avg = mean(value, na.rm=TRUE)), by = company]
single_order <- single_order[order(avg)]

single_long[, company := factor(company, levels = single_order$company)]

# 6 waterfall offset

single_long[, offset := as.numeric(company) * 0.8]
single_long[, value_plot := value_scaled + offset]

# 7 画图

US_Single_Name=ggplot(single_long,
                      aes(x = date,
                          y = value_plot,
                          group = company,
                          color = company,
                          fill = company)) +
  
  geom_ribbon(
    aes(ymin = offset, ymax = value_plot, fill = company),
    alpha = 0.25,
    colour = NA
  ) +
  
  geom_line(linewidth = 0.8, alpha = 0.9) +
  
  scale_y_continuous(
    breaks = unique(single_long$offset),
    labels = levels(single_long$company)
  ) +
  
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  
  labs(
    title = "US Single Name CDS Waterfall Plot",
    x = "Date",
    y = NULL
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face="bold", size=14)
  )


#plot North America OAS
US_Sector_OAS_HY
US_Sector_OAS_IG
cd[,..US_Sector_OAS]

library(tidyverse)
library(hrbrthemes) # 用于更好看的主题

# 1. 提取数据并转为长格式
# 假设你的原始数据框叫 cd
hy_data <- cd %>%
  select(date, all_of(US_Sector_OAS_HY)) %>%
  pivot_longer(cols = -date, names_to = "Sector", values_to = "OAS") %>%
  mutate(Rating = "High Yield")

ig_data <- cd %>%
  select(date, all_of(US_Sector_OAS_IG)) %>%
  pivot_longer(cols = -date, names_to = "Sector", values_to = "OAS") %>%
  mutate(Rating = "Investment Grade")

# 2. 合并数据
# 清洗一下 Sector 名称，去掉冗长的 "North America USD..." 方便画图
plot_data <- bind_rows(hy_data, ig_data) %>%
  mutate(Sector_Clean = str_remove(Sector, "North America USD (HY|IG) "),
         Sector_Clean = str_remove(Sector_Clean, ". OAS"))

# 直接使用你已经生成的 plot_data
ggplot(plot_data, aes(x = date, y = OAS, color = Rating, fill = Rating)) +
  # 增加一层半透明的填充，让面积图看起来更像专业的行情终端（如 Bloomberg/Reuters）
  geom_area(alpha = 0.1, position = "identity") + 
  geom_line(linewidth = 0.75) +
  # 按行业分面板，每个行业独立 y 轴坐标
  facet_wrap(~Sector, scales = "free_y", ncol = 3) +
  # 使用简洁的主题
  theme_minimal(base_family = "sans") +
  # 颜色方案：HY 为暖色，IG 为冷色
  scale_color_manual(values = c("High Yield" = "#D73027", "Investment Grade" = "#4575B4")) +
  scale_fill_manual(values = c("High Yield" = "#D73027", "Investment Grade" = "#4575B4")) +
  labs(
    title = "US Credit Sector Dashboard: Option-Adjusted Spreads (OAS)",
    subtitle = "Comparing High Yield vs Investment Grade across North American Sectors",
    x = NULL,
    y = "Spread (bps)",
    caption = paste("Source: Your Data Source | Latest Update:", max(plot_data$date))
  ) +
  # 细节修饰
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    strip.text = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
