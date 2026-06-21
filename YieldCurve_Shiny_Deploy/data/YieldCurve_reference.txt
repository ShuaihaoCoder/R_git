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

####test-----

# 1. 输入数据：tenor + zero rate

#use SOFR OIS test
curvenames_test="NZD OIS"
n=names(r)[str_detect(names(r), curvenames_test)]
cols=c(n,"date")
date1="2024-12-19"
row=which(r$date==date1)
# ifelse(row>1,print(row),print("No Data for such date"))
if(length(row)==0){
  print("No Data for Such Date")
}else{
  print(row)
}
# r[row,..cols] 
dt <- r[row, ..cols]
# 1️⃣ 提取列名
colnames <- names(dt)
# 2️⃣ 提取 tenor 部分（例如 "1 MO"、"2 WK"、"10 YR"）
tenor_str <- str_extract(colnames, "\\d+\\s*(WK|MO|YR)")
# 3️⃣ 转换成以年为单位
tenor <- case_when(
  str_detect(tenor_str, "WK") ~ as.numeric(str_extract(tenor_str, "\\d+")) / 52,
  str_detect(tenor_str, "MO") ~ as.numeric(str_extract(tenor_str, "\\d+")) / 12,
  str_detect(tenor_str, "YR") ~ as.numeric(str_extract(tenor_str, "\\d+")),
  TRUE ~ NA_real_
)

# 4️⃣ 提取对应数值（你的 zero rate）
zeroRate <- as.numeric(dt[1, ])
zeroRate <- zeroRate[-length(zeroRate)]
zeroRate=na.locf(zeroRate)
tenor <- tenor[-length(tenor)]

#original input
# tenor <- c(0.5, 1, 2, 3, 5, 7, 10)       # 年
# zeroRate <- c(0.025, 0.037, 0.049, 0.050, 0.052, 0.053, 0.054)  # 小数
# ns_fit <- Nelson.Siegel(zeroRate, tenor)#fit not good


ord <- order(tenor)
tenor_sorted <- tenor[ord]
zeroRate_sorted <- zeroRate[ord]
# 去掉太短的 maturities
sel <- tenor_sorted >= 0.25
tenor_use <- tenor_sorted[sel]
zeroRate_use <- zeroRate_sorted[sel]

# 拟合 Nelson–Siegel 模型
# ns_fit <- Nelson.Siegel(zeroRate_use,tenor_use)
# coef=unlist(ns_fit) %>% as.vector()

# 2. 拟合 Nelson-Siegel 曲线

# NS函数
NS_yield <- function(t, beta0, beta1, beta2, tau) {
  beta0 + beta1 * (1 - exp(-t/tau)) / (t/tau) + beta2 * ((1 - exp(-t/tau)) / (t/tau) - exp(-t/tau))
}

NS_obj <- function(p) {
  y_fit <- NS_yield(tenor, p[1], p[2], p[3], p[4])
  sum((zeroRate - y_fit)^2)
}

start <- c(3.8, 0.02, 0.01, 5)  # 初值
res <- optim(start, NS_obj, method="L-BFGS-B",
             lower = c(-10,-4,-5,0.01),  # tau > 0
             upper = c(10,4,5,15))
NS_params <- setNames(res$par, c("beta0","beta1","beta2","tau"))
# NS_params <- setNames(as.vector(ns_fit), c("beta0","beta1","beta2","tau"))#directly use ns_fit result
# NS_params[] <- coef
#beta0=level,long term average
#beta1=slope,positive <- inverted curve, negative=normal,larger <- steeper
#beta2=shape, negative <- concave up, positive <- concave down
#tau=decay,hump! larger <- hump will earlier (10 vs 0.1)

NSHelp <- paste(
  "# beta0 = level, long term average",
  "# beta1 = slope, positive <- inverted curve, negative = normal, larger <- steeper",
  "# beta2 = shape, negative <- concave up, positive <- concave down",
  "# tau   = decay, hump! larger <- hump will earlier (10 vs 0.1)",
  sep = "\n"
)

# 3. 生成平滑 zero rate 曲线
time_grid <- seq(0.1, 30, by = 0.1) 
smooth_zero <- NS_yield(time_grid, NS_params["beta0"], NS_params["beta1"], NS_params["beta2"], NS_params["tau"])

# 查看前几条结果
par(mfrow=c(1,1))
plot(
  time_grid,
  smooth_zero,
  type = "l",
  lwd = 2,
  col = "black",
  xlab = "Tenor",
  ylab = "Zero Rate",
  main = paste0("Zero Curve: Fitted vs Observed   ",curvenames_test," ",date1)
)

points(
  tenor,
  zeroRate,
  col = "red",
  pch = 16,
  cex = 1.8
)

legend(
  "topright",
  legend = c("Fitted Zero Curve", "Observed Zero Rates"),
  col = c("black", "red"),
  lwd = c(2, NA),
  pch = c(NA, 16),
  bty = "n"
)

# text(
#   x = max(time_grid) * 0.6,
#   y = min(smooth_zero),
#   pos = 3, 
#   labels = "Fitted curve via smoothing\nRed dots: market quotes",
#   adj = 0
# )
NSFitted <- recordPlot()

# plot(tenor,zeroRate,col="pink",cex=3)

#image of NS fitting

# 4. 计算 forward rate（t1 -> t2）
forward_rate <- function(t_start, t_end, params) {
  y1 <- NS_yield(t_start, params["beta0"], params["beta1"], params["beta2"], params["tau"])
  y2 <- NS_yield(t_end,   params["beta0"], params["beta1"], params["beta2"], params["tau"])
  ((1+y2)^t_end / (1+y1)^t_start)^(1/(t_end - t_start)) - 1
}
z=forward_rate(0.5,2.5,NS_params)
# class(z)
# as.vector(z)
# 5. 计算 carry 和 roll（持有 zero coupon bond）

# OIS carry & roll

carry_roll_ois <- function(t_start, t_end, hold, NS_params,dir) {

  d=ifelse(dir==1|dir=="R"|dir=="Receive"|dir=="r"|dir=="receive",1,-1)
  # carry+roll
 roll <- (forward_rate(t_start,t_end,NS_params)-forward_rate(0,t_end-hold,NS_params))*d
 carry <- (forward_rate(hold,t_end,NS_params)-forward_rate(t_start,t_end,NS_params))*d*ifelse((hold-t_start)<0,0,1)
 duration <- (t_end-hold)
  total_return <- carry + roll
  list(carry = carry, roll = roll,duration=duration, dir=d,total_return = total_return)
  
}


# 使用示例

w=carry_roll_ois(0, 3, 1, NS_params,dir=1)
# w

# forward_rate(1,3,NS_params)
# forward_rate(0,3,NS_params)
x=unlist(w) %>% as.vector()
# x

###build up table and plot----
curve=NS_params
hold_t=c(1/12,3/12,6/12,1)


# 1) 构造数据
tenor_carry=c(seq(1:5),7,10,15,20,30)
rows <- list()
for (t in tenor_carry) {
  spot <- as.vector(forward_rate(0, t, NS_params))
  for (h in hold_t) {
    # 排除 1Y tenor + 1Y hold
    if (t == 1 & h == 1) next
    r.1 <- carry_roll_ois(0, t, h, NS_params, dir = 1)
    rows[[length(rows) + 1]] <- list(
      tenor = t,
      hold = h,
      hold_label = if (h < 1) paste0(round(h*12), "m") else paste0(h, "y"),
      carry = as.vector(r.1$carry),
      roll  = as.vector(r.1$roll),
      total = as.vector(r.1$total_return),
      spot  = spot
    )
  }
}
dt <- rbindlist(rows)


dt[, `:=`(
  carry = carry * 100,   # bps
  roll  = roll * 100,
  total = total * 100,
  spot  = spot * 1       # %
)]

# 3) 左图数据准备

dt[, hold_label := factor(hold_label, levels = unique(dt[order(hold), hold_label]))]

plot_dt <- melt(
  dt,
  id.vars = c("tenor", "hold", "hold_label", "total", "spot"),
  measure.vars = c("carry", "roll"),
  variable.name = "component",
  value.name = "value"
)


# 4) 左图：facet 横向堆叠柱 + total 点和数值

p_left <- ggplot(plot_dt, aes(y = factor(tenor), x = value, fill = component)) +
  geom_col(position = "stack", width = 0.8) +
  geom_point(aes(x = total), color = "red", size = 3, shape = 16) +
  geom_text(aes(x = total, label = round(total,1)), color = "black",
            hjust = -0.3, size = 4) +
  facet_wrap(~ hold_label, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = c("carry" = "#3E8ED0", "roll" = "#E8A317")) +
  theme_minimal(base_size = 13) +
  labs(
    title = paste("Carry / Roll / Total by Tenor (Faceted by Hold Period)  ",curvenames_test," ",date1),
    x = "Value (bps)",
    y = "Tenor (Years)",
    fill = NULL
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold")
  )


# 5) 右图：spot rate 曲线 + 点上显示数值

spot_dt <- unique(dt[, .(tenor, spot)])
p_right <- ggplot(spot_dt, aes(x = tenor, y = spot)) +
  geom_line(color = "grey30", size = 1) +
  geom_point(color = "grey30", size = 2) +
  geom_text(aes(label = round(spot,2)), vjust = -0.5, size = 4) +
  scale_x_continuous(breaks = spot_dt$tenor) +
  labs(title = "Spot Rate Curve (%)", x = "Tenor (Years)", y = "Spot (%)") +
  theme_minimal(base_size = 13)


# 6) 拼接左右图

final_plot <- p_left + p_right + plot_layout(widths = c(2, 1))
# print(final_plot)
# ggplotly(p_left)
# ggplotly(final_plot)



# -------heatmap-------------

heat_dt <- unique(dt[, .(tenor, hold_label, total)])

# 绘制热力图，使用 rainbow 渐变
carry_heat_rainbow <- ggplot(heat_dt, aes(x = factor(tenor), y = hold_label, fill = total)) +
  geom_tile(color = "white") +  # 格子白边
  geom_text(aes(label = round(total,1)), color = "black", size = 5) + # total 数值
  scale_fill_gradientn(colors = rainbow(7),  # rainbow 渐变 7 色
                       name = "Total (bps)") +
  labs(
    title =paste("Total Carry + Roll Heatmap (Rainbow Colors)",curvenames_test),
    x = "Tenor (Years)",
    y = "Hold Period"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# print(carry_heat_rainbow)


###fit curve from ZERORATE CURVE----
zc=readRDS("ZERORATE_CURVE")
curvenames=unique(zc$DES) 
curveselected="AUD AUSTRALIA (vs. 6M Bank Bills)"
curvedetail=filter(zc,DES==curveselected)
tenor_selected <- curvedetail$MTY_YEARS_TDY %>% as.vector()
zerorate_selected<- curvedetail$PX_MID %>% as.vector()

NS_obj_selected <- function(p) {
  y_fit <- NS_yield(tenor_selected, p[1], p[2], p[3], p[4])
  sum((zerorate_selected - y_fit)^2)
}
start=start
res_selected <- optim(start, NS_obj_selected, method="L-BFGS-B",
             lower = c(-10,-4,-5,0.01),  # tau > 0
             upper = c(10,4,5,10))
NS_params_selected <- setNames(res_selected$par, c("beta0","beta1","beta2","tau"))
time_grid_selected <- seq(0.1, 30, by = 0.1) 
smooth_zero_selected<- NS_yield(time_grid_selected, NS_params_selected["beta0"], NS_params_selected["beta1"], NS_params_selected["beta2"], NS_params_selected["tau"])
plot(
  time_grid_selected,
  smooth_zero_selected,
  type = "l",
  lwd = 2,
  col = "black",
  xlab = "Tenor",
  ylab = "Zero Rate",
  main = "Zero Curve: Fitted vs Observed"
)
mtext(
  paste0(
    curvedetail$COUNTRY_FULL_NAME %>% unique(),
    " | Currency: ",
    curvedetail$CRNCY %>% unique(),
    " | names: ",
    curvedetail$DES %>% unique()
  ),
  side = 3,      # 上方
  line = 0,      # 紧贴 main title
  cex = 0.9
)

points(
  tenor_selected,
  zerorate_selected,
  col = "red",
  pch = 16,
  cex = 1.8
)

legend(
  "topright",
  legend = c("Fitted Zero Curve", "Observed Zero Rates"),
  col = c("black", "red"),
  lwd = c(2, NA),
  pch = c(NA, 16),
  bty = "n"
)
###Plot NS Function wrapper using ZERORATE Data----
curvenames
plot_NS_curve <- function(curve_name,start=start,
                          grid_from = 0.1,
                          grid_to = 30,
                          grid_by = 0.1) {
  
  ## 1. 选曲线
  curvedetail <- dplyr::filter(zc, DES == curve_name)
  
  if (nrow(curvedetail) == 0) {
    stop("Curve name not found in ZERORATE_CURVE")
  }
  
  tenor <- curvedetail$MTY_YEARS_TDY %>% as.vector()
  zerorate <- curvedetail$PX_MID %>% as.vector()
  idx <- is.finite(tenor) & is.finite(zerorate) & tenor > 0
  tenor <- tenor[idx]
  zerorate <- zerorate[idx]
  
  ## 2. NS objective
  NS_obj <- function(p) {
    y_fit <- NS_yield(tenor, p[1], p[2], p[3], p[4])
    sum((zerorate - y_fit)^2)
  }
  
  ## 3. Optimization
  res <- optim(
    start,
    NS_obj,
    method = "L-BFGS-B",
    lower = c(-10, -4, -5, 0.01),
    upper = c(10, 4, 5, 20)
  )
  
  NS_params <- setNames(res$par, c("beta0","beta1","beta2","tau"))
  print(NS_params)
  
  ## 4. Smooth curve
  time_grid <- seq(grid_from, grid_to, by = grid_by)
  smooth_zero <- NS_yield(
    time_grid,
    NS_params["beta0"],
    NS_params["beta1"],
    NS_params["beta2"],
    NS_params["tau"]
  )
  
  ## 5. Plot
  plot(
    time_grid,
    smooth_zero,
    type = "l",
    lwd = 2,
    col = "black",
    xlab = "Tenor (Years)",
    ylab = "Zero Rate",
    main = "Zero Curve: Fitted vs Observed"
  )
  
  ## 6. Subtitle
  mtext(
    paste0(
      unique(curvedetail$COUNTRY_FULL_NAME),
      " | Currency: ", unique(curvedetail$CRNCY),
      " | Curve: ", unique(curvedetail$DES)
    ),
    side = 3,
    line = 0,
    cex = 0.9
  )
  points(
    tenor,
    zerorate,
    col = "red",
    pch = 16,
    cex = 1.8
  )
  
  legend(
    "topright",
    legend = c("Fitted Zero Curve", "Observed Zero Rates"),
    col = c("black", "red"),
    lwd = c(2, NA),
    pch = c(NA, 16),
    bty = "n"
  )
  
  ## 7. 返回结果（不打扰画图）
  invisible(list(
    params = NS_params,
    fitted = data.frame(
      tenor = time_grid,
      zero = smooth_zero
    )
  ))
}
### Plot r.rds rate data----
keywords="EUR ESTR OIS"
n.r=names(r)[str_detect(names(r), keywords)]
cols.r=c(n.r,"date")
date1.r="2024-10-22"
row.r=which(r$date==date1.r)
# r[row,..cols] 
dt.r <- r[row, ..cols.r]
# 1️⃣ 提取列名
colnames.r <- names(dt.r)
# 2️⃣ 提取 tenor 部分（例如 "1 MO"、"2 WK"、"10 YR"）
tenor_str.r <- str_extract(colnames.r, "\\d+\\s*(WK|MO|YR)")
# 3️⃣ 转换成以年为单位
tenor.r <- case_when(
  str_detect(tenor_str.r, "WK") ~ as.numeric(str_extract(tenor_str.r, "\\d+")) / 52,
  str_detect(tenor_str.r, "MO") ~ as.numeric(str_extract(tenor_str.r, "\\d+")) / 12,
  str_detect(tenor_str.r, "YR") ~ as.numeric(str_extract(tenor_str.r, "\\d+")),
  TRUE ~ NA_real_
)

# 4️⃣ 提取对应数值r.rds at date.1.r
zerorate.r <- as.numeric(dt.r[1, ])
zerorate.r <- zerorate.r[-length(zerorate.r)]
tenor.r <- tenor.r[-length(tenor.r)]
#wrap function from r.rds
plot_NS_from_r <- function(keyword, date1,start=start) {
  # r is global
  # start is global
  
  # 1️⃣ 找到匹配关键词的所有列名
  n.r <- names(r)[str_detect(names(r), keyword)]
  if (length(n.r) == 0) {
    stop("❌ 找不到包含该 keyword 的曲线：", keyword)
  }
  
  # 2️⃣ 取这些列 + date
  cols.r <- c(n.r, "date")
  
  # 3️⃣ 选择该日期的数据
  row.r <- which(r$date == date1)
  if (length(row.r) == 0) stop("❌ 没有找到该日期：", date1)
  
  dt.r <- r[row.r, ..cols.r]
  
  # 4️⃣ 提取 tenor
  colnames.r <- names(dt.r)
  tenor_str.r <- str_extract(colnames.r, "\\d+\\s*(WK|MO|YR)")
  
  tenor.r <- case_when(
    str_detect(tenor_str.r, "WK") ~ as.numeric(str_extract(tenor_str.r, "\\d+")) / 52,
    str_detect(tenor_str.r, "MO") ~ as.numeric(str_extract(tenor_str.r, "\\d+")) / 12,
    str_detect(tenor_str.r, "YR") ~ as.numeric(str_extract(tenor_str.r, "\\d+")),
    TRUE ~ NA_real_
  )
  
  # 5️⃣ 提取 zero rate 数值
  zerorate.r <- as.numeric(dt.r[1, ])
  
  # ⚠ 去掉最后一列“date”
  zerorate.r <- zerorate.r[-length(zerorate.r)]
  tenor.r <- tenor.r[-length(tenor.r)]
  
  # 6️⃣ 去掉 NA 数据
  valid <- !(is.na(tenor.r) | is.na(zerorate.r))
  tenor.r <- tenor.r[valid]
  zerorate.r <- zerorate.r[valid]
  
  if (length(tenor.r) < 3) stop("❌ 可用的 tenor 数据太少，无法进行 NS 拟合")
  
  # 7️⃣ Nelson–Siegel 目标函数
  NS_obj_r <- function(p) {
    y_fit <- NS_yield(tenor.r, p[1], p[2], p[3], p[4])
    sum((zerorate.r - y_fit)^2)
  }
  
  # 8️⃣ 优化 NS
  res_r <- optim(
    start, NS_obj_r, method = "L-BFGS-B",
    lower = c(-10, -10, -5, 0.01),
    upper = c(10, 10, 5, 20)
  )
  
  if (!is.finite(res_r$value)) {
    stop("❌ NS 优化失败：可能数据不够或波动太大")
  }
  
  params <- setNames(res_r$par, c("beta0","beta1","beta2","tau"))
  
  # 9️⃣ 拟合曲线
  time_grid <- seq(0.01, 30, by = 0.1)
  smooth_zero <- NS_yield(time_grid, params["beta0"], params["beta1"], params["beta2"], params["tau"])
  
  # 1️⃣0️⃣ 绘图
  plot(
    time_grid, smooth_zero,
    type = "l", lwd = 2, col = "black",
    xlab = "Tenor (Years)",
    ylab = paste(keyword," Rate"),
    main = paste0("Fitted vs Observed NS Curve (", keyword, ")")
  )
  
  # 原始点
  points(tenor.r, zerorate.r, col = "red", cex = 2, pch = 19)
  
  # legend
  legend(
    "topright",
    legend = c("Fitted NS Curve", "Observed Data"),
    col = c("black", "red"),
    lty = c(1, NA),
    pch = c(NA, 19),
    bty = "n"
  )
  
  # 上方文字说明
  mtext(
    paste0("Keyword: ", keyword,
           " | Date: ", date1,
           " | Cols#: ", length(n.r)),
    side = 3, line = -0.8, cex = 0.9
  )
}

#



