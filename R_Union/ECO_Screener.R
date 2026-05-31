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
library(countrycode)
# remotes::install_github("jimjam-slam/ggflags")
library(ggimage) 
library(RColorBrewer)
library(plotly)
library(fmsb)
library(plotly)
library(dplyr)
library(tidyr)

# setwd("C:/Users/ASUS/Desktop/R_Union")
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
x=readRDS(file="WIDE_ALLX")#better eco data

###to-do list config----
# date="2024-10-10" This is to be defined by sourced_combine_all
row=which(x$date == date)
if(length(row)==0){
  print("No Data for Such Date")
}else{
  print(row)
}
# grep("SINGAPORE",names(x),value=T)
DM=vector()
EM=vector()
# substr(names(x), 1, 8) %>% unique()
DM=c("United States","Britain","Japan","Norway","Canada","Australia","Germany","EUROZONE","Singapore",
     "New Zealand","switzerland","Hong Kong")
DM=toupper(DM)
# grep(paste(DM, collapse = "|"), names(x), value = TRUE) %>% substr(1,10) %>% unique()
# grep(DM[5],names(x),value=T)
# # grep(paste0("(", paste(DM, collapse = "|"), ")(?=.*GDP)(?=.*YoY)"),
#      names(x), value = TRUE, ignore.case = TRUE, perl = TRUE)
EM=c("China","Korea","Taiwan","India","Indonesia","Philippines","Mexico","South Africa","Turkey")
EM=toupper(EM)
# grep(paste(EM, collapse = "|"), names(x), value = TRUE) %>% substr(1,10) %>% unique()
# grep(EM[5],names(x),value=T)
x1 <- as.data.frame(lapply(x, function(col) na.locf(col, na.rm = FALSE)))
names(x1) <- names(x) 
#GDP
DMGDP=grep(paste0("(", paste(DM, collapse = "|"), ")(?=.*GDP)(?=.*YoY)"),
           names(x), value = TRUE, ignore.case = TRUE, perl = TRUE)
DMGDP=c(DMGDP,"EUROZONE Euro Area Gross Domestic Product Chained Prices YoY")
setDT(x1)
DMGDPdata=x1[row, ..DMGDP, with = FALSE]
DMGDP_CTY=str_extract(names(DMGDPdata), paste(DM, collapse="|"))

#CPI
DMCPI=grep(paste0("(", paste(DM, collapse = "|"), ")(?=.*CPI)(?=.*YoY)"),
           names(x), value = TRUE, ignore.case = TRUE, perl = TRUE)
DMCPI <- c(DMCPI,"EUROZONE Euro Area MUICP All Items YoY NSA")
setDT(x1)
DMCPIdata=x1[row, ..DMCPI, with = FALSE]
DMCPI_CTY=str_extract(names(DMCPIdata), paste(DM, collapse="|"))

#IP
DMIP=grep(paste0("(", paste(DM, collapse = "|"), ")(?=.*Industrial Production)(?=.*YoY)"),
names(x), value = TRUE, ignore.case = TRUE, perl = TRUE)[c(1:7,9:12)]
DMIPdata=x1[row, ..DMIP, with = FALSE]
DMIP_CTY=str_extract(names(DMIPdata), paste(DM, collapse="|"))

#Unemployment
DMUE=grep(paste0("(", paste(DM, collapse = "|"), ")(?=.*Unemploy)"),
     names(x), value = TRUE, ignore.case = TRUE, perl = TRUE)[c(1:4,7:12,14,15)]
DMUEdata=x1[row, ..DMUE, with = FALSE]
DMUE_CTY=str_extract(names(DMUEdata), paste(DM, collapse="|"))

#PPI
DMPPI=grep(paste0("(", paste(DM, collapse = "|"), ")(?=.*(PPI|Producer))"),
     names(x), value = TRUE, ignore.case = TRUE, perl = TRUE)[c(1,2,5,6,7,8:10,16)]
others=c("CANADA STCA Industrial Product Price YoY NSA","JAPAN Input-Output Price Index of Manufacturing Industry Output YoY (Base=2005)" )
DMPPI=c(DMPPI,others)
DMPPIdata=x1[row, ..DMPPI, with = FALSE]
DMPPI_CTY=str_extract(names(DMPPIdata), paste(DM, collapse="|"))

#retail sales
DMRS=grep(
  paste0(
    "(", paste(DM, collapse = "|"), ")",   # DM 向量中任意元素
    "(?=.*Retail)",                        # AND 包含 Retail
    "(?=.*YoY)"                            # AND 包含 YoY
  ),
  names(x),
  value = TRUE,
  perl = TRUE,
  ignore.case = TRUE
)[c(1:3,5,6,9:14)]
DMRS <- c(DMRS,"UNITED STATES Adjusted Retail & Food Services Sales Total Yearly % Change SA")
DMRSdata=x1[row, ..DMRS, with = FALSE]
DMRS_CTY=str_extract(names(DMRSdata), paste(DM, collapse="|"))

#budget balance GDP
DMBB=grep(
  paste0(
    "(", paste(DM, collapse = "|"), ")",   # DM 向量中任意元素
    "(?=.*Budget)",                        # AND 包含 Retail
    "(?=.*GDP)"                            # AND 包含 YoY
  ),
  names(x),
  value = TRUE,
  perl = TRUE,
  ignore.case = TRUE
)
DMBBdata=x1[row, ..DMBB, with = FALSE]
DMBB_CTY=str_extract(names(DMBBdata), paste(DM, collapse="|"))

#CA Balance GDP
DMCA=grep(
  paste0(
    "(", paste(DM, collapse = "|"), ")",   # DM 向量中任意元素
    "(?=.*Current Account Balance)",                        # AND 包含 Retail
    "(?=.*GDP)"                            # AND 包含 YoY
  ),
  names(x),
  value = TRUE,
  perl = TRUE,
  ignore.case = TRUE
)
DMCAdata=x1[row, ..DMCA, with = FALSE]
DMCA_CTY=str_extract(names(DMCAdata), paste(DM, collapse="|"))

#PMI
pick_pmi <- function(x) {
  p1 <- grep("(?=.*Manufacturing)(?=.*PMI)", x, value = TRUE, perl = TRUE)
  if (length(p1)) p1 else grep("PMI", x, value = TRUE)
}
DMPMI=grep(paste0(
  "(", paste(DM, collapse = "|"), ")"),pick_pmi(names(x1)),value=T)[1:4]
DMPMI=c(DMPMI,grep("(?=.*PMI)(?=.*NORWAY)", names(x1), value = TRUE, perl = TRUE))
DMPMI=c(DMPMI,grep("(?=.*PMI)(?=.*SWITZERLAND)", names(x1), value = TRUE, perl = TRUE))
DMPMI=c(DMPMI,grep("(?=.*PMI)(?=.*HONG KONG)", names(x1), value = TRUE, perl = TRUE))

DMPMIdata=x1[row, ..DMPMI, with = FALSE]
DMPMI_CTY=str_extract(names(DMPMIdata), paste(DM, collapse="|"))

#FX Reserve
DMReserve=grep(
  paste0(
    "(", paste(DM, collapse = "|"), ")",   # DM 向量中任意元素
    "(?=.*Foreign)"                      # AND 包含 Retail                          # AND 包含 YoY
  ),
  names(x),
  value = TRUE,
  perl = TRUE,
  ignore.case = TRUE
)[c(1:4,7:9,13:18)]
DMReservedata=x1[row, ..DMReserve, with = FALSE]
DMReserve_CTY=str_extract(names(DMReservedata), paste(DM, collapse="|"))


###make DM eco data table----
nx=ls()
DMdata <- grep("^DM.+data$", nx, value = TRUE)
DMCTY <- grep("^DM.+_CTY$", nx, value = TRUE)
# get(DMdata[4])

# 提取中间字符串，例如 DMGDPdata -> GDP
extract_middle <- function(x) sub("^DM(.*)data$", "\\1", x)

# 目标大矩阵
all_country <- unique(unlist(mget(DMCTY)))
all_indicator <- extract_middle(DMdata)

result <- matrix(NA, 
                 nrow = length(all_country), 
                 ncol = length(all_indicator),
                 dimnames = list(all_country, all_indicator))

# 填入数据
for(i in seq_along(DMdata)) {
  dname <- DMdata[i]     # 例如 "DMGDPdata"
  cname <- DMCTY[i]      # 例如 "DMGDP_CTY"
  
  # 变量内容
  dat <- get(dname)      # 1 行多列的数据
  cty <- get(cname)      # 某些国家名向量（你已做好）
  
  values <- as.numeric(dat[1, ])   # 第 1 行
  names(values) <- cty             # 对应国家名
  
  # indicator 列名
  ind <- extract_middle(dname)
  
  # 填数据（自动处理缺失国家 → NA）
  result[names(values), ind] <- values
}

#result
colnames(result)=c("Budget Bal %GDP","CA Bal %GDP","CPI","Real GDP","IP","PMI","PPI","FX Reserve","Retail Sales","Unemployment")

#to check NA term
# grep("(?=.*Manufacturing PMI)", names(x1), value = TRUE, perl = TRUE)

#scoreboard----

df=as.data.frame(result)

# 1️⃣ 把行名转成 Country 列
df2 <- df %>%
  rownames_to_column(var = "Country")

# 2️⃣ 指标列名（除 Country 外）
macro_vars <- colnames(df2)[-1]

# 3️⃣ 计算 Total_Score
df_scaled <- df2 %>%
  rowwise() %>%
  mutate(
    Total_Score = {
      vals <- c_across(all_of(macro_vars))
      # 每行各指标归一化后求平均（忽略 NA）
      vals_scaled <- (vals - min(vals, na.rm = TRUE)) / (max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE))
      mean(vals_scaled, na.rm = TRUE)
    }
  ) %>%
  ungroup()

# 保留原始国家顺序
country_levels <- df2$Country

# 4️⃣ 转成长表，Total_Score 放最后
heat_df <- df_scaled %>%
  pivot_longer(cols = c(all_of(macro_vars), "Total_Score"),
               names_to = "Metric", values_to = "Value") %>%
  mutate(
    Metric = factor(Metric, levels = c(macro_vars, "Total_Score")), # Total_Score 最右
    Country = factor(Country, levels = country_levels)              # y 轴顺序固定
  ) %>%
  group_by(Metric) %>%
  mutate(Value_scaled = scales::rescale(Value)) %>%  # 每列归一化
  ungroup() %>%
  mutate(
    Label = ifelse(Metric %in% c("FX Reserve","PMI","Total_Score"),
                   round(Value, 2),
                   paste0(round(Value, 1), "%"))
  )

# 5️⃣ 标记每列最大值和最小值
highlight_df <- heat_df %>%
  group_by(Metric) %>%
  mutate(
    is_max = Value == max(Value, na.rm = TRUE),
    is_min = Value == min(Value, na.rm = TRUE)
  ) %>%
  ungroup()

# 6️⃣ 绘图
DMscorep <- ggplot(highlight_df, aes(x = Metric, y = Country, fill = Value_scaled)) +
  geom_tile(color = "white") +
  
  # 最大值发光
  geom_tile(data = subset(highlight_df, is_max),
            color = NA, fill = "yellow", alpha = 0.3, width = 1.1, height = 1.1) +
  geom_tile(data = subset(highlight_df, is_max),
            color = "red", size = 1.2, fill = NA) +
  
  # 最小值发光
  geom_tile(data = subset(highlight_df, is_min),
            color = NA, fill = "lightblue", alpha = 0.3, width = 1.1, height = 1.1) +
  geom_tile(data = subset(highlight_df, is_min),
            color = "blue", size = 1.2, fill = NA) +
  
  # 文字，Total_Score 放大
  geom_text(aes(label = Label, size = ifelse(Metric == "Total_Score", 5, 4)),
            color = "black") +
  
  scale_fill_gradientn(colors = terrain.colors(7)) +
  scale_size_identity() +  # 保持 size 实际值
  labs(title = "Macro Scoreboard + Total Score (Per-Metric Scaled, Glowing Max/Min)",
       x = "", y = "") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
# print(DMscorep)

#other plot
result=as.data.frame(result)
result_long <- result %>% 
  rownames_to_column(var="Country") %>% 
  pivot_longer(-Country, names_to="Indicator", values_to="Value")

#Radar
result <- as.data.frame(result)  # 确保是 data.frame
macro_vars <- colnames(result)

# ---------------------------
# 2. 归一化 0-1，用于雷达图
# ---------------------------
result_scaled <- result
result_scaled[macro_vars] <- apply(result[macro_vars], 2, scales::rescale)

# ---------------------------
# 3. 构造 radar_df 函数，返回 data.frame
# ---------------------------
make_radar_df <- function(x) {
  df <- rbind(
    rep(1, length(x)),  # max
    rep(0, length(x)),  # min
    x
  )
  df <- as.data.frame(df)
  colnames(df) <- macro_vars
  return(df)
}

# ---------------------------
# 4. 设置统一颜色
# ---------------------------
main_col <- "#1f77b4"  # 蓝色

# ---------------------------
# 5. 设置多图布局
# ---------------------------
n <- nrow(result)
ncol <- 4
nrow <- ceiling(n / ncol)
par(mfrow = c(nrow, ncol), mar = c(1, 2, 2, 1))

# ---------------------------
# 6. 循环绘制每个国家
# ---------------------------
# for(i in 1:n){
#   
#   country_name <- rownames(result)[i]
#   vals_scaled <- as.numeric(result_scaled[i, ])
#   
#   radar_df <- make_radar_df(vals_scaled)
#   
#   # 画雷达图
#   radarchart(
#     radar_df,
#     axistype = 0,                       # 不显示刻度数字
#     pcol = main_col,                    # 多边形边框颜色
#     pfcol = adjustcolor(main_col, 0.5), # 半透明填充
#     plwd = 1.5,                         # 边框线宽
#     cglcol = "grey85",                  # 网格颜色
#     cglty = 1,
#     cglwd = 0.6,
#     vlcex = 0.9                         # 指标文字大小
#   )
#   
#   title(country_name, cex.main = 1.3, font.main = 2)
# }



#Facet
cool_palette <- c(
  "#0033FF",  # electric blue
  "#3366FF",  # bright blue
  "#6699FF",  # light bright blue
  "#00CCFF",  # bright cyan
  "#0099CC",  # deep cyan
  "#006699",  # ocean indigo
  "#003366",  # deep navy blue
  "#6600CC",  # cold violet
  "#330099",  # deep indigo purple
  "#9900FF",  # neon cold purple
  "#0055FF",  # added: vivid blue (fills gap between 0033FF and 3366FF)
  "#2200AA"   # added: deep indigo (fills darker purple slot)
)

# 动态匹配国家数量
countries <- unique(result_long$Country)
cool_palette <- cool_palette[1:length(countries)]
names(cool_palette) <- countries

p_bar <- ggplot(result_long, aes(x = Country, y = Value, fill = Country)) +
  geom_col(alpha = 0.9) +
  facet_wrap(~Indicator, scales = "free") +
  scale_fill_manual(values = cool_palette) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "none"
  ) +
  labs(y = "Value", x = "Country")
# ggplotly(p_bar)
# print(p_bar)
