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
#to find the K
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}


iris
col <- c(1, 2, 3, 4)
MyData <- dplyr::select(iris, all_of(col))
wssplot(MyData)
KM<-kmeans(MyData,4)
autoplot(KM, data = MyData, frame=TRUE)

#bbg example
tickers <- c("USGG10YR Index","USGG2YR Index","VIX Index","DXY Curncy","USURTOT Index","SPX Index")
fields <- c("PX_LAST")
start_date <- "2005-01-01"
end_date <- Sys.Date()
raw <- get_bbg(tickers, fields, start_date, end_date)
saveRDS(raw,file = "C:/Users/ASUS/Desktop/R_Union/ClusterData")
raw=readRDS(file = "C:/Users/ASUS/Desktop/R_Union/ClusterData")

# 2) 转 xts，按 ticker wide format
dt_wide <- dcast(raw, date ~ ticker, value.var="PX_LAST")
data_xts <- xts(dt_wide[,-1], order.by = as.Date(dt_wide$date))
data_xts[1,"USURTOT Index"]=5.2 #manual input number for NA for the first line
data_xts <- na.locf(data_xts, na.rm=FALSE)
colnames(data_xts) <- c("DXY","SPX","US10Y","US2Y","UNRATE","VIX")

# 3) 计算特征
term_spread   <- data_xts$US10Y - data_xts$US2Y
dgs10_chg     <- diff(data_xts$US10Y)
dollar_mom3   <- ROC(data_xts$DXY, n=3)
vix_level     <- data_xts$VIX
unemp_chg     <- data_xts$UNRATE

features <- na.omit(merge(term_spread, dgs10_chg, dollar_mom3, vix_level, unemp_chg))
colnames(features) <- c("term_spread","dgs10_chg","dollar_mom3","vix_level","unemp_chg")

# 4) 标准化
#features_z <- scale(features)
# 转成 data.table
DT <- as.data.table(features)

# 对每一列做 rolling z-score
features_z <- DT[, lapply(.SD, function(x) {
  frollapply(x, 275, function(y) (tail(y,1) - mean(y)) / sd(y), align = "right")
})]
features_z <- na.omit(xts(features_z, order.by = index(features)))

# 5) K-means 聚类
set.seed(123)
wssplot(features_z)
k <- 4
km <- kmeans(features_z, centers = k)
autoplot(km,features_z,frame=TRUE,frame.type="convex")
regime <- xts(km$cluster, order.by = index(features_z))
colnames(regime) <- "regime"
km$centers
# cluster overlap,use pca
pca <- prcomp(features_z, scale.=TRUE)
df_pca <- data.frame(pca$x[,1:2], cluster=as.factor(regime))

ggplot(df_pca, aes(x=PC1, y=PC2, color=cluster)) +
  geom_point(alpha=0.6) +
  labs(title="K-means Clusters in PCA Space") +
  theme_minimal()
# 假设 regime_daily 是 xts,plot

# 对齐数据
df_plot <- data.frame(
  date = index(regime),
  cluster = as.factor(coredata(regime)),
  term_spread = coredata(term_spread[index(regime)])
)


# 假设 regime 是 xts 对象
df_plot <- data.frame(
  date = index(regime),
  cluster = as.factor(coredata(regime))
)

# 绘制 daily regime
ggplot(df_plot, aes(x=date, y=1, fill=cluster)) +
  geom_tile() +                       # 每天一个矩形块
  scale_y_continuous(breaks=NULL) +   # 隐藏 y 轴
  scale_x_date(
    date_breaks = "1 year",           # 每年显示一次刻度
    date_labels = "%Y"
  ) +
  labs(title="Daily Regime / Cluster Over Time",
       x="Year", y="", fill="Cluster") +
  theme_minimal() +
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(angle=45, hjust=1)
  )

# regime 描述
regime_descr <- data.frame(coredata(features_z), regime=km$cluster)
regime_summary <- regime_descr %>%
  group_by(regime) %>%
  summarise(across(everything(), mean))
print(regime_summary)

# 5) 构造 signal：term_spread > 0 → long SPX, else 0
spy <- data_xts$SPX
spy_ret <- ROC(spy, type="discrete")#meiyou filter
signal <- ifelse(features$term_spread > 0, 1, 0)
signal <- xts(signal, order.by=index(features))
strat_ret <- signal * spy_ret[index(signal)]

data_all <- na.omit(merge(strat_ret, spy_ret, regime))
colnames(data_all) <- c("strat","spy","regime")

#先plot culumative return for all regime
RegimeRet<-cumprod(1+data_all$strat)-1
plot(RegimeRet)

# 6) 按 regime 统计表现
df <- data.frame(date=index(data_all), coredata(data_all))
stats_by_regime <- df %>%
  group_by(regime) %>%
  summarise(
    months = n(),
    mean_m = mean(strat, na.rm=TRUE),
    ann_ret = (1+mean(strat, na.rm=TRUE))^12 - 1,
    sd_m = sd(strat, na.rm=TRUE),
    ann_vol = sd_m * sqrt(12),
    sharpe = ann_ret / ann_vol,
    win_rate = mean(strat > 0, na.rm=TRUE),
    max_dd = {
      cumret <- cumprod(1 + strat)
      min_dd <- min(cumret / cummax(cumret) - 1, na.rm = TRUE)
      -min_dd   # 返回正数
    }
  )
print(stats_by_regime)



# 2. 计算每个 regime 下策略累计收益
df$cumret <- NA  # 先创建列

# 获取所有 regime
regimes <- unique(df$regime)

for(r in regimes){
  # 取当前 regime 的行索引
  idx <- which(df$regime == r)
  # 计算该 regime 下的累计收益
  df$cumret[idx] <- cumprod(1 + df$strat[idx]) - 1
}

#add total return of non-regime to df
df=cbind(df,RegimeRet)
colnames(df)[6]="total"
# 3. 绘图
ggplot(df, aes(x=date, y=cumret, color=as.factor(regime))) +
  geom_line(size=1) +
  geom_line(aes(y = total), color = "red",linetype = "dashed")+
  labs(title="Term Spread Signal under regimes performance",
       x="Date",
       y="Cumulative Return",
       color="Regime") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

#use ggplot
# 找 regime=1 的连续区间

df1 <- data.frame(
  date = df$date,
  term_spread = coredata(features$term_spread[index(df)]),
  regime = coredata(df$regime)
)
reg2_idx <- which(df1$regime == 1)
start_idx <- reg2_idx[c(TRUE, diff(reg2_idx) != 1)]
end_idx   <- reg2_idx[c(diff(reg2_idx) != 1, TRUE)]

# 创建高亮区域数据框
highlight <- data.frame(
  xmin = df$date[start_idx],
  xmax = df$date[end_idx],
  ymin = min(df$term_spread, na.rm=TRUE),
  ymax = max(df$term_spread, na.rm=TRUE)
)

# 绘图
ggplot(df1, aes(x=date, y=term_spread)) +
  # 阴影
  geom_rect(data=highlight, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            inherit.aes=FALSE, fill="blue", alpha=0.3) +
  # 折线
  geom_line(color="black", size=1) +
  labs(title="Term Spread with Regime 1 Highlight",
       x="Date",
       y="Term Spread (10Y-2Y)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

