
#Heatmap
#can be used as sanpshot of carry,vol,fundemental,trade war,positioning,sentiment, vs USJPY,USDCNH etc
install.packages("gplots")
install.packages("heatmap")
install.packages("RColorBrewer")

#Quarto+shiny dashboard
head(sunspots)
data.matrix(sunspots)
heatmap(data.matrix(sunspots))

###-------heatmap 用 sunspot.month 做 heatmap（行 = 年，列 = 月）
data("sunspot.month")   # base R 内建 time-series
methods(as)
# 基本信息
st <- start(sunspot.month)   # 如 c(1749, 1)
freq <- frequency(sunspot.month)  # 月度应该是 12
v <- as.numeric(sunspot.month)

# 把向量按年分块（每 12 个为一行），最后一行若不满则补 NA
months_per_year <- freq
n_years <- ceiling(length(v) / months_per_year)
#pad_len <- n_years * months_per_year - length(v)
#v_padded <- c(v, rep(NA, pad_len))

m <- matrix(v, nrow = n_years, byrow = TRUE)  # 每行 = 一年，列 = 月
# 生成行名（年份）和列名（月）
start_year <- st[1]
years <- start_year:(start_year + n_years - 1)
rownames(m) <- as.character(years)
colnames(m) <- month.abb[1:months_per_year]

# 可选：将 NA（未满的最后一年）设为 0 或保留为 NA（heatmap 会显示空白）
# m[is.na(m)] <- 0

# 画 heatmap —— 不做行/列聚类，保持时间顺序
# scale="none" 保持原始数值；Rowv=NA, Colv=NA 关闭聚类重排
res <- heatmap(m, Rowv = NA, Colv = NA, scale = "none",
               margins = c(6,8),  # 调整边距以显示年份和月份标签
               xlab = "Month", ylab = "Year",
               main = "Sunspot (monthly) — heatmap by Year × Month")

# 如果你想让颜色渐变更漂亮，可以先创建调色板：
cols <- colorRampPalette(c("purple", "yellow", "red"))(100)
heatmap(m, Rowv = NA, Colv = NA, scale="none", col = cols)
<- 

# ---- 如果你使用 pheatmap 或 heatmap.2（更强）
# library(pheatmap)
# p <- pheatmap::pheatmap(m, cluster_rows = FALSE, cluster_cols = FALSE, 
#                         main = "Sunspot heatmap")
# p 返回的对象里有 tree_row/tree_col 等（当 cluster_* = TRUE 时更有用）

# ---- 如何拿到 heatmap 的“内部数据”
# 当 heatmap 做聚类（默认）时，res 会包含 rowInd / colInd：
str(res)    # 查看 heatmap 返回的对象（可能包含 rowInd, colInd）
# 若 res$rowInd 存在，则表示行在图中被重新排列的索引；对应到原始矩阵：
# plotted_matrix <- m[res$rowInd, res$colInd]

# 若不做聚类（Rowv=NA, Colv=NA），直接用 m 即可：m[row, col] 就是每个 cell 的数值
####


###----Factor
mtcars
factor(mtcars)
table(mtcars$cyl)
factor(mtcars$cyl)
plot(factor(mtcars$cyl))
mtcars$cyl %>% plot()
par(mfrow=c(2,1))

###----read document----
install.packages("XML")
library(XML)
help(package="foreign")
readClipboard()

###-----reshape----
melt(ReadWideCOMM)
melt(ReadWideCOMM,id.vars = "date")

###tidyr+dplyr----
###describe----
library("MASS")
head(Cars93)
factor(Cars93$AirBags)
g=group_by(Cars93,by=AirBags)
library(dplyr)
mutate(g,sumpro=cumprod(MPG.city)) %>% dplyr::select(last_col())
df_num <- g %>% dplyr::select(where(is.numeric))
mat <- as.matrix(df_num[,-1])      # 转为矩阵（仍是 numeric）
rownames(mat) <- g$rowname   # 若需要把某一列当行名（或：rownames(df)）
heatmap(mat)#not good!

###country code----
# install.packages("countrycode")
# install.packages("ISOcodes")

library(countrycode)

# 示例向量
codes <- c("US", "CN", "GB", "FR")

# 转换成完整英文名
country_names <- countrycode(codes, origin = "iso2c", destination = "country.name")

country_names

