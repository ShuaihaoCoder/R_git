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
library(MASS)
library(leaps)
library(olsrr)
library(multcomp)
library(HH)
library(car)
library(bruceR)
library(RVAideMemoire)
library(tseries)
library(forecast)
library(fUnitRoots)
library(pwr)
library(GPArotation)
library(psych) 
library(lmtest)
library(vars)
library(sjPlot)
library(broom)
library(car)
library(corrplot)
library(ggcorrplot)
library(qgraph)
library(ppcor)
library(vcd)
library(patchwork)
library(ggridges)
# library(gghalves)
library(ggdist)
library(caret)


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

#Independent test,Correlation Analysis,Regression,Anova,Manova,Ancova,
#Power Analysis,Logistic,
#Times Series(ARIMA)+VAR Model
#EFA+PCA+Cluster
#Grange Causality

###regression----
# par
regname=vector()
rf=grep("(?=.*CAD)(?=.*USD)",names(f),value=T,perl=T)
rr=grep("CANADA",names(r),value=T,perl=T)[1:3]
re=grep("Canada",names(eq),value=T,perl=T)[1:3]
rfs=dplyr::select(f,all_of(c("date",rf)))
rrs=dplyr::select(r,all_of(c("date",rr)))
res=dplyr::select(eq,all_of(c("date",re)))
t1=inner_join(rfs,rrs,by="date")
test=inner_join(t1,res,by="date")         
# names(test)
test1=test[,c(1,2,5,11,12,15,16),with=F]
names(test1)=c("date","USDCAD","12mY","10y","2y","TSX60","TSXC")
head(test1)
test1=na.omit(test1)
fit <- lm(USDCAD~TSXC,data=test1)
summary(fit)
# plot(fit)
coefficients(fit)
confint(fit,level=0.95)
# fitted(fit) %>% plot()
# residuals(fit)
# predict(fit,newdata=data.frame(TSXC=test1$TSX60)) %>% plot()
# plot(fit)
fit1=lm(USDCAD~TSXC+I(TSXC^2),data=test1)
summary(fit1)
# plot(test1$TSXC,test1$USDCAD)
# abline(fit,col="red")
# lines(test1$TSXC,fitted(fit1),lty=2,col="purple")
setDT(test1)
plot_model(fit1,type="pred")

# estimator confidence interval
# tidy(fit1, conf.int = TRUE) %>%
#   filter(term != "(Intercept)") %>%
#   ggplot(aes(x = term, y = estimate)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
#   coord_flip() +
#   theme_minimal()


# 把 USDCAD 和 TSXC 转成 xts
usdcad_xts <- xts(test1$USDCAD, order.by = test1$date)
tsxc_xts   <- xts(test1$TSXC,   order.by = test1$date)

# 计算 daily return
test1[, USDCAD_ret := as.numeric(dailyReturn(usdcad_xts))]
test1[, TSXC_ret   := as.numeric(dailyReturn(tsxc_xts))]
# head(test1,10)
fit2=lm(USDCAD_ret~TSXC_ret,data=test1)
summary(fit2)
plot(test1$TSXC_ret,test1$USDCAD_ret,xlim = c(-0.05,0.05))
abline(fit2,col="red")
fit3=lm(USDCAD_ret~TSXC_ret+I(TSXC_ret^2),data=test1)
AIC(fit2,fit3)#smaller, the better
BIC(fit2,fit3)
points(test1$TSXC_ret,fitted(fit3),col="blue")
summary(fit3)

#交互项
fit4 <- lm(USDCAD_ret ~ TSXC_ret + `10y`+`2y`, data=test1)
summary(fit4)
test1 <- test1 %>%
  mutate(delta10y = `10y` - lag(`10y`))#change of 10yr yield
fit5<- lm(USDCAD_ret ~ TSXC_ret + `10y`+delta10y, data=test1,na.action=na.omit)
summary(fit5)
plot(test1$USDCAD_ret,test1$delta10y)
abline(fit5)
fit6<- lm(USDCAD_ret ~ TSXC_ret + `10y`*delta10y, data=test1,na.action=na.omit)
points(test1$delta10y[-1],fitted(fit5),col="green")
# plot(fit6)
summary(fit6)
# fit7<- lm(USDCAD_ret ~ TSXC_ret + delta10y*`10y`, data=test1,na.action=na.omit)
# summary(fit7)
# plot(fit7)
#all subset regression-search for minumum variables
stepAIC(fit4,direction = "backward")
leaps=regsubsets(USDCAD_ret~.,data=test1,nbest=2,method="exhaustive")
# plot(leaps,scale="adjr2")
# subsets(leaps,statistic="bic",ylim=c(-800,-600))
# subsets(leaps,statistic="cp",xlim=c(2,5),ylim=c(3,10))

##independent test----
#chi-square+Fisher+CMH only applied to categories variable 
#NULL Hypo: Independent:small p,reject,mean there is correlation
# data(package="vcd")
data("Arthritis",package="vcd")
Arthritis %>% names()     
attach(Arthritis)
xt=xtabs(~Age+Improved+Sex,data=Arthritis)
Age_Improved_chi=chisq.test(Age,Improved)
chi_mosaic=mosaic(
  Age_Improved_chi$observed,
  shade = TRUE,
  legend = FALSE,
  gp = shading_Friendly,
  
  direction = c("v", "h"),  # 关键：Age 纵向展开
  
  labeling_args = list(
    set_varnames = c(Age = "Age", Improved = "Outcome"),
    rot_labels = c(left = 0, top = 0),
    just_labels = c(left = "center", top = "center"),
    offset_varnames = c(left = 3, top = 3)
  ),
  
  main = "Mosaic Plot: Age vs Improvement (Chi-square Structure)"
)

Treatment_Improved_chi=chisq.test(Treatment,Improved)
Treatment_Improved_fisher=fisher.test(Treatment,Improved)
Age_Treatnebt_Improved_mantelhaen=mantelhaen.test(Age,Treatment,Improved)#under each categories of z,correlation b/t x and y
mantelhaen.test(xt)
detach()
#corr analysis
names(test1)
cr=cor(test1[-1,-1])
autoplot(cr)
cor.test(test1$USDCAD[-1], test1$`12mY`[-1])

corrplot(cr, method = "color", addCoef.col = "black" )
ggcorrplot(cr, lab = TRUE, hc.order = TRUE)
qgraph(cr, layout="spring", vsize=5)
# BiocManager::install("graph")
# library(ggm)
# colnames(test1)
# pcor(c(8,10,2,3,4,5,6)-1,cov(test1[-1,2:10]))
# S <- cov(test1[, 2:6], use="pairwise.complete.obs")
# 
# pcor_mat <- pcor(S)
# qgraph(pcor_mat, graph="pcor")

#partial correlation
# install.packages("corpcor")
library(corpcor)
S <- cov(test1[-1, 2:6], use = "pairwise.complete.obs")  # 协方差矩阵
pcor_mat <- corpcor::cor2pcor(S)    # 直接得到偏相关矩阵
# 对称化
pcor_mat <- (pcor_mat + t(pcor_mat)) / 2

# 对角线
diag(pcor_mat) <- 1

 qgraph(pcor_mat,
       graph = "pcor",
       layout = "spring",
       labels = colnames(test1)[2:6],
       edge.color = ifelse(pcor_mat > 0, "red", "blue"),
       edge.width = abs(pcor_mat)*2)  # 5 可调整比例
#ppcor library
library(ppcor)
ppcor_test=ppcor::pcor(test1[-1,-1]) 
ppcor_test_pcor=ppcor_test$estimate
ppcor_test_pcor_test=ppcor_test$p.value 
library(tidyverse)
library(reshape2)
library(igraph)
library(ggraph)
library(corrplot)

# 你的偏相关矩阵和 p-value
pcor_mat <- ppcor_test$estimate
pval_mat <- ppcor_test$p.value

# 1️⃣ 偏相关热力图（显著性过滤）
df_heat <- melt(pcor_mat, varnames = c("Var1","Var2"), value.name = "pcor") %>%
  left_join(
    melt(pval_mat, varnames = c("Var1","Var2"), value.name = "pval"),
    by = c("Var1","Var2")
  ) %>%
  mutate(pcor_sig = ifelse(pval < 0.05, pcor, NA))

pcor_ptest_image=ggplot(df_heat, aes(Var1, Var2, fill = pcor_sig)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Significant Partial Correlation Heatmap",
    subtitle = "Grey = not significant (p >= 0.05)",
    fill = "Partial Corr"
  )

# 2️⃣ 偏相关网络图-----
# -----------------------------
net_df <- df_heat %>%
  filter(Var1 != Var2, !is.na(pcor_sig))

g1<- graph_from_data_frame(
  net_df[, c("Var1","Var2","pcor_sig")],
  directed = FALSE
)

pcor_qgraph=ggraph(g1, layout = "fr") +
  geom_edge_link(aes(width = abs(pcor_sig), color = pcor_sig > 0), alpha = 0.8) +
  geom_node_point(size = 6, color = "black") +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_color_manual(
    values = c("TRUE" = "red", "FALSE" = "blue"),
    labels = c("Negative", "Positive")
  ) +
  scale_edge_width(range = c(0.3, 3)) +
  theme_void() +
  labs(
    title = "Partial Correlation Network",
    subtitle = "Edges = significant partial correlations",
    edge_color = "Sign"
  )

# 3️⃣ 偏相关 vs 普通相关对-------
cor_mat <- cor(test1[-1,-1], use = "pairwise.complete.obs")

df_compare <- melt(cor_mat, varnames = c("Var1","Var2"), value.name = "cor") %>%
  filter(Var1 != Var2) %>%
  left_join(
    melt(pcor_mat, varnames = c("Var1","Var2"), value.name = "pcor"),
    by = c("Var1","Var2")
  )

pcor_vs_corr=ggplot(df_compare, aes(cor, pcor)) +
  geom_point(alpha = 0.6, color = "#2c7fb8", size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Correlation vs Partial Correlation",
    subtitle = "Far from diagonal = spurious correlation removed by partial correlation",
    x = "Correlation",
    y = "Partial Correlation"
  )


# --------------------------------------
test2=test1[-1,-1]
test_cad_date=test1[-1,]
#independent test-continuous varaible 
cor.test(test1[[2]],test1[[3]])
library(psych)
tep=corr.test(test2)#calculate correlation, both value and p-value
tep$r %>% ggcorrplot(lab = T)
#t-test test for correlation/independent,测量两组变量是否同一个mean，就是sample是否是population的一部分
#类别vs数值,one sample test,
library(MASS)
# UScrime %>% head(10)
t.test(Prob~So,data=UScrime)#Welch t-test:non-parametric testing for each categories of So
attach(UScrime)
t.test(U1[1:20],U1[21:40],paired=T)#p too high ,cannot reject ho mean1=mean2
t.test(U1,mu=90)
t.test(U1,mu=90,alternative="greater")
t.test(U1,mu=90,alternative="greater",var.equal = T)
tt=t.test(U1,mu=95) #p too high ,can not reject Ho:mean=95

summary_df <- UScrime %>%
  group_by(So) %>%
  summarise(
    mean = mean(Prob),
    se   = sd(Prob)/sqrt(n()),
    n    = n()
  ) %>%
  mutate(
    lower = mean - qt(0.975, n-1) * se,
    upper = mean + qt(0.975, n-1) * se
  )

ggplot(summary_df, aes(x = factor(So), y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  labs(
    x = "So",
    y = "Mean Prob",
    title = "Mean Prob with 95% CI by So"
  ) +
  theme_minimal()

annot <- paste0(
  "t = ", round(tt$statistic,2),
  "\np = ", signif(tt$p.value,3)
)

ttest_result=ggplot(summary_df, aes(x = factor(So), y = mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  annotate("text", x = 1.5, y = max(summary_df$upper), label = annot) +
  theme_minimal()

boxplot(
  Prob ~ So,
  data = UScrime,
  xlab = "So",
  ylab = "Prob",
  main = "Prob by So"
)
box=recordPlot()

Violin=ggplot(UScrime, aes(x = factor(So), y = Prob)) +
  geom_violin(trim = FALSE, fill = "blue") +
  geom_boxplot(width = 0.1) +
  annotate("text", x = 1.5, y = max(summary_df$upper), label = annot) +
  labs(x = "So", y = "Prob", title = "Distribution of Prob by So") +
  theme_minimal()



#anova, acova,
##ANOVA----
#Anova applied to CATEGORY variable !!! to test difference among groups(>3,=2 use t.test)
# names(eco)
grep(toupper("United States"),names(x),value=T) %>% grep("Nonfar",x=.,value=T)
#make category variable vs continues variable
x <- as.data.table(readRDS(file="WIDE_ALLX"))
r <- as.data.table(r)

# 按 date 内连接，同时用反引号处理复杂列名
#method1
 merged_dt <- merge(
  x[, .(date, `UNITED STATES US Employees on Nonfarm Payrolls Total MoM Net Change SA`)],
  r[, .(date, `US OIS 10Y`)],
  by = "date",
  all = FALSE  # 内连接
)
 #method2
merged_dt <- inner_join(
  x %>% dplyr::select(date, `UNITED STATES US Employees on Nonfarm Payrolls Total MoM Net Change SA`),
  r %>% dplyr::select(date, `US OIS 10Y`),
  by = "date"
)
#method3
merged_dt <- x[r, 
               on = "date", 
               roll = Inf, 
               .(date, 
                 NFP = `UNITED STATES US Employees on Nonfarm Payrolls Total MoM Net Change SA`, 
                 `10y` = `US OIS 10Y`)]
merged_dt[, NFP := na.locf(NFP, na.rm = FALSE)]
merged_dt=na.omit(merged_dt)

# setnames(merged_dt,
#          old = c("UNITED STATES US Employees on Nonfarm Payrolls Total MoM Net Change SA", "US OIS 10Y"),
#          new = c("NFP", "10y"))

dt_plot <- na.omit(merged_dt) %>%
  mutate(y_rank = rank(`10y`) / n())

p_10y <- ggplot(dt_plot, aes(x = date, y = `10y`, color = y_rank)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1) +
  scale_color_gradientn(colors = rainbow(7)) +
  labs(
    title = "10Y Yield Over Time with Historical Distribution",
    x = "Date",
    y = "10Y Yield",
    color = "Percentile"
  ) +
  theme_minimal()

current_y <- tail(dt_plot$`10y`, 1)

p_10y2 <- p_10y+
  geom_hline(
    yintercept = current_y,
    linetype = "dashed",
    color = "black"
  )
library(ggExtra)

p_10y3 <- ggMarginal(
  p_10y2,
  type = "density",
  margins = "y",
  fill = "red",  # 这里直接设置颜色透明度
  alpha = 0.3                 # 透明度参数
)


dt <- na.omit(merged_dt)

p1 <- ggplot(dt, aes(x = date)) +
  geom_line(aes(y = `10y`), color = "blue", linewidth = 1) +
  geom_point(aes(y = `10y`), size = 1, color = "red") +
  labs(
    title = "10Y Yield",
    y = "10Y"
  ) +
  theme_minimal()

p2 <- ggplot(dt, aes(x = date)) +
  geom_col(aes(y = NFP, fill = NFP)) +
  scale_fill_gradient2(
    low = "red",
    mid = "black",
    high = "blue",
    midpoint = 0
  ) +
  labs(
    title = "Non-Farm Payrolls",
    x = "Date",
    y = "NFP"
  ) +
  theme_minimal()

p3 <- ggplot(dt, aes(x = date)) +
  geom_col(aes(y = NFP, fill = NFP)) +
  scale_fill_gradientn(colours = rainbow(7)) +
  labs(
    title = "Non-Farm Payrolls",
    x = "Date",
    y = "NFP"
  ) +
  scale_y_continuous(trans = "asinh")+
  theme_minimal()

NFP_10Y_chart=p1 / p3

merged_dt[, NFP_cat := cut(
  NFP,
  breaks = quantile(NFP, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Q1","Q2","Q3","Q4")
)]


# 2. 新增 10y 的变化列
merged_dt[, change_10y := `10y` - shift(`10y`)]

x_subset <- x[, .(date, `UNITED STATES Bloomberg Country Risk Economic Score for`)]

# 按 date 左连接到 merged_dt
merged_dt <- x_subset[merged_dt, on = "date"]
# na.omit(merged_dt)
setnames(merged_dt,old=c("UNITED STATES Bloomberg Country Risk Economic Score for"),new=c("Risk"))
merged_dt=na.locf(merged_dt) %>% na.omit()
merged_dt[, Risk_cat := cut(
  Risk,
  breaks = quantile(Risk, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Q1", "Q2", "Q3", "Q4")
)]

#single factor
afit <- aov(change_10y ~ NFP_cat, data = merged_dt[-1,])
summary(afit)

boxplot(
  change_10y ~ NFP_cat,
  data = merged_dt[-1,],
  xlab = "NFP Category",
  ylab = "Change in 10Y Yield",
  main = "10Y Yield Change by NFP Category"
)

ggplot(merged_dt, aes(x = NFP_cat, y = change_10y)) +
  geom_violin(fill = "lightgray") +
  geom_boxplot(width = 0.1) +
  theme_minimal()

change10y_NFP_cat_plot_violin=ggplot(merged_dt, aes(x = NFP_cat, y = change_10y, fill = NFP_cat)) +
  geom_violin(
    trim = FALSE,
    alpha = 0.75,
    color = "grey40"
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.shape = NA,
    fill = "white",
    alpha = 0.9
  ) +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

change10y_NFP_cat_plot_ridge=ggplot(merged_dt, aes(
  x = change_10y, 
  y = NFP_cat, 
  fill = after_stat(density)
)) +
  geom_density_ridges_gradient(
    scale = 2,           # 控制每条 ridge 高度
    rel_min_height = 0.01, # 去掉尾巴的极小值
    size = 0.3            # 轮廓线粗细
  ) +
  scale_fill_viridis_c(option = "C") + # 渐变色
  labs(
    x = "Change in 10Y Yield",
    y = "NFP Category",
    title = "Distribution of 10Y Yield Change by NFP Category"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    legend.position = "none"
  )

tuk <- TukeyHSD(afit)
tuk_plot=plot(tuk)
# ------------------------------------#
afit1 <- aov(`10y`~NFP_cat,data=merged_dt)
summary(afit1)
#two facor
afit2 <- aov(change_10y~NFP_cat+Risk_cat,data=merged_dt)
summary(afit2)
# plot(afit2)
# 用平均值 + 置信区间
df_summary1 <- merged_dt %>%
  group_by(NFP_cat, Risk_cat) %>%
  summarise(mean_change = mean(change_10y, na.rm = TRUE),
            se = sd(change_10y, na.rm = TRUE)/sqrt(n()), .groups="drop")

ggplot(df_summary1, aes(x = NFP_cat, y = mean_change, color = Risk_cat, group = Risk_cat)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = mean_change - se, ymax = mean_change + se), width = 0.2) +
  theme_minimal() +
  labs(title = "Interaction of NFP and Risk on change_10y",
       y = "Mean change_10y ± SE",
       x = "NFP Category") +
  scale_color_brewer(palette = "Set1")

emm <- emmeans(afit2, ~ NFP_cat * Risk_cat)
df_emm <- as.data.frame(emm)

NFP_RISK_Effect=ggplot(df_emm, aes(x = NFP_cat, y = emmean, color = Risk_cat, group = Risk_cat)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  theme_minimal() +
  labs(title = "Effect of NFP and Risk (ANOVA results)",
       y = "Predicted change_10y ± SE",
       x = "NFP Category") +
  scale_color_brewer(palette = "Set1")

lm(change_10y~NFP_cat+Risk_cat,data=merged_dt) %>% summary()
# aggregate(`10y`,by=list(Risk_cat,NFP_cat),FUN=mean)
# tolower(Risk_cat)
# merged_dt$NFP_cat=tolower(merged_dt$NFP_cat)
afit3 <- aov(`10y`~NFP_cat+Risk_cat,data=merged_dt)
summary(afit3)
attach(merged_dt)
sapply(list(Risk_cat,NFP_cat,`10y`),FUN=class)
afit4 <- aov(`10y`~NFP_cat*Risk_cat,data=merged_dt)
emmeans::emmeans(afit4,~NFP_cat|Risk_cat)
NFP_RISK_CAT_Interact=interaction2wt(`10y` ~ NFP_cat * Risk_cat,
               data = merged_dt,
               type = "b",
               col = c("red", "blue"),
               pch = c(16, 18),
               main = "Interaction of 10y by NFP_cat and Risk_cat")
NFP_RISK_CAT_Interact1=interaction.plot(
  x.factor = merged_dt$NFP_cat,
  trace.factor = merged_dt$Risk_cat,
  response = merged_dt$`10y`,
  type = "b",
  col = c("red", "blue", "green", "black"),
  pch = c(16, 18, 17, 15),
  main = "10y ~ NFP_cat by Risk_cat"
)
#acova----
#covariant:Risk,--Independent:NFP_cat, --Dependent:10y
#assumption:linear between Risk and 10y
acova_as1=ggplot(merged_dt, aes(Risk, `10y`, color=NFP_cat)) +
  geom_point(alpha=.3) +
  geom_smooth(method="lm") +
  theme_minimal()
#Assumption 2 — Homogeneity of Regression Slopes
#检查交互项是否显著
lm(`10y` ~ Risk * NFP_cat) %>% summary()# some p is too small, there is interaction
acova_as2=ggplot(merged_dt, aes(x = Risk, y = `10y`, color = NFP_cat)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Homogeneity of Regression Slopes Check",
    subtitle = "Parallel lines required for ANCOVA",
    x = "Risk",
    y = "10Y Yield Change"
  )
#Assumption3: Homogeneity of Variances
#each NFP_cat's variance on 10y remain same
leveneTest(`10y` ~ NFP_cat, data=merged_dt) # p too small, variance of 10y differ among NFP category
acova_as3=ggplot(merged_dt, aes(x = NFP_cat, y = `10y`, fill = NFP_cat)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.8) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Variance of 10Y Across NFP Regimes",
    subtitle = "Different spread = heteroskedasticity",
    y = "10Y Yield Change"
  )
#Assumption 4 — Independence of sample
#Assumption 5 — Normality of Residuals
fit_Norm_test <- lm(`10y` ~ Risk + NFP_cat, data=merged_dt)
shapiro.test(residuals(fit_Norm_test))
# W = 0.97456：Shapiro-Wilk 检验统计量，越接近 1 表示数据越接近正态分布。
# p-value < 2.2e-16：p 值非常小，远小于常用显著性水平（如 0.05）。
qqnorm(residuals(fit_Norm_test))
qqline(residuals(fit_Norm_test))
#then do ancova,在控制 Risk 后，不同 NFP_cat 的 10y“调整后均值”是否仍然不同。
HH::ancova(`10y`~NFP_cat+Risk,data=merged_dt)
ancova1_plot=recordPlot()
ancova1=HH::ancova(`10y`~NFP_cat+Risk,data=merged_dt)
ancova2=aov(`10y`~NFP_cat+Risk,data=merged_dt)

emmacova <- emmeans(ancova1, ~ NFP_cat)

acova_mean=plot(emmacova) +
  labs(
    title = "Adjusted Mean of 10Y by NFP Regime",
    subtitle = "Controlling for Risk",
    y = "Adjusted 10Y"
  )
# 在相同风险情绪下，不同 NFP 状态对应的利率水平系统性不同
# → NFP 不只是通过 risk channel 影响 10Y

raw_means <- merged_dt %>%
  group_by(NFP_cat) %>%
  summarise(raw_mean = mean(`10y`, na.rm = TRUE))

adj_means <- as.data.frame(emmacova)

acova_mean_adjustedmean=ggplot() +
  geom_point(data = raw_means,
             aes(NFP_cat, raw_mean),
             color = "grey40", size = 3) +
  geom_point(data = adj_means,
             aes(NFP_cat, emmean),
             color = "red", size = 4) +
  geom_errorbar(data = adj_means,
                aes(NFP_cat,
                    ymin = lower.CL,
                    ymax = upper.CL),
                width = 0.15, color = "red") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Raw vs Risk-Adjusted Mean of 10Y",
    subtitle = "Grey = raw mean, Red = adjusted mean"
  )
#灰点差不大，红点明显分层
# ✅ 正的结构效应被 Risk 掩盖了

#Manova----- 
#在 (10y, Risk) 这个“联合空间”里，不同 NFP regime 的中心点是否不同
attach(merged_dt)
mfit1=manova(cbind(`10y`,Risk)~NFP_cat)
summary.aov(mfit1)
manova_dist=merged_dt %>%
  ggplot(aes(x = Risk, y = `10y`, color = NFP_cat)) +
  stat_ellipse(type = "norm", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Joint Distribution of (Risk, 10Y) by NFP Regime",
    subtitle = "Ellipses show group-wise covariance structure"
  )
#在所有 NFP_cat 两两组合中，哪些组的 10y 均值“显著不同”
TukeyHSD(aov(`10y` ~ NFP_cat, data=merged_dt)) %>% plot()
TukeyHSD(aov(Risk ~ NFP_cat, data=merged_dt)) %>% plot()
tukey_df <- TukeyHSD(aov(`10y` ~ NFP_cat, data=merged_dt))$NFP_cat %>%
  as.data.frame() %>%
  tibble::rownames_to_column("comparison") %>%
  mutate(
    comparison = factor(comparison,
                        levels = comparison[order(diff)])
  )

manova_Tukey=ggplot(tukey_df, aes(x = diff, y = comparison)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    x = "Mean difference in 10y",
    y = "NFP regime comparison",
    title = "Tukey HSD: Pairwise Differences Across NFP Regimes"
  )
#outlier
#每一个观测点，离“多变量均值中心”有多远
d=mahalanobis(test2,colMeans(test2),cov(test2))
#是否符合 D2∼χk2​
chi <- qchisq(ppoints(length(d)), df=ncol(test2))
cutoff <- qchisq(0.975, df = ncol(test2))
#超过这个值的概率只有 2.5%

ord <- order(d)
d_sorted <- d[ord]
chi_sorted <- chi[ord]
plot(chi_sorted, d_sorted,
     xlab = "Theoretical Chi-square Quantiles",
     ylab = "Mahalanobis Distances",
     main = "Mahalanobis QQ Plot with Outliers")
abline(0,1, col="red", lwd=2)
outliers <- which(d_sorted > cutoff)
points(chi_sorted[outliers], d_sorted[outliers], col="blue", pch=19)
text(chi_sorted[outliers], d_sorted[outliers], labels=ord[outliers], pos=4, cex=0.7, col="blue")

md_df <- data.frame(
  chi = chi_sorted,
  md  = d_sorted,
  index = ord
)

md_df$outlier <- md_df$md > cutoff

manova_MQQplot=ggplot(md_df, aes(x = chi, y = md)) +
  geom_point(aes(color = outlier), alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_hline(yintercept = cutoff, linetype = "dotted") +
  scale_color_manual(values = c("grey60", "blue")) +
  theme_minimal() +
  labs(
    x = expression("Theoretical " * chi^2 * " Quantiles"),
    y = "Mahalanobis Distance",
    title = "Mahalanobis QQ Plot for Multivariate Outlier Detection",
    color = "Outlier"
  )

# sample_idx <- sample(nrow(test2), 500)  # 抽 500 个样本
# mshapiro.test(t(as.matrix(test2[sample_idx, ])))#too many singular 

#Variance Homogeneity
library(biotools)
#多元版 Levene’s Test → 就是 Box’s M Test
boxM(merged_dt[, .(`10y`,Risk)], merged_dt$NFP_cat)


###Generalized Linear Model----
#for assumption being not normal
#possion distrtion: dependent~possion distribution:某段时间或空间内，随机事件发生的次数
#Yi​∼Poisson(λi​)
#assumption: 1,var=mean,2,gamma is constant,3 independent
library(robust)
data(breslow.dat,package="robust")
pfit=glm(sumY~Base+Age+Trt,data=breslow.dat,family = poisson(link="log"))
summary(pfit)
exp(coef(pfit)) # interpretation, each increases one unit-each increase expectation y happened time
#assumption verification-residual
par(mfrow=c(2,2))

# (1) Raw residuals vs Fitted
plot(fitted(pfit), residuals(pfit), 
     xlab="Fitted values", ylab="Raw residuals",
     main="Residuals vs Fitted")
abline(h=0, col="red")
#没有明显趋势（随机散布在 0 附近）
# 如果呈现“漏斗形/曲线” → 可能 非线性或方差不均

# (2) Pearson residuals vs Fitted
pearson_res <- residuals(pfit, type="pearson")
plot(fitted(pfit), pearson_res,
     xlab="Fitted values", ylab="Pearson residuals",
     main="Pearson Residuals vs Fitted")
abline(h=0, col="red")
#残差随机散布在 0 附近

# (3) QQ plot of deviance residuals
dev_res <- residuals(pfit, type="deviance")
qqnorm(dev_res, main="QQ Plot of Deviance Residuals")
qqline(dev_res, col="red")
# 检查正态近似（Poisson 本身不要求残差正态，但 deviance 残差在样本大时近似正态）
# 如果点接近红线 → 没有严重异常

# (4) Cook’s distance
plot(cooks.distance(pfit), type="h",
     main="Cook’s Distance", ylab="Cook's D")
abline(h = 4/length(cooks.distance(pfit)), col="red", lty=2)
# 检测 高杠杆/高影响力点
# Cook’s D > 4/n → 可能影响模型显著
# 在计数数据中，高值可能是：
# 极端计数+罕见组合的自变量

possion_plot=recordPlot()
par(mfrow=c(1,1))

#2. 检查是否过度离散（Overdispersion）
dispersion <- sum(pearson_res^2) / pfit$df.residual
dispersion>1.5 #not possion, maybe negative binormal dist
#plot original vs fitted
# 预测值（期望值，不是样本）
breslow.dat$predicted <- fitted(pfit)

# 排序（不排序也可）
breslow.dat$order_id <- 1:nrow(breslow.dat)

plot(breslow.dat$order_id, breslow.dat$sumY,
     type="p", col="blue",
     xlab="Sample Index",
     ylab="Y (Actual vs Predicted)",
     main="Actual vs Predicted Counts")

lines(breslow.dat$order_id, breslow.dat$predicted,
      col="red", lwd=2)

legend("topleft",
       legend=c("Actual Y", "Predicted Y"),
       col=c("blue","red"),
       pch=c(1, NA),
       lty=c(NA,1),
       lwd=c(1,2))

possion_predict_plot=recordPlot()
# 蓝点 = 实际计数
# 红线 = 模型预测值（期望 λ）

breslow.dat$upper <- breslow.dat$predicted + sqrt(breslow.dat$predicted)
breslow.dat$lower <- breslow.dat$predicted - sqrt(breslow.dat$predicted)

possion_predict_plot_1=ggplot(breslow.dat, aes(x = order_id)) +
  geom_point(aes(y = sumY), color = "blue", alpha = 0.6) +
  geom_line(aes(y = predicted), color = "red", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, fill = "darkgreen") +
  labs(title = "Actual vs Predicted Counts with Poisson SD") +
  theme_minimal()

###logistic regression-----
#data Prepare:
test3 <- merge(test1, m[, .(date, CAD_ON = `CAD ON`)], 
               by = "date", 
               all.x = TRUE)
test3=test3[,CAD_ON:=na.locf(CAD_ON,na.rm=F)]
usdcad_xts <- xts(test3$USDCAD, order.by = test3$date)
TSX60_xts <- xts(test3$TSX60, order.by = test3$date)
# test3[,USDCAD_Month:=as.numeric(periodReturn(usdcad_xts,period="monthly"))]
# test3[,TSX60_Month:=as.numeric(periodReturn(TSX60_xts,period="monthly"))]
usdcad_monthly_ret <- periodReturn(usdcad_xts, period = "monthly", type = "log")
usdcad_monthly_dt <- data.table(
  date = index(usdcad_monthly_ret),
  USDCAD_monthly_ret = coredata(usdcad_monthly_ret)
)
test4 <- merge(test3, usdcad_monthly_dt, by = "date", all.x = TRUE) %>% na.locf(na.rm = FALSE) %>% na.omit()
setnames(test4, old = "USDCAD_monthly_ret.monthly.returns", new = "USDCAD_monthly")
test4[, USDCAD_monthly_bin := ifelse(USDCAD_monthly > 0, 1, 0)]
test4[, USDCAD_30d := log(USDCAD / shift(USDCAD, 30))]
test4$USDCAD_monthly.x=NULL
test4$USDCAD_monthly.y=NULL
test4$USDCAD_monthly_bin=NULL
test4$USDCAD_30d
test4[, USDCAD_M_B := factor(ifelse(USDCAD_30d > 0, 1, 0),
                             levels = c(0, 1))] %>% na.omit()#make monthly to be bin
test5=na.omit(test4)

#fit to test USDCAD 月度方向（涨 / 不涨）logistic model
logfit <- glm(
  USDCAD_M_B ~ USDCAD + TSXC + delta10y + CAD_ON,
  data = test5,
  family = binomial
)
summary(logfit)
logistic_prob=exp(coef(logfit))#USDCAD 每上升 1 个单位，odds 变成原来的 12 倍
test5[, prob := predict(logfit, type = "response")]
test5[, pred := ifelse(prob > 0.5, 1, 0)]
#result analysis
#model signifcant p 很小 → 模型整体显著
# summary(logfit) H0​:β1​=β2​=⋯=0
logistic_pchi=with(summary(logfit), 
     pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#Hosmer-lemeshow 预测 vs 实际是否一致,但很容易否定，不能完全参考
library(ResourceSelection)
y_numeric <- as.numeric(as.character(test5$USDCAD_M_B))
hoslem_test=hoslem.test(y_numeric, fitted(logfit), g = 10)#p too small not good
#模型拟合优度
#伪R
library(pscl)
pR2(logfit) #19% too small McFadden
#log_likehood
logLik(logfit)
#混淆矩阵----
table(Predicted = test5$pred, Actual = test5$USDCAD_M_B)
confusion_matrix=ggplot(
  as.data.frame(table(Predicted = test5$pred,
                      Actual   = test5$USDCAD_M_B)),
  aes(x = Actual, y = Predicted, fill = Freq)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Confusion Matrix",
    x = "Actual",
    y = "Predicted"
  ) +
  theme_minimal(base_size = 14)
confusion_matrix_ratio=ggplot(
  transform(
    as.data.frame(table(Predicted = test5$pred,
                        Actual   = test5$USDCAD_M_B)),
    Prop = ave(Freq, Actual, FUN = function(x) x / sum(x))
  ),
  aes(x = Actual, y = Predicted, fill = Prop)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Prop, 2)), size = 5) +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(
    title = "Confusion Matrix (Proportion by Actual)",
    x = "Actual",
    y = "Predicted"
  ) +
  theme_minimal(base_size = 14)
#ROC----理想模型 → 曲线尽量靠左上角----
library(pROC)
roc_obj <- roc(test5$USDCAD_M_B, test5$prob)
roc_obj$auc      # AUC 模型“猜对顺序”的概率,bigger, the better
plot(roc_obj)      # ROC 曲线
roc_df <- data.frame(
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)
ROC_Plot=ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  
  # 随机猜测线
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", color = "grey50") +
  
  # 坐标 & 标题
  labs(
    title = "ROC Curve — USDCAD Direction Model",
    subtitle = paste0("AUC = ", round(roc_obj$auc, 3),
                      "  (模型区分 1 vs 0 的概率)"),
    x = "False Positive Rate\n(把 0 错判成 1 的比例)",
    y = "True Positive Rate / Recall\n(把 1 抓住的比例)"
  ) +
  
  # 图内解释
  annotate(
    "text", x = 0.65, y = 0.55,
    label = "虚线 = 随机猜测",
    color = "grey40"
  ) +
  annotate(
    "text", x = 0.15, y = 0.85,
    label = "越靠左上角\n= 少犯错就能多抓机会",
    hjust = 0
  ) +
  annotate(
    "text", x = 0.35, y = 0.7,
    label = "阈值降低 → 更激进\nRecall ↑ 但误报 ↑",
    hjust = 0
  ) +
  
  coord_equal() +
  theme_minimal(base_size = 14)





#k-fold
# 设置 5-fold 交叉验证
set.seed(123)  # 保证可重复
ctrl <- trainControl(
  method = "cv",       # cross-validation
  number = 5,          # k = 5
  classProbs = TRUE,   # 计算概率
  summaryFunction = twoClassSummary  # 可以计算AUC
)

# 因为caret要求因子型标签为 "yes"/"no" 或 "1"/"0"
test5$USDCAD_M_B <- factor(test5$USDCAD_M_B, levels = c(0,1), labels = c("No","Yes"))

logfit_cv <- train(
  USDCAD_M_B ~ USDCAD + TSXC + delta10y + CAD_ON,
  data = test5,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"   # 用AUC作为评估指标
)

# 查看结果
print(logfit_cv)
Kfold_roc=ggplot(logfit_cv$resample, aes(x = Resample, y = ROC)) +
  geom_bar(stat="identity", fill="skyblue") +
  geom_hline(yintercept = mean(logfit_cv$resample$ROC), color="red", linetype="dashed") +
  labs(title="5-Fold CV ROC per Fold", y="AUC", x="Fold") +
  theme_minimal()

Kfold_Sensitivity=ggplot(logfit_cv$resample, aes(x=Resample, y=Sens)) +
  geom_bar(stat="identity", fill="orange") +
  geom_hline(yintercept = mean(logfit_cv$resample$Sens), color="red", linetype="dashed") +
  labs(title="Sensitivity per Fold", y="Sensitivity", x="Fold") +
  theme_minimal()

###power analysis----
#sample size
#type II error- type I error
#power analysis for lm
LMPower=pwr.f2.test(u=3,sig.level = 0.05,power=0.9,f2=0.0769)
LMPower$v
#power analysis for anova one way
ANOVAPower=pwr.anova.test(k=2,f=0.25,sig.level = 0.05,power=0.9)
ANOVAPower$n

###Time Series----
#data cleaning
delta_ts <- ts(test5$delta10y,
               start = c(as.numeric(format(min(test5$date), "%Y")),
                         as.numeric(format(min(test5$date), "%j"))), 
               end = c(as.numeric(format(max(test5$date), "%Y")),
                         as.numeric(format(max(test5$date), "%j"))), 
               frequency = 365)  # 日频率
CAD10Y_ts <- ts(test5$`10y`,
                start = c(as.numeric(format(min(test5$date), "%Y")),
                          as.numeric(format(min(test5$date), "%j"))), 
                end = c(as.numeric(format(max(test5$date), "%Y")),
                        as.numeric(format(max(test5$date), "%j"))), 
                frequency = 365)  # 日频率
# delta_ts %>% time()
start(delta_ts)  # 起始周期
end(delta_ts)    # 结束周期
# delta_ts[1:10]       # 前 10 个观测值
# delta_ts["2012.002"] # 可以用 time() 对应的小数索引（不太直观，通常用下标）
delta_subset <- window(delta_ts, start=c(2012, 32), end=c(2012, 74))
plot(delta_ts, main="delta10y Time Series", ylab="delta10y", xlab="Time", col="blue")
abline(h=mean(delta_ts, na.rm=TRUE), col="red", lty=2)
#diff
delta_diff <- diff(delta_ts, differences = 1)# could be use for garch model
plot(delta_diff, main="First Difference of delta10y")
ndiffs(delta_ts)#estimate the number of differences for stationary
#moving average
plot(SMA(delta_ts,50))#simple moving average
#look seasonal
delta_stl <- stl(delta_ts, s.window=50)        # s.window = 平滑窗口
TS_STL=plot(delta_stl,main="Observed=Trend+Seasonal+Remainder")#Observed=Trend+Seasonal+Remainder

# by month
delta_zoo <- zoo(test5$delta10y, order.by = test5$date)#change into ts 
delta_monthly <- aggregate(delta_zoo, as.yearmon, mean)  # 每月平均
# 转成 ts
delta_monthly_ts <- ts(delta_monthly, start=c(2010,1), frequency=12)
# monthplot
Delta10Y_Monthly_plot=monthplot(delta_monthly_ts, main="Monthly Plot of delta10y", ylab="delta10y")
plot.ts(delta_diff)
abline(h=mean(delta_diff, na.rm=TRUE), col="red", lty=2)
#ADF test stationary
ADF_Delta10y=adf.test(delta_ts)
adf.test(delta_diff)
#Unit Root test:HO:non-stationary
unitrootTest(delta_ts)
#纯随机性：H0:white noise
Delta_White_noise_test=Box.test(delta_ts,type="Ljung-Box",lag=6)
#SARIMA(P,D,Q)(p,d,q)----
#ACF-for MA(q),#PACF-for AR(p)
par(mfrow=c(1,2))
acf(delta_diff, main="ACF of delta10y (diff)")
pacf(delta_diff, main="PACF of delta10y (diff)")
par(mfrow=c(1,1))

#fit model!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
tsarimafit=arima(delta_ts,order=c(4,1,2)) #after find pdq
summary(tsarimafit)
# 预测未来 30 天
fc <- predict(tsarimafit, n.ahead = 30)
pred_ts <- ts(fc$pred,
              start = end(delta_ts) + 1/frequency(delta_ts),
              frequency = frequency(delta_ts))
plot.ts(pred_ts)
# 取 ts 内部时间
ts_t <- time(pred_ts)   # 年 + 小数

# 年份和小数部分
year <- floor(ts_t)
frac <- ts_t - year

# 转成 numeric
day_of_year <- round(as.numeric(frac) * 365)

# 转成实际日期
pred_dates <- as.Date(paste0(year, "-01-01")) + day_of_year

# 和预测值组合
pred_df <- data.frame(date = pred_dates,
                      pred = as.numeric(pred_ts))

head(pred_df)
# plot(pred_df,type="l")
#result:BIC,AIC: the smaller, the better
# AIC
AIC(tsarimafit)

# BIC / Schwarz criterion
BIC(tsarimafit)
#residual normality
qqnorm(tsarimafit$residuals)
qqline(tsarimafit$residuals)
#residual autocorrelation H0:white noise,residual自相关系数为0
Box.test(tsarimafit$residuals,type="Ljung-Box") 
#predict with confidence
forecast(tsarimafit,300) %>% plot()
fit_values <- fitted(tsarimafit)
plot(delta_ts, col="blue", lty=1, xlab="Time", ylab="delta10y", main="ARIMA Fit vs Original")
lines(fit_values, col="red", lty=2)
legend("topleft", legend=c("Original","ARIMA Fit"), col=c("blue","red"), lty=c(1,2))

#auto.arima---------
tsfit1=auto.arima(delta_ts)
summary(tsfit1)
AIC(tsarimafit,tsfit1)

AutoArima_plot1=ggplot() +
  geom_line(aes(x = time(delta_ts), y = delta_ts), color = "black") +
  geom_line(aes(x = time(delta_ts), y = fitted(tsfit1)), color = "steelblue") +
  labs(
    title = "ARIMA Fit on delta_ts",
    x = "Time",
    y = "delta10y",
    caption = paste("AIC:", round(AIC(tsfit1),2))
  ) +
  theme_minimal()
#test assumption
#test residual dist
AutoArima_residual_plot=autoplot(tsfit1$residuals) +
  labs(title = "Residuals of ARIMA Fit", y = "Residuals") +
  theme_minimal()
#test whether there is auto-correlation/residual is white noise
AutoArimaResidual_ACF=ggAcf(tsfit1$residuals) + labs(title = "ACF of Residuals")
AutoArimaResidual_PACF=ggPacf(tsfit1$residuals) + labs(title = "PACF of Residuals")
#test residual independent (white noise)
AutoArima_residual_box_test=Box.test(tsfit1$residuals, lag=20, type="Ljung-Box")
#test residual normal dist
qqnorm(tsfit1$residuals)
qqline(tsfit1$residuals, col="red")
#test staionariy of residual
adf.test(tsfit1$residuals)

library(FinTS)
###ARCH+GARCH model----
ArchTest(tsfit1$residuals)
#εt​=rt​−r^t​
#assume:εt​=σt​zt​,zt​∼N(0,1)
#ARCH(q): σt2​=α0​+α1​εt−12​+⋯+αq​εt−q----
#rt​=ARIMA+ARCH
e2 <- tsfit1$residuals^2
ggAcf(e2) + labs(title = "ACF of Squared Residuals")#ACF 拖很长尾
ggPacf(e2) + labs(title = "PACF of Squared Residuals")#主要看pacf确定q，ACF 拖很长尾用 GARCH，而不是高阶 ARCH
#ARCH1 for residuals
library(rugarch)
spec_arch1 <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 0)   # (p=1, q=0) = ARCH(1)
  ),
  mean.model = list(
    armaOrder = c(0, 0), # has already done ARIMA
    include.mean = FALSE  # residual 均值为 0
  ),
  distribution.model = "norm"
)
# fit ARCH1
fit_arch1 <- ugarchfit(
  spec = spec_arch1,
  data = tsfit1$residuals
)

show(fit_arch1)
# omega：长期基础波动
# alpha1：昨天的 shock 对今天波动的影响强度
infocriteria(fit_arch1)#log likelyhood
ARCHCoef=coef(fit_arch1)
ARCHCoef_S4=fit_arch1@fit$matcoef
# sigma(fit_arch1)
residuals(fit_arch1, standardize = TRUE) %>% plot()
#test the residual of residual
ArchTest(residuals(fit_arch1, standardize = TRUE))


#GARCH(p,q)今天的波动，既来自昨天的冲击，也来自昨天的波动状态本身
spec_garch11 <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = FALSE
  ),
  distribution.model = "std"   # 金融数据强烈建议
)

fit_garch11 <- ugarchfit(
  spec = spec_garch11,
  data = tsfit1$residuals
)
ArchTest(residuals(fit_garch11, standardize = TRUE))
fit_garch11@fit$matcoef

### incorporation----
###CAD10Y_ts → ARIMA（均值） → 残差诊断 → GARCH（波动） → 预测 → 同图展示----
Original_TS_Plot=autoplot(CAD10Y_ts) +
  labs(
    title = "Original Time Series: CAD 10Y",
    y = "CAD10Y_ts"
  ) +
  theme_minimal()
#rt​=μt​+εt​,μt​=ARIMA(rt−1​,rt−2​,…)
fit_arima <- auto.arima(CAD10Y_ts)
fit_arima <- arima(CAD10Y_ts,order=c(5,1,2))
summary(fit_arima)
#Arima plot
dt_fit <- data.table(
  time   = time(CAD10Y_ts),
  actual = as.numeric(CAD10Y_ts),
  fitted = as.numeric(fitted(fit_arima))
)

CAD10Y_ARIMA_Compare=ggplot(dt_fit, aes(time)) +
  geom_line(aes(y = actual), color = "red") +
  geom_line(aes(y = fitted), color = "steelblue") +
  labs(
    title = "ARIMA Fit on CAD 10Y",
    subtitle = paste("Model:", arimaorder(fit_arima)),
    y = "delta_ts"
  ) +
  theme_minimal()
#test the residual
resid_arima <- residuals(fit_arima)
CAD10Y_res_ACF=ggAcf(resid_arima) + labs(title = "ACF of ARIMA Residuals")
CAD10Y_res_PACF=ggPacf(resid_arima) + labs(title = "PACF of ARIMA Residuals")
BOX_WhiteNoise_CAD10Y_Res=Box.test(resid_arima, lag = 20, type = "Ljung-Box")
ArchTest(resid_arima)
#there is ARCH effect in residuals, e^2 ACF 拖尾 ⇒ GARCH，而不是高阶 ARCH
e2 <- resid_arima^2
ggAcf(e2)  + labs(title = "ACF of Squared Residuals")
ggPacf(e2) + labs(title = "PACF of Squared Residuals")
#Garch
spec_garch11 <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),  # 均值已由 ARIMA 建模
    include.mean = FALSE
  ),
  distribution.model = "std"
)

fit_garch <- ugarchfit(
  spec = spec_garch11,
  data = resid_arima
)
show(fit_garch)
#test GARCH model residuals
z_t <- residuals(fit_garch, standardize = TRUE)
ArchTest(z_t)
ggAcf(z_t^2) + labs(title = "ACF of Squared Standardized Residuals")
#Show result
horizen=200
fc_arima <- forecast(fit_arima, h = horizen, level = 95)
#Arima range
dt_arima_fc <- data.table(
  time  = time(fc_arima$mean),
  mean  = as.numeric(fc_arima$mean),
  lower = as.numeric(fc_arima$lower),
  upper = as.numeric(fc_arima$upper),
  model = "ARIMA"
)
#garch range
garch_fc <- ugarchforecast(
  fit_garch,
  n.ahead = horizen
)

sigma_fc <- as.numeric(sigma(garch_fc))
nu <- coef(fit_garch)["shape"]
q  <- qt(0.975, df = nu)
#ARIMA+GARCH
dt_garch_fc <- data.table(
  time  = time(fc_arima$mean),
  mean  = as.numeric(fc_arima$mean),
  lower = as.numeric(fc_arima$mean) - q * sigma_fc,
  upper = as.numeric(fc_arima$mean) + q * sigma_fc,
  model = "ARIMA + GARCH"
)
#final plot
dt_hist <- data.table(
  time  = time(CAD10Y_ts),
  value = as.numeric(CAD10Y_ts)
)

dt_fc <- rbind(dt_arima_fc, dt_garch_fc)

Arima_Garch_Auto_Plot=ggplot() +
  geom_line(
    data = dt_hist,
    aes(time, value),
    color = "black"
  ) +
  geom_line(
    data = dt_fc,
    aes(time, mean, color = model),
    linewidth = 1
  ) +
  geom_ribbon(
    data = dt_fc,
    aes(time, ymin = lower, ymax = upper, fill = model),
    alpha = 0.25
  ) +
  labs(
    title = "From ARIMA to ARIMA–GARCH: Full Modeling Pipeline",
    subtitle = "Mean unchanged, volatility dynamically modeled",
    y = "CAD 10Y"
  ) +
  theme_minimal()




###EFA-----Xi​=λi1​F1​+λi2​F2​+⋯+λik​Fk​+εi​----
#find effective factor given bunch of factor这些看起来相关的变量，背后是不是被少数几个看不见的共同因子驱动的
names(test4)
num_vars <- c("USDCAD", "TSXC", "delta10y", "CAD_ON", "USDCAD_ret", "TSXC_ret", "12mY")
efa_data <- test4[, ..num_vars]
#check correlation matrix
cor_matrix <- cor(efa_data, use="pairwise.complete.obs")
par(mfrow=c(1,1))
EFA_Corr=corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.cex = 0.9,
  addCoef.col = "black",
  number.cex = 0.7,
  col = colorRampPalette(c("#2c7bb6", "white", "#d7191c"))(200)
)

#KMO 和 Bartlett 检验
kmo_res <- KMO(cor_matrix) #简单相关vs偏相关
print(kmo_res) #>0.6 can do factor
bartlett_res <- cortest.bartlett(cor_matrix, n=nrow(efa_data))#H₀：相关矩阵 = 单位矩阵（变量不相关）
print(bartlett_res)#p-value < 0.05，拒绝原假设 → 变量相关，可以做 EFA
#Scree plot to select number of factor
ParallelAnalysis_plot=fa.parallel(efa_data, fm="ml", fa="fa")#真实数据的 eigenvalue vs「纯随机数据」的 eigenvalue 对比
#执行因子分析 + 旋转
efa_raw <- fa(efa_data, nfactors=3, rotate="none", fm="ml")
print(efa_raw$loadings, cutoff=0.05)#因子 j 对变量 i 的解释强度
efa_raw$communality#each变量有多少比例的方差 was explained by EF,too low not fit
efa_raw$Vaccounted#每个因子解释的 共同方差比例
efa_raw$scores#「不可观测因子」as time series,most important
#旋转-OBLIMIN 允许因子相关,risk-on / rates / vol 本来就相关
efa_oblimin <- fa(efa_data, nfactors=3, rotate="oblimin", fm="ml")
print(efa_oblimin$loadings, cutoff=0.05)
# 查看每个变量对总方差的解释
efa_oblimin$communality
efa_oblimin$Vaccounted
#旋转-varimax 正交，回归中 multicollinearity 小，强行假设因子不相关
efa_varimax <- fa(efa_data, nfactors=3, rotate="varimax", fm="ml")

# 1. 干净地取出 loading（关键！）
loadings_df <- as.data.frame(unclass(efa_varimax$loadings))

# 2. 保留变量名
loadings_df$Variable <- rownames(loadings_df)

# 3. 转 long format
loadings_long <- melt(
  loadings_df,
  id.vars = "Variable",
  variable.name = "Factor",
  value.name = "Loading"
)

# 4. 画 heatmap
EFA_Loading_Coef_Plot=ggplot(loadings_long, aes(x = Factor, y = Variable, fill = Loading)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#2c7bb6",
    mid = "white",
    high = "#d7191c",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  geom_text(aes(label = round(Loading, 2)), size = 3) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Factor Loadings (Varimax Rotation)",
    x = "Factor",
    y = ""
  )


# 查看每个变量对总方差的解释
efa_varimax$communality
efa_varimax$Vaccounted
#compare
efa_list <- list(Raw=efa_raw$loadings,
                 Oblimin=efa_oblimin$loadings,
                 Varimax=efa_varimax$loadings)

# 可以打印对比或画热图
loading_long <- do.call(
  rbind,
  lapply(names(efa_list), function(m){
    tmp <- as.data.frame(unclass(efa_list[[m]]))
    tmp$Variable <- rownames(tmp)
    tmp$Model <- m
    tmp
  })
)

loading_long <- melt(
  loading_long,
  id.vars = c("Variable","Model"),
  variable.name = "Factor",
  value.name = "Loading"
)

EFA_Loading_Factor_Compare=ggplot(
  subset(loading_long, abs(Loading) >= 0.3),
  aes(x = Factor, y = Variable, fill = Loading)
) +
  geom_tile(color = "white") +
  facet_wrap(~ Model) +
  scale_fill_gradient2(
    low = "#2c7bb6", mid = "white", high = "#d7191c",
    midpoint = 0, limits = c(-1,1)
  ) +
  geom_text(aes(label = round(Loading,2)), size = 3) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Factor Loadings Across EFA Specifications (|loading| ≥ 0.3)",
    x = "Factor",
    y = ""
  )

#base on EFA, do analysis
scores <- efa_raw$scores
#sanity check 因子 ≈ 原变量的线性组合
cor(cbind(efa_data, scores)) %>% corrplot(method="color", addCoef.col="black", number.cex=0.7)
cor_mat <- cor(cbind(efa_data, scores), use="pairwise.complete.obs")
EFA_Factor_Score_CORR=corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.cex = 0.8,
  addCoef.col = "black",
  number.cex = 0.6,
  col = colorRampPalette(c("#2c7bb6","white","#d7191c"))(200)
)
str(efa_varimax$loadings)
head(unclass(efa_varimax$loadings))

#regression:delta10y 是不是由「少数几个宏因子」驱动？
lm(delta10y ~ ML1 + ML2 + ML3, data = cbind(efa_data, scores)) %>% summary()
data = cbind(efa_data, scores,binary=test4$USDCAD_M_B) %>% na.omit()
data$binary <- as.numeric(as.character(data$binary))

lm_df <- tidy(
  lm(delta10y ~ ML1 + ML2 + ML3, data = cbind(efa_data, scores)),
  conf.int = TRUE
)

EFA_D10_Factor=ggplot(lm_df[-1,], aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "#1b9e77") +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Effect of Macro Factors on Δ10Y",
    x = "",
    y = "Coefficient (95% CI)"
  )
#把因子当 状态变量+用于 direction / regime 判断
logit_df <- tidy(
  glm(binary ~ ML1 + ML2 + ML3, family = binomial, data=data),
  conf.int = TRUE,
  exponentiate = TRUE
)
#因子上升 1 个单位时，事件发生的“相对可能性”
EFA_D10_Binary_Logistic_plot=ggplot(logit_df[-1,], aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "#d95f02") +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1
  ) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Factor-Based Directional Model (Odds Ratios)",
    x = "",
    y = "Odds Ratio"
  )
#fitted vs original all based on non-rotated factor analysis
model_raw <- lm(delta10y ~ USDCAD + TSXC+ CAD_ON + USDCAD_ret + TSXC_ret + `12mY`, data=efa_data)
model_factor <- lm(delta10y ~ ML1 + ML2 + ML3, data=cbind(efa_data, scores))
df_efa <- cbind(efa_data, scores)

#check if factor has time series:
df_long <- reshape2::melt(
  df_efa,
  measure.vars = c("ML1","ML2","ML3"),
  variable.name = "Factor",
  value.name = "Score"
)

EFA_Factor_TS_plot=ggplot(df_long, aes(x = seq_along(Score), y = Score, color = Factor)) +
  geom_line(linewidth = 0.8) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Factor Scores Over Time",
    x = "Time",
    y = "Standardized Factor Score"
  ) +
  scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))

library(Metrics)
# Raw model summary, vs variables
summary(model_raw)
AIC(model_raw); BIC(model_raw)
rmse_raw <- rmse(df_efa$delta10y, fitted(model_raw))

# Factor model summary vs factor
summary(model_factor)
AIC(model_factor); BIC(model_factor)
rmse_factor <- rmse(df_efa$delta10y, fitted(model_factor))
#plot two model different parameter
perf_df <- data.frame(
  Model = c("Raw Variables", "Factor Scores"),
  R2 = c(summary(model_raw)$r.squared,
         summary(model_factor)$r.squared),
  Adj_R2 = c(summary(model_raw)$adj.r.squared,
             summary(model_factor)$adj.r.squared),
  AIC = c(AIC(model_raw), AIC(model_factor)),
  BIC = c(BIC(model_raw), BIC(model_factor)),
  RMSE = c(rmse_raw, rmse_factor)
)

perf_long <- perf_df %>%
  pivot_longer(
    -Model,
    names_to = "Metric",
    values_to = "Value"
  )

EFA_Compare_Statstic=ggplot(perf_long, aes(x = Model, y = Value, fill = Model)) +
  geom_col(width = 0.6, alpha = 0.8) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 3) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  ) +
  labs(
    title = "Model Performance Comparison: Raw Variables vs Factor Scores",
    x = "",
    y = ""
  )


#在控制其他 factor 后，每一个 latent factor 对 delta10y 的边际影响
coef_df <- tidy(model_factor, conf.int = TRUE)
EFA_Factor_Estimate_plot=ggplot(coef_df[-1, ], aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "#2c7bb6") +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.1, linewidth = 0.8
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Factor Model Coefficients (95% CI)",
    x = "",
    y = "Estimate"
  )
#plot comparision
plot(df_efa$delta10y, type="l", lwd=2, 
     main="Actual vs Predicted (Raw vs Factor Models)", 
     ylab="delta10y")

lines(fitted(model_raw), col="blue", lwd=2)
lines(fitted(model_factor), col="red", lwd=2)

legend("topleft", legend=c("Actual","Raw Model","Factor Model"),
       col=c("black","blue","red"), lwd=2, bty="n")

#version2
df_plot <- data.frame(
  Actual = df_efa$delta10y,
  Raw = fitted(model_raw),
  Factor = fitted(model_factor),
  t = seq_len(nrow(df_efa))
)

df_long2 <- melt(df_plot, id.vars = "t")

EFA_Compare_plot=ggplot(df_long2, aes(x = t, y = value, color = variable)) +
  geom_line(linewidth = 0.8,alpha=0.5) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Actual vs Fitted:  USE Raw Variables vs Factor to Fit Model,",
    x = "Time",
    y = "delta10y"
  ) +
  scale_color_manual(
    values = c("red", "#d95f02", "#1b9e77"),
    labels = c("Actual", "Factor Model", "Raw Model")
  )


#scatter
par(mfrow=c(1,2))
plot(fitted(model_raw), df_efa$delta10y,
     main="Raw Model: Fitted vs Actual",
     xlab="Fitted", ylab="Actual",
     pch=16, col=rgb(0,0,1,0.5))
abline(0,1,col="red",lwd=2)

plot(fitted(model_factor), df_efa$delta10y,
     main="Factor Model: Fitted vs Actual",
     xlab="Fitted", ylab="Actual",
     pch=16, col=rgb(1,0,0,0.5))
abline(0,1,col="blue",lwd=2)

Raw_Fit_Actual_plot_EFA=ggplot(df_plot, aes(x = Raw, y = Actual)) +
  geom_point(alpha = 0.5, color = "#d95f02") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 12) +
  labs(title = "Raw Model: Fitted vs Actual")

Factor_Fit_Actual_plot_EFA=ggplot(df_plot, aes(x = Factor, y = Actual)) +
  geom_point(alpha = 0.5, color = "#1b9e77") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 12) +
  labs(title = "Factor Model: Fitted vs Actual")

###VAR test----
#所有变量在同一时间频率、同一时间轴上
var_data <- df_efa[, c("delta10y","ML1","ML2","ML3")] #df is result after EFA
var_data <- na.omit(var_data)
#stationary test:所有变量必须是平稳的（I(0)）
adf_results <- sapply(var_data, function(x) adf.test(x)$p.value)#p too big
var_data_diff <- diff(as.matrix(var_data))
#select lag
lag_select <- VARselect(var_data_diff, lag.max=10, type="const")
lag_select$selection
#fit
p=2 #should be lag_select
#Δ10yt​=α+β11​Δ10yt−1​+β12​ML1t−1​+β13​ML2t−1​+β14​ML3t−1​+β21​Δ10yt−2​+⋯+εt​
var_model <- VAR(var_data_diff, p=p, type="const")
summary(var_model)
# var_model$datamat %>% view()
#granger test: ML2 的过去值，是否能提高对 delta10y 的预测能力
grangertest(var_data_diff[,1] ~ var_data_diff[,2], order=2)#var_data_diff was converted to matrix
grangertest(var_data_diff[,1] ~ var_data_diff[,3], order=2)
grangertest(var_data_diff[,1] ~ var_data_diff[,4], order=2)
causality(var_model, cause = "ML2")#H0：ML2 不 Granger-cause 其他变量
var_names <- colnames(var_data_diff)
gc_res <- data.frame()

for (v in var_names) {
  test <- causality(var_model, cause = v)
  
  gc_res <- rbind(
    gc_res,
    data.frame(
      cause = v,
      p.value = test$Granger$p.value
    )
  )
}


gc_res <- gc_res %>%
  mutate(
    sig = p.value < 0.05,
    strength = -log10(p.value)
  ) %>%
  arrange(desc(strength))

VAR_Granger_Strength=ggplot(gc_res,
       aes(x = reorder(cause, strength),
           y = strength,
           fill = sig)) +
  geom_col(width = 0.7, color = "grey30") +
  geom_hline(yintercept = -log10(0.05),
             linetype = "dashed",
             linewidth = 0.8,
             color = "firebrick") +
  coord_flip() +
  scale_fill_manual(
    values = c("grey80", "steelblue"),
    labels = c("Not significant", "p < 0.05")
  ) +
  labs(
    title = "Granger Causality Strength",
    subtitle = "Information contribution to the VAR system",
    x = NULL,
    y = expression(-log[10](p.value)),
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )

#Granger matrix
library(igraph)
library(ggraph)
library(lmtest)

var_names <- colnames(var_data_diff)
k <- ncol(var_data_diff)

gc_mat <- matrix(
  0,
  nrow = k,
  ncol = k,
  dimnames = list(var_names, var_names)
)

for (i in 1:k) {
  for (j in 1:k) {
    if (i != j) {
      gt <- grangertest(
        var_data_diff[, j] ~ var_data_diff[, i],
        order = 2
      )
      
      gc_mat[i, j] <- -log10(gt$`Pr(>F)`[2])
    }
  }
}

gc_mat[gc_mat < -log10(0.05)] <- 0

g <- graph_from_adjacency_matrix(
  gc_mat,
  mode = "directed",
  diag = FALSE
)

V(g)$outdeg <- degree(g, mode = "out")
V(g)$indeg  <- degree(g, mode = "in")
VAR_Granger_Network=ggraph(g, layout = "fr") +
  geom_edge_link(
    arrow = arrow(length = unit(5, "mm")),
    end_cap = circle(4, "mm"),
    linewidth = 1.2,
    alpha = 0.7,
    color = "steelblue"
  ) +
  geom_node_point(
    aes(
      size = outdeg,
      color = outdeg > indeg
    )
  ) +
  geom_node_text(
    aes(label = name),
    size = 5,
    fontface = "bold",
    repel = TRUE
  ) +
  scale_color_manual(
    values = c("grey60", "firebrick")
  ) +
  scale_size(range = c(5, 12)) +
  labs(
    title = "Granger Causality Network",
    subtitle = "Node size = information outflow (driver strength)"
  ) +
  theme_void()



#IRF-Impulse Response Function
#给 ML2 一个 1-sd 的“意外冲击”，看 delta10y 未来怎么走
irf_res2 <- irf(
  var_model,
  impulse = c("ML1","ML2","ML3"),
  response = "delta10y",
  n.ahead = 10,
  boot = TRUE
)
irf_df_ci <- do.call(rbind, lapply(names(irf_res2$irf), function(imp) {
  data.frame(
    h = 0:(nrow(irf_res2$irf[[imp]]) - 1),
    irf = irf_res2$irf[[imp]][,1],
    lower = irf_res2$Lower[[imp]][,1],
    upper = irf_res2$Upper[[imp]][,1],
    impulse = imp
  )
}))
IRF_EFA_Impulse_Plot=ggplot(irf_df_ci, aes(h, irf, color = impulse, fill = impulse)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "IRF with CI: ML shocks → Δ10Y",
    x = "Horizon",
    y = "Response"
  ) +
  theme_minimal()


#FEVD（方差分解）Forecast Error Variance Decomposition
fevd_res <- fevd(var_model, n.ahead=20)
#在第 h 期预测误差中,有多少 % 来自：自己,ML1 / ML2 / ML3 的 shock
# dev.new(width = 10, height = 8)
# plot(fevd_res)#% of variance error contributed by each impulse
# 
# cols <- brewer.pal(3, "Set1")  # 3个冲击变量颜色
# 画 delta10y 的 FEVD
# plot(fevd_res$delta10y, col=cols, main="FEVD of delta10y", xlab="Horizon", ylab="Variance Contribution")
# legend("topright", legend=names(fevd_res$delta10y), col=cols, lty=1, cex=0.8)
#version2
fevd_df <- as.data.frame(fevd_res$delta10y)
fevd_df$h <- 1:nrow(fevd_df)

fevd_long <- reshape2::melt(fevd_df, id.vars="h")
while (!is.null(dev.list())) dev.off()
FEVD_VAR_Decomp=ggplot(fevd_long, aes(h, value, fill=variable)) +
  geom_area(alpha=0.8) +
  scale_y_continuous(labels=scales::percent) +
  labs(
    title="FEVD of Δ10Y",
    x="Horizon",
    y="Variance Contribution"
  ) +
  theme_minimal()


###PCA----
#data clean from test2
head(test_cad_date)
test_cad_date=na.omit(test_cad_date)
ToAdd=c(grep("CANADIAN",names(c),value=T)[c(4,10)],"date")
z=c[,..ToAdd]
result <- merge(
  test_cad_date,
  c[, ..ToAdd],
  by = "date",
  all.x = TRUE     # 保留 test2 的所有行
)
setnames(result,
         old = tail(names(result), 2),
         new = c("NonCom_Long", "OpenInterest"))
#linear interpolate weekly data
result[, NonCom_Long := na.approx(NonCom_Long, date, rule = 2)]
result[, OpenInterest := na.approx(OpenInterest, date, rule = 2)]
#pca calculation
num_cols <- names(result)[sapply(result, is.numeric)]
pca_data <- result[, ..num_cols]  # 提取所有 numeric 列
pca_res <- prcomp(pca_data, scale. = TRUE) 
pca_res_all=prcomp(pca_data, scale. = TRUE) 
summary(pca_res)
#scree plot
explained_var <- pca_res$sdev^2 / sum(pca_res$sdev^2)
qplot(x = 1:length(explained_var), y = explained_var) +
  geom_line() + geom_point() +
  xlab("Principal Component") +
  ylab("Proportion of Variance") +
  ggtitle("Screen Plot")
library(factoextra)
PCA_Scree_Plot=fviz_eig(pca_res,addlabels = T)

#pca loading
loadings <- pca_res$rotation
print(loadings)
#pca used as new variable
pca_res$x %>% head()
#plot
autoplot(pca_res)
biplot(pca_res,scale=0)
#fviz_pca_biplot(pca_res,repel = T,col.var="blue") too many sample
pca.var=pca_res$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100,1)
barplot(pca.var.per,main="scree Plot",xlab = "PCA",ylab = "% variation")

# Version2 for CAD data
# scores（样本）
scores <- as.data.frame(pca_res$x[, 1:2])
scores$date <- result$date
# loadings（变量）
loadings <- as.data.frame(pca_res$rotation[, 1:2])
loadings$var <- rownames(loadings)

PCA_Biplot=ggplot() +
  # 样本点（时间）
  geom_point(
    data = scores,
    aes(x = PC1, y = PC2),
    alpha = 0.3,
    color = "steelblue"
  ) +
  
  # 坐标轴
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
  
  # 变量箭头
  geom_segment(
    data = loadings,
    aes(x = 0, y = 0, xend = PC1*4, yend = PC2*4),
    arrow = arrow(length = unit(0.02, "npc")),
    color = "red"
  ) +
  
  # 变量名字
  geom_text(
    data = loadings,
    aes(x = PC1*4.3, y = PC2*4.3, label = var),
    color = "red",
    size = 4
  ) +
  
  theme_bw() +
  labs(
    title = "PCA Biplot for test_cad_date",
    subtitle = "PC1 vs PC2",
    x = "PC1",
    y = "PC2"
  )
# 蓝点表示每日市场状态在因子空间中的位置
# 红色箭头表示原始变量对各主成分的贡献方向和强度
# 箭头方向相近的变量高度相关
# 箭头方向相反的变量呈负相关

#rolling pca for regime----
# 假设 result 已经按 date 排序
setorder(result, date)

window_size <- 60  # 60 天滚动窗口
dates <- result$date

rolling_pc1 <- rep(NA, nrow(result))
rolling_pc2 <- rep(NA, nrow(result))

num_cols <- names(result)[sapply(result, is.numeric)]

for(i in window_size:nrow(result)){
  window_data <- result[(i-window_size+1):i, ..num_cols]
  
  # scale 每个窗口
  pca_res <- prcomp(window_data, scale. = TRUE)
  
  # 取最后一天的 scores
  rolling_pc1[i] <- pca_res$x[window_size, 1]
  rolling_pc2[i] <- pca_res$x[window_size, 2]
}

rolling_df <- data.table(
  date = dates,
  PC1 = rolling_pc1,
  PC2 = rolling_pc2
)

Rolling_PCA=ggplot(rolling_df, aes(x=date)) +
  geom_line(aes(y=PC1, color="PC1")) +
  geom_line(aes(y=PC2, color="PC2")) +
  labs(title="Rolling PC1,PC2 Scores for regime identification", y="Score", x="Date") +
  theme_bw()

#PC1 大于均值+1σ为 regime A，小于均值-1σ为 regime B
rolling_df[, regime := fifelse(
  PC1 > mean(PC1, na.rm=TRUE)+sd(PC1, na.rm=TRUE), "A",
  fifelse(PC1 < mean(PC1, na.rm=TRUE)-sd(PC1, na.rm=TRUE), "B", "Neutral")
)]

PC1_Regime_Change=ggplot(rolling_df, aes(x=date, y=PC1, color=regime, group=regime)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title="PC1-based Regime Classification (Lines by Regime)")

#PCA-projection non rolling-----
library(data.table)
scores_all <- data.table(
  date = result$date,
  PC1_all = pca_res_all$x[,1],
  PC2_all = pca_res_all$x[,2]
)

# Regime 分类（基于 PC1_all）
scores_all[, regime_all := fifelse(
  PC1_all > mean(PC1_all, na.rm=TRUE) + sd(PC1_all, na.rm=TRUE), "High",
  fifelse(PC1_all < mean(PC1_all, na.rm=TRUE) - sd(PC1_all, na.rm=TRUE), "Low", "Neutral")
)]

loadings_all <- as.data.table(pca_res_all$rotation[,1:2])
setnames(loadings_all, old = c("PC1", "PC2"), new = c("PC1_all", "PC2_all"))
loadings_all[, var_all := rownames(pca_res_all$rotation)]

library(ggplot2)
library(grid)  # arrow unit

PCA_Biplot2 <- ggplot() +
  # 样本点
  geom_point(data = scores_all, 
             aes(x = PC1_all, y = PC2_all, color = regime_all), 
             size = 2.5, alpha = 0.8) +
  
  # 每个 regime 的椭圆，带阴影填充
  stat_ellipse(data = scores_all,
               aes(x = PC1_all, y = PC2_all, fill = regime_all, color = regime_all),
               level = 0.68, geom = "polygon", alpha = 0.2, size = 1.2) +
  
  # 箭头表示变量 loadings
  geom_segment(data = loadings_all,
               aes(x = 0, y = 0, xend = PC1_all*4, yend = PC2_all*4),
               arrow = arrow(length = unit(0.02, "npc")),
               color = "red", size = 1) +
  
  geom_text(data = loadings_all,
            aes(x = PC1_all*4.3, y = PC2_all*4.3, label = var_all),
            color = "red", size = 4, fontface = "bold") +
  
  # 坐标轴参考线
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  
  # 颜色和填充
  scale_color_manual(values = c("High" = "#E41A1C", "Neutral" = "#4DAF4A", "Low" = "#377EB8")) +
  scale_fill_manual(values = c("High" = "#E41A1C", "Neutral" = "#4DAF4A", "Low" = "#377EB8")) +
  
  theme_minimal(base_size = 14) +
  labs(title = "Full-sample PCA Biplot for test_cad_date",
       subtitle = "Samples colored by PC1-based regime",
       x = "PC1_all",
       y = "PC2_all",
       color = "Regime_all",
       fill = "Regime_all") +
  theme(legend.position = "top")

#pca loading plot
PCA_Loading_plot=ggplot(loadings_all, aes(x = PC1_all, y = PC2_all)) +
  geom_segment(aes(x = 0, y = 0, xend = PC1_all*4, yend = PC2_all*4),
               arrow = arrow(length = unit(0.02, "npc")), color = "darkred", size=1) +
  geom_text(aes(x = PC1_all*4.3, y = PC2_all*4.3, label = var_all),
            color="darkred", fontface="bold") +
  theme_minimal() +
  labs(title="PCA Loadings Plot", x="PC1", y="PC2")






#typical IRIS data plot
pc=prcomp(iris[-5],center=T,scale=T)
library(ggbiplot)
ggbiplot(pc,obs.scale=1,var.scale=1,groups=iris$Species,ellipse=T,circle=T,ellipse.prob=0.68)
fviz_pca_ind(pc,col.ind=iris$Species,addEllipses = T,geom=("point"))
fviz_pca_biplot(pc,repel = T,col.var="blue")
biplot(pc,scale=0)
#USArrest data
head(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)
pr.out$center #平均值
pr.out$scale #sigma
pr.out$rotation # loading of each pca
pr.out$x # new varianble  using PCA
dim(pr.out$x)
biplot(pr.out, scale=0)# 看哪些sample离得近，以及每个sample偏向哪个变量

library(ggforce)
plotdat <- as.data.frame(pr.out$x[,1:2])
plotdat$state <- rownames(plotdat)
rotdat <- as.data.frame(pr.out$rotation[,1:2])
rotdat$crime <- rownames(rotdat)
ggplot() + geom_text(data = plotdat, aes(x = PC1, y = PC2, label = state),size = 3) +
  theme_bw() + theme(panel.grid.major=element_line(colour=NA), panel.grid.minor = element_blank()) +
  geom_hline(aes(yintercept = 0), colour="gray88", linetype="dashed") + 
  geom_vline(aes(xintercept = 0), colour="gray88", linetype="dashed") +
  scale_y_continuous(sec.axis = sec_axis(~./4)) + scale_x_continuous(sec.axis = sec_axis(~./4)) +
  geom_segment(data = rotdat,aes(x=0, xend= PC1*4, y=0, yend= PC2*4), arrow = arrow(length = unit(0.03, "npc")), colour = 'red') +
  geom_text(data = rotdat,aes(x = PC1*4.4, y = PC2*4.4, label = crime), size = 4, colour = 'red')

#PCA doesn't fit for time series as there are HUGE DEPENDENT relationship


###Granger Causality----
# attach(df)
#原假设 (H0)：ML2 的过去两期（lag 1 和 lag 2）对 delta10y 没有预测力
#p small <- can predict
# lmtest::grangertest(delta10y ~ ML2, order = 2)
###Bayes Naive vs Guassian--------
#Bayes scenario EV calculation----
dt <- copy(test_cad_date)
# 次日USDCAD收益（预测目标）
dt[, usdcad_next := shift(USDCAD_ret, type = "lead")]
# 去掉最后一天（没有next）
dt <- dt[!is.na(usdcad_next)]
dt[, scenario :=
     fifelse(TSXC_ret < -0.005 & delta10y <= 0, "RiskOff",
             fifelse(TSXC_ret >  0.005 & delta10y >= 0, "RiskOn",
                     "Neutral"))]
#group aggregation
fx_stats <- dt[, .(
  mu = mean(usdcad_next, na.rm=TRUE),
  sigma = sd(usdcad_next, na.rm=TRUE),
  n = .N
), by = scenario]
#prior distribution
dist_stats <- dt[, .(
  tsx_mu = mean(TSXC_ret),
  tsx_sd = sd(TSXC_ret),
  rate_mu = mean(delta10y),
  rate_sd = sd(delta10y),
  prior = .N / nrow(dt)   # 情景先验概率
), by = scenario]

dist_stats

#calculate posterio
calc_posterior <- function(tsx, rate, dist_stats){
  
  tmp <- copy(dist_stats)
  
  # Likelihood: 正态密度（Naive Bayes连续版）
  tmp[, likelihood :=
        dnorm(tsx, tsx_mu, tsx_sd) *
        dnorm(rate, rate_mu, rate_sd)]
  
  # 未归一化 posterior
  tmp[, post_raw := likelihood * prior]
  
  # 归一化
  tmp[, posterior := post_raw / sum(post_raw)]
  
  tmp[, .(scenario, posterior)]
}
#calculate each scenario prob based on posterior
posterior_list <- dt[, calc_posterior(TSXC_ret, delta10y, dist_stats),
                     by = date]
#select the biggest scenario
pred_scn <- posterior_list[
  , .SD[which.max(posterior)], 
  by = date
][, .(date, pred_scenario = scenario)]
#merge with original defined scenario
scn_compare <- merge(
  dt[, .(date, true_scenario = scenario)],
  pred_scn,
  by="date"
)
# correct ratio
scn_compare[, correct := pred_scenario == true_scenario]

mean(scn_compare$correct)
#confusion matrix
table(
  Predicted = scn_compare$pred_scenario,
  Actual    = scn_compare$true_scenario
)

#calculate EV
posterior_ev <- merge(
  posterior_list,
  fx_stats[, .(scenario, mu)],
  by = "scenario",
  all.x = TRUE
)
ev_daily <- posterior_ev[, .(
  EV = sum(posterior * mu)
), by = date]

head(ev_daily)
#trading Signal
ev_daily[, signal :=
           fifelse(EV > 0.00001,  1,
                   fifelse(EV < -0.00001, -1, 0))]
#backtest
bt <- merge(
  ev_daily,
  dt[, .(date, usdcad_next)],
  by="date"
)

bt[, pnl := signal * usdcad_next*1000]
mean(bt$pnl, na.rm=TRUE)
sd(bt$pnl, na.rm=TRUE)

#posteior prob
ggplot(posterior_list,
       aes(date, posterior, color=scenario)) +
  geom_line() +
  labs(title="Posterior Probability of Macro Scenarios")
#stack
ggplot(posterior_list,
       aes(x=date, y=posterior, fill=scenario)) +
  geom_area(position="stack", alpha=0.7) +
  labs(title="Posterior Probability of Macro Scenarios",
       y="Probability")
scn_cols <- c(
  RiskOff = "#E41A1C",   # 鲜红
  Neutral = "#FFD92F",   # 亮黄
  RiskOn  = "#1F78B4"    # 深蓝
)
top_scn <- posterior_list[
  , .SD[which.max(posterior)],
  by=date
][, .(date, dominant=scenario)]


Bayer_Posteior_scenary=ggplot() +
  
  # 主图：posterior stack
  geom_area(
    data = posterior_list,
    aes(date, posterior, fill=scenario),
    position="stack",
    alpha=0.9
  ) +
  
  # 顶部：dominant regime色带（非常薄）
  geom_tile(
    data = top_scn,
    aes(date, y=-0.02, fill=dominant),
    height=0.04
  ) +
  
  scale_fill_manual(values=scn_cols) +
  
  coord_cartesian(ylim=c(-0.05,1.06), expand=FALSE) +
  
  labs(
    title="Posterior Macro Scenario Probability",
    y="Probability",
    x=NULL
  ) +
  
  theme_minimal() +
  theme(
    legend.position="top",
    panel.grid.minor=element_blank()
  )


#EV vs real change
plot_dt <- merge(ev_daily, dt[, .(date, usdcad_next)], by="date")

# ggplot(plot_dt, aes(date)) +
#   geom_line(aes(y=EV*100, color="Expected")) +
#   geom_line(aes(y=usdcad_next*100, color="Realized")) +
#   labs(title="Expected vs Realized USDCAD Return (%)")
#scatter
Bayers_Scatter=ggplot(plot_dt, aes(EV, usdcad_next)) +
  geom_point(alpha=0.4) +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Expected vs Realized Return",
       x="Expected Return",
       y="Realized Return")
#only dir
plot_dt[, correct := sign(EV)==sign(usdcad_next)]

# ggplot(plot_dt, aes(date, correct)) +
#   geom_step() +
#   labs(title="Directional Accuracy Over Time")
#cumulative
plot_dt[, cum_real := cumsum(usdcad_next)]
plot_dt[, cum_ev   := cumsum(EV)]

Bayers_Cum_return=ggplot(plot_dt, aes(date)) +
  geom_line(aes(y=cum_real, color="Realized")) +
  geom_line(aes(y=cum_ev, color="Expected")) +
  labs(title="Cumulative Expected vs Realized FX Return")


#FX return by scenario
# ggplot(dt, aes(scenario, usdcad_next)) +
#   geom_boxplot() +
#   labs(title="USDCAD Next-Day Return by Scenario")

# ggplot(dt,
#        aes(usdcad_next, fill=scenario, color=scenario)) +
#   geom_density(alpha=0.25) +
#   labs(title="Distribution of Next-Day USDCAD Return by Scenario",
#        x="USDCAD next-day return")


Bayers_Kernal_ret=ggplot(dt, aes(x = usdcad_next, fill = scenario)) +
  
  # 核密度
  geom_density(alpha = 0.4, color = NA) +
  
  # 底部散点（抖动模拟 rug）
  geom_jitter(aes(y = 0),   # y=0 贴底
              height = 5,   # 垂直不抖动
              width = 0.005, # 横向微抖动
              alpha = 0.4,
              size = 1.5,
              color = "black") +
  
  # 叠加箱线图（竖着 box）
  geom_boxplot(aes(y = 50),  # 向下偏移，避免覆盖密度
               width = 10,     # 竖直方向压扁
               alpha = 0.8,
               color = "black",
               fill = "yellow",
               outlier.size = 1) +
  
  # 分 facet
  facet_wrap(~scenario, ncol = 1, scales = "free") +
  
  labs(
    title = "Scenario-wise FX Return Distribution with Sample Points and Boxplot",
    x = "USDCAD Next Day Return",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")



#dashboard
final_table <- Reduce(function(x,y) merge(x,y,by="date",all=TRUE),
                      list(
                        dcast(posterior_list, date~scenario, value.var="posterior"),
                        ev_daily,
                        dt[,.(date, usdcad_next)]
                      ))

head(final_table)


