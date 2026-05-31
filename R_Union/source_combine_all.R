setwd("G:/我的云端硬盘/R_Union")

###ECO_Screener----
ecoenv=new.env()
ecoenv$date="2023-10-10"
source("ECO_Screener.R",local=ecoenv)
ecoenv$p_bar %>% print()
ecoenv$DMscorep %>% print()
par(mfrow = c(ecoenv$nrow, ecoenv$ncol), mar = c(1, 2, 2, 1))
# pal <- colorRampPalette(
#   brewer.pal(9, "Blues")   # 或 "Greens", "Purples", "YlGnBu"
# )(ecoenv$n)
# 
# pal <- rainbow(
#   ecoenv$n,
#   s = 0.6,   # ↓ 降低饱和度（最关键）
#   v = 0.8  # ↓ 降低亮度，避免荧光感
# )
pal <- viridis(
  ecoenv$n,
  option = "C",   # "C" = inferno / magma 中间那种力量感
  begin = 0.1,
  end   = 0.9
)

for(i in 1:ecoenv$n){
  main_col <- pal[i]
  country_name <- rownames(ecoenv$result)[i]
  vals_scaled <- as.numeric(ecoenv$result_scaled[i, ])
  
  radar_df <- ecoenv$make_radar_df(vals_scaled)
  
  # 画雷达图
  radarchart(
    radar_df,
    axistype = 0,                       # 不显示刻度数字
    pcol = main_col,                    # 多边形边框颜色
    pfcol = adjustcolor(main_col, 0.5), # 半透明填充
    plwd = 1.5,                         # 边框线宽
    cglcol = "grey85",                  # 网格颜色
    cglty = 1,
    cglwd = 0.6,
    vlcex = 0.9                         # 指标文字大小
  )
  
  title(country_name, cex.main = 1.3, font.main = 2)
}

###FX_Screenr----
fxenv=new.env()
fxenv$TS="2024-10-22"
pdf(NULL)
source("FXScreener.R",local=fxenv)
dev.off()
# all graph
#DMFXBubble,p_dark,FXZfacet,FXCorr,
#heatmap repost:matem,matemZ,mat,matz,
#EMFX:EMFXZRadial,EMFXZRadial_FACET,EMFXFacet
#pcarry: carry vs spot return
fxenv$EMFXZRadial_FACET
fxenv$EMFXZRadial
fxenv$EMFXFacet
fxenv$DMFXBubble
fxenv$p_dark
fxenv$pcarry
fxenv$FXZfacet #DMFX Z socre plot
fxenv$FXCorr
par(mfrow=c(1,1))
corrplot::corrplot(fxenv$cormat, method = "ellipse",addCoef.col = "black")
#repost heatmap
heatmap(t(fxenv$mat), Rowv = NA, Colv = NA, col = heat.colors(256))
heatmap(t(fxenv$matz), Rowv = NA, Colv = NA, col = heat.colors(256))
heatmap(fxenv$matem, Rowv = NA, Colv = NA, col = fxenv$hm_col,scale = "column")
heatmap(fxenv$matemZ, Rowv = NA, Colv = NA, col = fxenv$hm_col,scale = "column")

###YieldCurve----

renv=new.env()
renv$date1="2025-06-10"
renv$curvenames_test="CHF SARON OIS" #select from r
source("YieldCurve.R",local=renv)
dev.off()
names(renv$r)

#image:NSFitted,p_left,p_right,final_plot
renv$p_left
renv$p_right
renv$final_plot
#heatmap:carry_heat_rainbow

#function in Yield Curve
#plot_NS_curve
renv$curvenames
renv$start
renv$plot_NS_curve(renv$curvenames[103],start=renv$start)# zero rate
renv$NSHelp %>% cat()

#plot_NS_from_r 
renv$plot_NS_from_r(keyword = "NOK NOWA OIS",date1=renv$date1,start=renv$start)
names(renv$r)#ois curve
#plot
renv$NSFitted

###DataScience----
DSenv=new.env()
source("DataScience.R",local=DSenv)

#Regression
Caddata=DSenv$test1

#independent test
