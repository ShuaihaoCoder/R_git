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
library(plotly)

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

###Initial fx group----
DMFX <- c("EUR","JPY","GBP","CAD","AUD","NZD","NOK","CHF","SGD")
EMFX <- c("CNH","HKD","ZAR","TRY","MXN")
NDF <- c("KRW","TWD","INR","IDR","PHP","CNY")
setDT(f)
setDT(v)
# TS="2024-09-20" to be defined at source_combine
# f[which(f$date==TS),c("EURUSD","AUDUSD")]

###define variable----
#data wrangling
rp=252 #return peirod
f$EUR_ret <- (f$EURUSD-(dplyr::lag(f$`EURUSD 12m forward point`,rp)/10000))/ dplyr::lag(f$EURUSD, rp) - 1 
#f$EUR_ret=scales::percent(f$EUR_ret,accuracy = 0.1)
f$EUR3M <- ((f$`EURUSD 3m forward point`/(10000*f$EURUSD))*4) #%>% scales::percent(accuracy = 0.01)
f$EUR6M <- ((f$`EURUSD 6m forward point`/(10000*f$EURUSD))*2) #%>% scales::percent(accuracy = 0.01)
f$EUR12M <- ((f$`EURUSD 12m forward point`/(10000*f$EURUSD))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, EURV1M=`EURUSDV1M Curncy`,EURV3M=`EURUSDV3M Curncy`,EURV6M=`EURUSDV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, EURV1MP=`EURUSDV1M Curncy`-`EURUSDH1M Curncy`,EURV3MP=`EURUSDV3M Curncy`-`EURUSDH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, EURR3MR=-`EURUSD25R3M Curncy`)], by = "date", all.x = TRUE)

f$AUD_ret <- (f$AUDUSD-(dplyr::lag(f$`AUDUSD 12m forward point`,rp)/10000))/ dplyr::lag(f$AUDUSD, rp) - 1 
#f$AUD_ret=scales::percent(f$AUD_ret,accuracy = 0.1)
f$AUD3M <- ((f$`AUDUSD 3m forward point`/(10000*f$AUDUSD))*4) #%>% scales::percent(accuracy = 0.01)
f$AUD6M <- ((f$`AUDUSD 6m forward point`/(10000*f$AUDUSD))*2) #%>% scales::percent(accuracy = 0.01)
f$AUD12M <- ((f$`AUDUSD 12m forward point`/(10000*f$AUDUSD))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, AUDV1M=`AUDUSDV1M Curncy`,AUDV3M=`AUDUSDV3M Curncy`,AUDV6M=`AUDUSDV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, AUDV1MP=`AUDUSDV1M Curncy`-`AUDUSDH1M Curncy`,AUDV3MP=`AUDUSDV3M Curncy`-`AUDUSDH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, AUDR3MR=-`AUDUSD25R3M Curncy`)], by = "date", all.x = TRUE)

f$GBP_ret <- (f$GBPUSD-(dplyr::lag(f$`GBPUSD 12m forward point`,rp)/10000))/ dplyr::lag(f$GBPUSD, rp) - 1 
#f$GBP_ret=scales::percent(f$GBP_ret,accuracy = 0.1)
f$GBP3M <- ((f$`GBPUSD 3m forward point`/(10000*f$GBPUSD))*4) #%>% scales::percent(accuracy = 0.01)
f$GBP6M <- ((f$`GBPUSD 6m forward point`/(10000*f$GBPUSD))*2) #%>% scales::percent(accuracy = 0.01)
f$GBP12M <- ((f$`GBPUSD 12m forward point`/(10000*f$GBPUSD))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, GBPV1M=`GBPUSDV1M Curncy`,GBPV3M=`GBPUSDV3M Curncy`,GBPV6M=`GBPUSDV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, GBPV1MP=`GBPUSDV1M Curncy`-`GBPUSDH1M Curncy`,GBPV3MP=`GBPUSDV3M Curncy`-`GBPUSDH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, GBPR3MR=-`GBPUSD25R3M Curncy`)], by = "date", all.x = TRUE)

f$NZD_ret <- (f$NZDUSD-(dplyr::lag(f$`NZDUSD 12m forward point`,rp)/10000))/ dplyr::lag(f$NZDUSD, rp) - 1 
#f$NZD_ret=scales::percent(f$NZD_ret,accuracy = 0.1)
f$NZD3M <- ((f$`NZDUSD 3m forward point`/(10000*f$NZDUSD))*4) #%>% scales::percent(accuracy = 0.01)
f$NZD6M <- ((f$`NZDUSD 6m forward point`/(10000*f$NZDUSD))*2) #%>% scales::percent(accuracy = 0.01)
f$NZD12M <- ((f$`NZDUSD 12m forward point`/(10000*f$NZDUSD))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, NZDV1M=`NZDUSDV1M Curncy`,NZDV3M=`NZDUSDV3M Curncy`,NZDV6M=`NZDUSDV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, NZDV1MP=`NZDUSDV1M Curncy`-`NZDUSDH1M Curncy`,NZDV3MP=`NZDUSDV3M Curncy`-`NZDUSDH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, NZDR3MR=-`NZDUSD25R3M Curncy`)], by = "date", all.x = TRUE)

f$JPY_ret <- -((f$USDJPY-(dplyr::lag(f$`USDJPY 12m forward point`,rp)/10000))/ dplyr::lag(f$USDJPY, rp) - 1 )
#f$JPY_ret=scales::percent(f$JPY_ret,accuracy = 0.1)
f$JPY3M <- ((-f$`USDJPY 3m forward point`/(100*f$USDJPY))*4) #%>% scales::percent(accuracy = 0.01)
f$JPY6M <- ((-f$`USDJPY 6m forward point`/(100*f$USDJPY))*2) #%>% scales::percent(accuracy = 0.01)
f$JPY12M <- ((-f$`USDJPY 12m forward point`/(100*f$USDJPY))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, JPYV1M=`USDJPYV1M Curncy`,JPYV3M=`USDJPYV3M Curncy`,JPYV6M=`USDJPYV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, JPYV1MP=`USDJPYV1M Curncy`-`USDJPYH1M Curncy`,JPYV3MP=`USDJPYV3M Curncy`-`USDJPYH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, JPYR3MR=-`USDJPY25R3M Curncy`)], by = "date", all.x = TRUE)

f$SGD_ret <- -((f$USDSGD-(dplyr::lag(f$`USDSGD 12m forward point`,rp)/10000))/ dplyr::lag(f$USDSGD, rp) - 1 )
#f$SGD_ret=scales::percent(f$SGD_ret,accuracy = 0.1)
f$SGD3M <- ((-f$`USDSGD 3m forward point`/(10000*f$USDSGD))*4) #%>% scales::percent(accuracy = 0.01)
f$SGD6M <- ((-f$`USDSGD 6m forward point`/(10000*f$USDSGD))*2) #%>% scales::percent(accuracy = 0.01)
f$SGD12M <- ((-f$`USDSGD 12m forward point`/(10000*f$USDSGD))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, SGDV1M=`USDSGDV1M Curncy`,SGDV3M=`USDSGDV3M Curncy`,SGDV6M=`USDSGDV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, SGDV1MP=`USDSGDV1M Curncy`-`USDSGDH1M Curncy`,SGDV3MP=`USDSGDV3M Curncy`-`USDSGDH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, SGDR3MR=-`USDSGD25R3M Curncy`)], by = "date", all.x = TRUE)

f$NOK_ret <- -((f$USDNOK-(dplyr::lag(f$`USDNOK 12m forward point`,rp)/10000))/ dplyr::lag(f$USDNOK, rp) - 1 )
#f$NOK_ret=scales::percent(f$NOK_ret,accuracy = 0.1)
f$NOK3M <- ((-f$`USDNOK 3m forward point`/(10000*f$USDNOK))*4) #%>% scales::percent(accuracy = 0.01)
f$NOK6M <- ((-f$`USDNOK 6m forward point`/(10000*f$USDNOK))*2) #%>% scales::percent(accuracy = 0.01)
f$NOK12M <- ((-f$`USDNOK 12m forward point`/(10000*f$USDNOK))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, NOKV1M=`USDNOKV1M Curncy`,NOKV3M=`USDNOKV3M Curncy`,NOKV6M=`USDNOKV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, NOKV1MP=`USDNOKV1M Curncy`-`USDNOKH1M Curncy`,NOKV3MP=`USDNOKV3M Curncy`-`USDNOKH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, NOKR3MR=-`USDNOK25R3M Curncy`)], by = "date", all.x = TRUE)

f$CHF_ret <- -((f$USDCHF-(dplyr::lag(f$`USDCHF 12m forward point`,rp)/10000))/ dplyr::lag(f$USDCHF, rp) - 1 )
#f$CHF_ret=scales::percent(f$CHF_ret,accuracy = 0.1)
f$CHF3M <- ((-f$`USDCHF 3m forward point`/(10000*f$USDCHF))*4) #%>% scales::percent(accuracy = 0.01)
f$CHF6M <- ((-f$`USDCHF 6m forward point`/(10000*f$USDCHF))*2) #%>% scales::percent(accuracy = 0.01)
f$CHF12M <- ((-f$`USDCHF 12m forward point`/(10000*f$USDCHF))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, CHFV1M=`USDCHFV1M Curncy`,CHFV3M=`USDCHFV3M Curncy`,CHFV6M=`USDCHFV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, CHFV1MP=`USDCHFV1M Curncy`-`USDCHFH1M Curncy`,CHFV3MP=`USDCHFV3M Curncy`-`USDCHFH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, CHFR3MR=-`USDCHF25R3M Curncy`)], by = "date", all.x = TRUE)

f$CAD_ret <- -((f$USDCAD-(dplyr::lag(f$`USDCAD 12m forward point`,rp)/10000))/ dplyr::lag(f$USDCAD, rp) - 1 )
#f$CAD_ret=scales::percent(f$CAD_ret,accuracy = 0.1)
f$CAD3M <- ((-f$`USDCAD 3m forward point`/(10000*f$USDCAD))*4) #%>% scales::percent(accuracy = 0.01)
f$CAD6M <- ((-f$`USDCAD 6m forward point`/(10000*f$USDCAD))*2) #%>% scales::percent(accuracy = 0.01)
f$CAD12M <- ((-f$`USDCAD 12m forward point`/(10000*f$USDCAD))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, CADV1M=`USDCADV1M Curncy`,CADV3M=`USDCADV3M Curncy`,CADV6M=`USDCADV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, CADV1MP=`USDCADV1M Curncy`-`USDCADH1M Curncy`,CADV3MP=`USDCADV3M Curncy`-`USDCADH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, CADR3MR=-`USDCAD25R3M Curncy`)], by = "date", all.x = TRUE)

f$CNH_ret <- -((f$USDCNH-(dplyr::lag(f$`USDCNH 12m forward point`,rp)/10000))/ dplyr::lag(f$USDCNH, rp) - 1 )
#f$CNH_ret=scales::percent(f$CNH_ret,accuracy = 0.1)
f$CNH3M <- ((-f$`USDCNH 3m forward point`/(10000*f$USDCNH))*4) #%>% scales::percent(accuracy = 0.01)
f$CNH6M <- ((-f$`USDCNH 6m forward point`/(10000*f$USDCNH))*2) #%>% scales::percent(accuracy = 0.01)
f$CNH12M <- ((-f$`USDCNH 12m forward point`/(10000*f$USDCNH))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, CNHV1M=`USDCNHV1M Curncy`,CNHV3M=`USDCNHV3M Curncy`,CNHV6M=`USDCNHV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, CNHV1MP=`USDCNHV1M Curncy`-`USDCNHH1M Curncy`,CNHV3MP=`USDCNHV3M Curncy`-`USDCNHH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, CNHR3MR=-`USDCNH25R3M Curncy`)], by = "date", all.x = TRUE)

f$ZAR_ret <- -((f$USDZAR-(dplyr::lag(f$`USDZAR 12m forward point`,rp)/10000))/ dplyr::lag(f$USDZAR, rp) - 1 )
#f$ZAR_ret=scales::percent(f$ZAR_ret,accuracy = 0.1)
f$ZAR3M <- ((-f$`USDZAR 3m forward point`/(10000*f$USDZAR))*4) #%>% scales::percent(accuracy = 0.01)
f$ZAR6M <- ((-f$`USDZAR 6m forward point`/(10000*f$USDZAR))*2) #%>% scales::percent(accuracy = 0.01)
f$ZAR12M <- ((-f$`USDZAR 12m forward point`/(10000*f$USDZAR))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, ZARV1M=`USDZARV1M Curncy`,ZARV3M=`USDZARV3M Curncy`,ZARV6M=`USDZARV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, ZARV1MP=`USDZARV1M Curncy`-`USDZARH1M Curncy`,ZARV3MP=`USDZARV3M Curncy`-`USDZARH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, ZARR3MR=-`USDZAR25R3M Curncy`)], by = "date", all.x = TRUE)

f$TRY_ret <- -((f$USDTRY-(dplyr::lag(f$`USDTRY 12m forward point`,rp)/10000))/ dplyr::lag(f$USDTRY, rp) - 1 )
#f$TRY_ret=scales::percent(f$TRY_ret,accuracy = 0.1)
f$TRY3M <- ((-f$`USDTRY 3m forward point`/(10000*f$USDTRY))*4) #%>% scales::percent(accuracy = 0.01)
f$TRY6M <- ((-f$`USDTRY 6m forward point`/(10000*f$USDTRY))*2) #%>% scales::percent(accuracy = 0.01)
f$TRY12M <- ((-f$`USDTRY 12m forward point`/(10000*f$USDTRY))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, TRYV1M=`USDTRYV1M Curncy`,TRYV3M=`USDTRYV3M Curncy`,TRYV6M=`USDTRYV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, TRYV1MP=`USDTRYV1M Curncy`-`USDTRYH1M Curncy`,TRYV3MP=`USDTRYV3M Curncy`-`USDTRYH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, TRYR3MR=-`USDTRY25R3M Curncy`)], by = "date", all.x = TRUE)

f$MXN_ret <- -((f$USDMXN-(dplyr::lag(f$`USDMXN 12m forward point`,rp)/10000))/ dplyr::lag(f$USDMXN, rp) - 1 )
#f$MXN_ret=scales::percent(f$MXN_ret,accuracy = 0.1)
f$MXN3M <- ((-f$`USDMXN 3m forward point`/(10000*f$USDMXN))*4) #%>% scales::percent(accuracy = 0.01)
f$MXN6M <- ((-f$`USDMXN 6m forward point`/(10000*f$USDMXN))*2) #%>% scales::percent(accuracy = 0.01)
f$MXN12M <- ((-f$`USDMXN 12m forward point`/(10000*f$USDMXN))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, MXNV1M=`USDMXNV1M Curncy`,MXNV3M=`USDMXNV3M Curncy`,MXNV6M=`USDMXNV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, MXNV1MP=`USDMXNV1M Curncy`-`USDMXNH1M Curncy`,MXNV3MP=`USDMXNV3M Curncy`-`USDMXNH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, MXNR3MR=-`USDMXN25R3M Curncy`)], by = "date", all.x = TRUE)

f$HKD_ret <- -((f$USDHKD-(dplyr::lag(f$`USDHKD 12m forward point`,rp)/10000))/ dplyr::lag(f$USDHKD, rp) - 1 )
#f$HKD_ret=scales::percent(f$HKD_ret,accuracy = 0.1)
f$HKD3M <- ((-f$`USDHKD 3m forward point`/(10000*f$USDHKD))*4) #%>% scales::percent(accuracy = 0.01)
f$HKD6M <- ((-f$`USDHKD 6m forward point`/(10000*f$USDHKD))*2) #%>% scales::percent(accuracy = 0.01)
f$HKD12M <- ((-f$`USDHKD 12m forward point`/(10000*f$USDHKD))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, HKDV1M=`USDHKDV1M Curncy`,HKDV3M=`USDHKDV3M Curncy`,HKDV6M=`USDHKDV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, HKDV1MP=`USDHKDV1M Curncy`-`USDHKDH1M Curncy`,HKDV3MP=`USDHKDV3M Curncy`-`USDHKDH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, HKDR3MR=-`USDHKD25R3M Curncy`)], by = "date", all.x = TRUE)

f$CNY_ret <- -((f$`CNY Fixing`-(dplyr::lag(f$`USDCNY 12m NDF`,rp)/10000))/ dplyr::lag(f$`CNY Fixing`, rp) - 1 )
#f$CNY_ret=scales::percent(f$CNY_ret,accuracy = 0.1)
f$CNY3M <- ((-f$`USDCNY 3m NDF`/(10000*f$`CNY Fixing`))*4) #%>% scales::percent(accuracy = 0.01)
f$CNY6M <- ((-f$`USDCNY 6m NDF`/(10000*f$`CNY Fixing`))*2) #%>% scales::percent(accuracy = 0.01)
f$CNY12M <- ((-f$`USDCNY 12m NDF`/(10000*f$`CNY Fixing`))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, CNYV1M=`USDCNYV1M Curncy`,CNYV3M=`USDCNYV3M Curncy`,CNYV6M=`USDCNYV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, CNYV1MP=`USDCNYV1M Curncy`-`USDCNYH1M Curncy`,CNYV3MP=`USDCNYV3M Curncy`-`USDCNYH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, CNYR3MR=-`USDCNY25R3M Curncy`)], by = "date", all.x = TRUE)

f$KRW_ret <- -((f$USDKRW-(dplyr::lag(f$`USDKRW 12m NDF`,rp)))/ dplyr::lag(f$USDKRW, rp) - 1 )
#f$KRW_ret=scales::percent(f$KRW_ret,accuracy = 0.1)
f$KRW3M <- ((-f$`USDKRW 3m NDF`/(f$USDKRW))*4) #%>% scales::percent(accuracy = 0.01)
f$KRW6M <- ((-f$`USDKRW 6m NDF`/(f$USDKRW))*2) #%>% scales::percent(accuracy = 0.01)
f$KRW12M <- ((-f$`USDKRW 12m NDF`/(f$USDKRW))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, KRWV1M=`USDKRWV1M Curncy`,KRWV3M=`USDKRWV3M Curncy`,KRWV6M=`USDKRWV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, KRWV1MP=`USDKRWV1M Curncy`-`USDKRWH1M Curncy`,KRWV3MP=`USDKRWV3M Curncy`-`USDKRWH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, KRWR3MR=-`USDKRW25R3M Curncy`)], by = "date", all.x = TRUE)

f$TWD_ret <- -((f$USDTWD-(dplyr::lag(f$`USDTWD 12m NDF`,rp)))/ dplyr::lag(f$USDTWD, rp) - 1 )
#f$TWD_ret=scales::percent(f$TWD_ret,accuracy = 0.1)
f$TWD3M <- ((-f$`USDTWD 3m NDF`/(f$USDTWD))*4) #%>% scales::percent(accuracy = 0.01)
f$TWD6M <- ((-f$`USDTWD 6m NDF`/(f$USDTWD))*2) #%>% scales::percent(accuracy = 0.01)
f$TWD12M <- ((-f$`USDTWD 12m NDF`/(f$USDTWD))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, TWDV1M=`USDTWDV1M Curncy`,TWDV3M=`USDTWDV3M Curncy`,TWDV6M=`USDTWDV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, TWDV1MP=`USDTWDV1M Curncy`-`USDTWDH1M Curncy`,TWDV3MP=`USDTWDV3M Curncy`-`USDTWDH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, TWDR3MR=-`USDTWD25R3M Curncy`)], by = "date", all.x = TRUE)

f$INR_ret <- -((f$USDINR-(dplyr::lag(f$`USDINR 12m NDF`,rp)/100))/ dplyr::lag(f$USDINR, rp) - 1 )
#f$INR_ret=scales::percent(f$INR_ret,accuracy = 0.1)
f$INR3M <- ((-f$`USDINR 3m NDF`/(100*f$USDINR))*4) #%>% scales::percent(accuracy = 0.01)
f$INR6M <- ((-f$`USDINR 6m NDF`/(100*f$USDINR))*2) #%>% scales::percent(accuracy = 0.01)
f$INR12M <- ((-f$`USDINR 12m NDF`/(100*f$USDINR))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, INRV1M=`USDINRV1M Curncy`,INRV3M=`USDINRV3M Curncy`,INRV6M=`USDINRV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, INRV1MP=`USDINRV1M Curncy`-`USDINRH1M Curncy`,INRV3MP=`USDINRV3M Curncy`-`USDINRH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, INRR3MR=-`USDINR25R3M Curncy`)], by = "date", all.x = TRUE)

f$IDR_ret <- -((f$USDIDR-(dplyr::lag(f$`USDIDR 12m NDF`,rp)))/ dplyr::lag(f$USDIDR, rp) - 1 )
#f$IDR_ret=scales::percent(f$IDR_ret,accuracy = 0.1)
f$IDR3M <- ((-(f$`USDIDR 3m NDF`)/(f$USDIDR))*4) #%>% scales::percent(accuracy = 0.01)
f$IDR6M <- ((-f$`USDIDR 6m NDF`/(f$USDIDR))*2) #%>% scales::percent(accuracy = 0.01)
f$IDR12M <- ((-f$`USDIDR 12m NDF`/(f$USDIDR))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, IDRV1M=`USDIDRV1M Curncy`,IDRV3M=`USDIDRV3M Curncy`,IDRV6M=`USDIDRV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, IDRV1MP=`USDIDRV1M Curncy`-`USDIDRH1M Curncy`,IDRV3MP=`USDIDRV3M Curncy`-`USDIDRH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, IDRR3MR=-`USDIDR25R3M Curncy`)], by = "date", all.x = TRUE)

f$PHP_ret <- -((f$USDPHP-(dplyr::lag(f$`USDPHP 12m NDF`,rp)))/ dplyr::lag(f$USDPHP, rp) - 1 )
#f$PHP_ret=scales::percent(f$PHP_ret,accuracy = 0.1)
f$PHP3M <- ((-f$`USDPHP 3m NDF`/(f$USDPHP))*4) #%>% scales::percent(accuracy = 0.01)
f$PHP6M <- ((-f$`USDPHP 6m NDF`/(f$USDPHP))*2) #%>% scales::percent(accuracy = 0.01)
f$PHP12M <- ((-f$`USDPHP 12m NDF`/(f$USDPHP))*1) #%>% scales::percent(accuracy = 0.01)
f <- merge(f, v[, .(date, PHPV1M=`USDPHPV1M Curncy`,PHPV3M=`USDPHPV3M Curncy`,PHPV6M=`USDPHPV6M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, PHPV1MP=`USDPHPV1M Curncy`-`USDPHPH1M Curncy`,PHPV3MP=`USDPHPV3M Curncy`-`USDPHPH3M Curncy`)], by = "date", all.x = TRUE)
f <- merge(f, v[, .(date, PHPR3MR=-`USDPHP25R3M Curncy`)], by = "date", all.x = TRUE)

# fill in non-numeric cells
for (col in names(f)) {
  if (is.numeric(f[[col]])) {
    # 对数值列：用后一个非NA的值填补（NOCB）
    f[[col]] <- na.locf(f[[col]], fromLast = TRUE, na.rm = FALSE)
  }
}

fz <- copy(f)
# 找出数值列
num_cols <- names(fz)[sapply(fz, is.numeric)]
# 去掉第一列（假设是日期）
num_cols <- setdiff(num_cols, names(fz)[1])

# 对选定列计算 z-score
fz[, (num_cols) := lapply(.SD, scale), .SDcols = num_cols]


###make DMFX Carry Vol table----
DMFXcname=c("Level","Return","3M Carry","6M Carry","12M Carry","1M Vol","3M Vol","6M Vol","1M Vol RP","3M Vol RP","3M25D RR")
DMFXrname=DMFX
rownumber=which(f$date==TS)
# 1️⃣ 创建空 data.frame
dtDM <- data.frame(matrix(NA, nrow = length(DMFXrname), ncol = length(DMFXcname)),
                   stringsAsFactors = FALSE)
dtDMZ <- data.frame(matrix(NA, nrow = length(DMFXrname), ncol = length(DMFXcname)),
                   stringsAsFactors = FALSE)

# 2️⃣ 设置列名和行名
colnames(dtDM) <- DMFXcname
rownames(dtDM) <- DMFXrname
colnames(dtDMZ) <- DMFXcname
rownames(dtDMZ) <- DMFXrname

# 循环按行赋值
for (i in seq_along(DMFXrname)) {
  cur <- DMFXrname[i]
  
  # 找当前货币匹配的列
  cols <- names(f)[startsWith(names(f), cur) & nchar(names(f)) < 10 & !grepl("USD", names(f))|(nchar(names(f)) == 6 & grepl(cur, names(f)))]
  print(cols)
  if (length(cols) == 0) next
  # 取出 f 中 rownumber 行对应列的值，并转换成向量
  row_values <- as.vector(unlist(f[rownumber, ..cols]))
  row_values_z <- as.vector(unlist(fz[rownumber, ..cols]))
  # print(cur)
  # print(row_values)
  # 
  # 赋值到 dtDM 对应行和列
  dtDM[cur , ] <- row_values
  dtDMZ[cur, ] <- row_values_z
}
dtDM[, 2:5] <- lapply(dtDM[, 2:5], as.numeric)
dtDM <- dtDM %>% mutate(`3M Carry to Vol`=(100*`3M Carry`)/`3M Vol`)
sapply(dtDM, typeof)
mat <- as.matrix(dtDM) 
DMFXheatmap=heatmap(t(mat), Rowv = NA, Colv = NA, col = heat.colors(256))
matz <- as.matrix(dtDMZ) 
DMFXZheatmap=heatmap(t(matz), Rowv = NA, Colv = NA, col = heat.colors(256))
#these two variables can not store the values!
DMclean=copy(dtDM)

DMcleanZ=copy(dtDMZ)

DMclean[, c(2:5,12)] <- lapply(DMclean[, c(2:5,12)], function(x) percent(x, accuracy = 0.01))
# 找出剩余的列
other_cols <- setdiff(seq_len(ncol(DMclean)), c(2:5, 12))
# 对剩下的列保留两位小数
DMclean[, other_cols] <- lapply(DMclean[, other_cols], function(x) format(round(as.numeric(x), 2), nsmall = 2))

#dtDMZ[, 2:5] <- lapply(dtDMZ[, 2:5], as.numeric)
#DMcleanZ <- DMcleanZ %>% mutate(`3M Carry to Vol`=(100*`3M Carry`)/`3M Vol`)
#DMcleanZ[, c(2:5,11)] <- lapply(DMcleanZ[, c(2:5,11)], function(x) percent(x, accuracy = 0.01))
DMcleanZ[,] <- lapply(DMcleanZ[,], function(x) format(round(as.numeric(x), 2), nsmall = 2))
NonUSD <- c("EUR","AUD","NZD","GBP")
dtDMZ[NonUSD,"Level"]=-dtDMZ[NonUSD,"Level"]
#plot dtDM result
dtDM$Currency <- rownames(dtDM)
dt_long4 <- dtDM %>%
  select(-Level) %>%           # 去掉 Level 列
  melt(id.vars = "Currency")
colnames(dt_long4) <- c("Currency", "Metric", "Value")

# 2️⃣ 每个指标独立归一化气泡大小
dt_long4 <- dt_long4 %>%
  group_by(Metric) %>%
  mutate(Value_size = abs(Value) / max(abs(Value))) %>%
  ungroup()

# 3️⃣ 绘制 bubble plot
DMFXBubble=ggplot(dt_long4, aes(x=Metric, y=Currency, size=Value_size, fill=Value)) +
  geom_point(shape=21, color="black", alpha=0.7) +
  geom_text(aes(label=round(Value,2)), color="black", size=4, fontface="bold", check_overlap = FALSE) +  # 黑色加粗字体
  scale_size_continuous(range=c(3,15)) +
  scale_fill_viridis_c(option="C", direction=-1, alpha=0.5) +  # 浅色系填充，半透明
  theme_minimal() +
  theme(axis.text.x = element_text(angle=0, hjust=0.5),
        legend.position="right") +
  labs(title="Currency Metrics Bubble Plot (Size normalized per Metric)",
       x="Metric", y="Currency", size="Value (per Metric)", fill="Value")

###plot return-----
# 简洁处理：如果没有 Currency 列就用 rownames
df <- dtDM
if(!("Currency" %in% names(df))) {
  df$Currency <- if(!is.null(rownames(df)) && any(rownames(df) != "")) rownames(df) else as.character(seq_len(nrow(df)))
}
# 保证 Return 为数值并移除 NA
df <- transform(df, Return = as.numeric(Return), Currency = as.character(Currency))
df <- df[!is.na(df$Return), ]
df$Currency <- factor(df$Currency, levels = df$Currency[order(df$Return)])  # 排序

# -------- 浅色版（默认）
p <- ggplot(df, aes(x = Return, y = Currency, fill = Return, alpha = abs(Return))) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 0, color = "black", size = 0.35) +
  scale_fill_gradient2(low = "#FFB3B3", mid = "grey95", high = "#6EA8FF", midpoint = 0,
                       labels = percent_format(accuracy = 0.1), name = "Return") +
  scale_alpha(range = c(0.35, 1), guide = FALSE) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  geom_text(aes(label = percent(Return, accuracy = 0.1)),
            hjust = ifelse(df$Return > 0, -0.05, 1.05),
            size = 3) +
  labs(x = "Return", y = NULL, title = paste("Returns by Currency",TS)) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())

# print(p)

# -------- 深色版（更像 Bloomberg）
p_dark <- p +
  theme_dark(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#0f1720", colour = NA),
    panel.background = element_rect(fill = "#0f1720", colour = NA),
    panel.grid.major = element_line(colour = "grey30"),
    axis.text = element_text(colour = "white"),
    plot.title = element_text(colour = "white")
  ) +
  scale_fill_gradient2(low = "#ff9b9b", mid = "#0f1720", high = "#7fb0ff", midpoint = 0)

# print(p_dark)

###facet for dtDMZ table----
dt_long <- dtDMZ %>%
  tibble::rownames_to_column("currency") %>%
  pivot_longer(cols = -currency, names_to = "metric", values_to = "value")

FXZfacet=ggplot(dt_long, aes(x = value, y = reorder(currency, value), fill = value)) +
  geom_col() +
  facet_wrap(~ metric, scales = "free_x") +  # 小 multiples
  scale_fill_gradient2(
    low = "#d73027", mid = "white", high = "#1a9850", midpoint = 0,
    labels = percent_format(accuracy = 0.1)
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  labs(title = "FX Metrics by Currency", subtitle = paste(TS))


###COR----
# 计算相关矩阵
cormat <- cor(dtDMZ, use = "pairwise.complete.obs")
# 画图
cormat_dt <- as.data.table(cormat, keep.rownames = TRUE)
setnames(cormat_dt, "rn", "Var1")
melted_cormat <- melt(cormat_dt, id.vars = "Var1", variable.name = "Var2", value.name = "value")
# 将 cormat 转为 long format
FXCorr=ggplot(melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(aes(color = ifelse(Var2 == "Return", "highlight", "normal")), 
            size = 1, width = 0.95, height = 0.95) +
  scale_color_manual(values = c("highlight" = "gold", "normal" = "grey20"), guide = "none") +
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 3) +
  scale_fill_gradient2(
    low = "#D73027", mid = "#F7F7F7", high = "#4575B4",
    midpoint = 0, limit = c(-1, 1), name = "Correlation"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", face = "bold"),
    panel.grid = element_blank()
  ) +
  labs(title = "FX Correlation Matrix", x = NULL, y = NULL)
# print(FXCorr)
par(mfrow=c(1,1))
FXCorrEllipse=corrplot::corrplot(cormat, method = "ellipse",addCoef.col = "black")
#can not save the image, need to re=plotted

###make EMFX vol table----
EMFXcname=c("Return","3M Carry","6M Carry","12M Carry","1M Vol","3M Vol","6M Vol","1M Vol RP","3M Vol RP","3M25D RR")
EMFXrname=c(EMFX,NDF)
rownumber=which(f$date==TS)
# 1️⃣ 创建空 data.frame
dtEM <- data.frame(matrix(NA, nrow = length(EMFXrname), ncol = length(EMFXcname)),
                   stringsAsFactors = FALSE)
dtEMZ <- data.frame(matrix(NA, nrow = length(EMFXrname), ncol = length(EMFXcname)),
                   stringsAsFactors = FALSE)

# 2️⃣ 设置列名和行名
colnames(dtEM) <- EMFXcname
rownames(dtEM) <- EMFXrname
colnames(dtEMZ) <- EMFXcname
rownames(dtEMZ) <- EMFXrname

# non_numeric_rows <- which(is.na(suppressWarnings(as.numeric(f$`USDCNY 12m NDF)&!is.na(f$`USDCNY 12m NDF`))
# f[non_numeric_rows, ]


# 循环按行赋值
for (i in seq_along(EMFXrname)) {
  cur <- EMFXrname[i]
  
  # 找当前货币匹配的列
  cols <- names(f)[startsWith(names(f), cur) & nchar(names(f)) < 10 & !grepl("USD", names(f))]
  # print(cols)
  if (length(cols) == 0) next
  
  # 取出 f 中 rownumber 行对应列的值，并转换成向量
  row_values <- as.vector(unlist(f[rownumber, ..cols]))
  row_values_z <- as.vector(unlist(fz[rownumber, ..cols]))
  # print(cur)
  # print(row_values)
  
  # 赋值到 dtDM 对应行和列
  dtEM[cur, ] <- row_values
  dtEMZ[cur, ] <- row_values_z
}
dtEM_1=copy(dtEM)
dtEMZ_1=copy(dtEMZ)
dtEM=dtEM_1
dtEMZ=dtEMZ_1
dtEM[, 1:4] <- lapply(dtEM[, 1:4], as.numeric)
dtEM <- dtEM %>% mutate(`3M Carry to Vol`=(100*`3M Carry`)/`3M Vol`)
matem <- as.matrix(dtEM) 
hm_col <- colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(256)
EMFXheatmap=heatmap( matem, Rowv = NA, Colv = NA, col = hm_col,scale = "column")   # 或 "row")
#can not save, need to repost
dtEM[, ] <- lapply(dtEM[, ], as.numeric)
dtEM[,] <- lapply(dtEM[,], function(x) round(as.numeric(x), 2))
dtEM[, c(1:4,11)] <- lapply(dtEM[, c(1:4,11)], function(x) percent(x, accuracy = 0.01))
#dtEMZ[, 1:4] <- lapply(dtDMZ[, 1:4], as.numeric)
matemZ <- as.matrix(dtEMZ) 
EMFXZheatmap=heatmap( matemZ, Rowv = NA, Colv = NA, col = hm_col,scale = "column") 
#plot for EMZ
dtEMZ_1$Currency <- rownames(dtEMZ_1)
# 2️⃣ melt，指定 id.vars
dt_long2 <- melt(dtEMZ_1, id.vars = "Currency")
colnames(dt_long2) <- c("Currency", "Metric", "Value")

# 2️⃣ 花瓣图 / Radial Bar Plot
EMFXZRadial=ggplot(dt_long2, aes(x = Metric, y = Value, fill = Currency)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar() +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10)) +
  labs(title = "Radial Bar Plot of Currency Metrics")
# print(EMFXZRadial)
#3 花瓣图+facet
EMFXZRadial_FACET=ggplot(dt_long2, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_polar() +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  labs(title = "Radial Bar Plot per Currency Z-scores") +
  facet_wrap(~Currency)
#plot for EM
dtEM_1$Currency <- rownames(dtEM_1)
dt_long3 <- melt(dtEM_1, id.vars = "Currency")
colnames(dt_long3) <- c("Currency", "Metric", "Value")
# 2️⃣ 对每个指标独立排序货币（从大到小）
dt_long3 <- dt_long3 %>%
  group_by(Metric) %>%
  arrange(Metric, desc(Value)) %>%        # 排序
  mutate(Currency_ordered = factor(Currency, levels = Currency)) %>% # 生成每个facet独立factor
  ungroup()

# 3️⃣ 横向条形图 + facet，每个 facet 横轴独立
EMFXFacet=ggplot(dt_long3, aes(x=Value, y=Currency_ordered, fill=Currency)) +
  geom_bar(stat="identity") +
  facet_wrap(~Metric, scales="free_x") +  # 横轴独立
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position="none") +
  labs(title="Horizontal Bar Plot per Metric (Each facet sorted independently)",
       x="Value", y="Currency")
# #violin
# ggplot(dt_long2, aes(x = Metric, y = Value, fill = Currency)) +
#   geom_violin(trim = FALSE, alpha = 0.7) +
#   geom_jitter(height = 0, width = 0.1, size = 1, alpha = 0.5) +
#   scale_fill_viridis_d() +
#   theme_minimal() +
#   labs(title = "Violin Plot of Currency Metrics")

###make DMFX Carry Return table----
colcarry <-vector()
for (i in DMFX) {
  t <- names(f)[grepl(i,names(f))&grepl("USD",names(f))&grepl("Carry",names(f))]
  colcarry <- c(colcarry,t)
}
colcarry <- c(colcarry,"date")
# f[,..colcarry] %>% head(10)
fc <- f[,..colcarry] 
# f_long <- pivot_longer(fc, cols = -date, names_to = "series", values_to = "value")
# 
# ggplot(f_long, aes(x = date, y = value, linetype = series,color=series)) +
#   geom_line(size = 1) +
#   geom_point(aes(shape = series), size = 1) +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(x = NULL, y = NULL, linetype = "Series", shape = "Series") +
#   theme_minimal(base_size = 14) +
#   theme(
#     legend.position = "bottom",
#     legend.box = "horizontal",
#     panel.grid.minor = element_blank(),
#     axis.text.x = element_text(angle = 0, vjust = 0.5)
#   )

DMspot=vector()
for (i in DMFX) {
  t <- names(f)[grepl(i,names(f))&grepl("USD",names(f))&(nchar(names(f))==6)|(nchar(names(f))==11)]
  DMspot <- c(DMspot,t)
}

for (col in DMspot) {
  f[, paste0(col, "_cumr") := 100*(get(col) / first(get(col))) ]
}

c=unique(c(DMspot,colcarry,"date"))
w=f[,..c]

###plot FX carry return ----

# 假设 w 是你的 data.table
df <- as.data.frame(w)

# 找出所有 spot_cumr 列（11 个字符，以 _cumr 结尾）
spot_cumr_cols <- names(f)[nchar(names(f)) == 11 & grepl("_cumr$", names(f))]

# 对应的 carry 列（列名里包含 spot 名字 + "  Carry Return"）
carry_cols <- sapply(spot_cumr_cols, function(x) {
  spot <- sub("_cumr$", "", x)
  carry_name <- grep(paste0(spot, "  Carry Return"), names(df), value = TRUE)
  if(length(carry_name) == 1) carry_name else NA
})
carry_cols <- carry_cols[!is.na(carry_cols)]

# 只保留 date + spot_cumr + carry 列
all_cols <- c("date", spot_cumr_cols, carry_cols) %>% as.vector()
df_plot <- f[,..all_cols]

#alldf# 转成长表
df_long <- pivot_longer(
  df_plot,
  cols = -date,
  names_to = "series",
  values_to = "value"
)

# 创建 group，把每个 spot_cumr 与对应 carry 配对
df_long$group <- sapply(df_long$series, function(x) {
  if(grepl("_cumr$", x)) sub("_cumr$", "", x)
  else sub("  Carry Return$", "", x)
})

# 添加一个变量，标记线型
df_long$line_type <- ifelse(grepl("_cumr$", df_long$series), "spot_cumr", "carry")

# 绘图
pcarry <- ggplot(df_long, aes(x = date, y = value, color = line_type, linetype = line_type)) +
  geom_line(size = 1) +
  facet_wrap(~group, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = c("spot_cumr" = "red", "carry" = "blue"),
    labels = c("spot_cumr" = "Spot Cumulative Return", "carry" = "Carry Return")
  ) +
  scale_linetype_manual(
    values = c("spot_cumr" = "solid", "carry" = "dashed"),
    labels = c("spot_cumr" = "Spot Cumulative Return", "carry" = "Carry Return")
  ) +
  guides(
    color = guide_legend(title = NULL, override.aes = list(linetype = c("solid", "dashed"))),
    linetype = "none"   # 隐藏重复 linetype legend
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "#2B2B2B", color = NA),
    plot.background = element_rect(fill = "#2B2B2B", color = NA),
    strip.background = element_rect(fill = "#3C3C3C", color = NA),
    legend.background = element_rect(fill = "#2B2B2B", color = NA),
    legend.key = element_rect(fill = "#2B2B2B", color = NA),
    legend.text = element_text(color = "white"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "white"),  # 横轴文字水平
    axis.text.y = element_text(color = "white"),
    strip.text = element_text(color = "white"),
    plot.title = element_text(color = "white"),
    panel.grid.major = element_line(color = "#444444", size = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Spot Cumulative Return and Carry Return Time Series"
  )

