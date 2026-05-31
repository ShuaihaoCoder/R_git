library(data.table)
library(Rblpapi)
library(stringr)
library(dplyr)

setwd("C:/Users/ASUS/Desktop/R_Union")
startDate <- as.Date("2025-01-01")
endDate   <- Sys.Date()
get_bbg <- function(tickers, fields, start_date, end_date){
  start_date <-as.Date(start_date)
  end_date <-as.Date(end_date)
  con<-blpConnect()
  res_all  <- list()
  for(i in 1:length(tickers)){
    
    fields_i <- if(is.na(fields[i]) || fields[i] == "" || toupper(fields[i]) == "NA") {
      "PX_LAST"
    } else {
      fields[i]
    }
    
    data <- bdh(securities = tickers[i],
                fields     = fields_i,
                start.date = start_date,
                end.date   = end_date)
    if (length(data) == 0) {
      stop("No data returned from Bloomberg. Please check tickers/fields/date range.")
    }
    data <- data.table(data)
    data[ ,ticker := tickers[i]] 
    res_all[[i]] = data
    
  }
  blpDisconnect(con)
  rbindlist(res_all,fill = T)
}

get_bdp <- function(tickers, fields){
  con<-blpConnect()
  res_all  <- list()
  for(i in 1:length(tickers)){
    
    fields_i <- if(is.na(fields[i]) || fields[i] == "" || toupper(fields[i]) == "NA") {
      "PX_LAST"
    } else {
      fields[i]
    }
    
    data <- bdp(securities = tickers[i],
                fields     = fields_i)
    if (length(data) == 0) {
      stop("No data returned from Bloomberg. Please check tickers/fields/date range.")
    }
    data <- data.table(data)
    data[ ,ticker := tickers[i]] 
    res_all[[i]] = data
    
  }
  blpDisconnect(con)
  rbindlist(res_all,fill = T)
}

plot_d <- function(y) {
  expr <- substitute(y)
  # 试着从类似 df$col 的表达式里取出 data.frame
  if (is.call(expr) && identical(expr[[1]], as.name("$"))) {
    dfname <- as.character(expr[[2]])
    df <- get(dfname, envir = parent.frame())
    d <- df[["date"]]
    yvals <- eval(expr, envir = parent.frame())
  } else {
    # 不是 df$col 的形式：直接用传入的向量 y
    yvals <- y
    # 尝试按优先级取 date：1) x$date 2) 全局 date 3) 序列索引
    if (exists("x", envir = parent.frame()) && "date" %in% names(get("x", envir = parent.frame()))) {
      d <- get("x", envir = parent.frame())[["date"]]
    } else if (exists("date", envir = parent.frame())) {
      d <- get("date", envir = parent.frame())
    } else {
      d <- seq_along(yvals)
    }
  }
  
  valid <- !is.na(yvals) & !is.na(d)
  plot(d[valid], yvals[valid],
       type = "p", pch = 19,
       xlab = "Date",
       ylab = deparse(expr),
       main = deparse(expr))
  lines(d[valid], yvals[valid], col = "gray70")
}


setwd("C:/Users/ASUS/Desktop/R_Union")
file_path_eq <- "C:/Users/ASUS/Desktop/R_Union/equity_database.csv"
file_path_rates <- "C:/Users/ASUS/Desktop/R_Union/rates_database.csv"
file_path_fx <- "C:/Users/ASUS/Desktop/R_Union/FX_Data.csv"
file_path_vol <- "C:/Users/ASUS/Desktop/R_Union/Vol_database.csv"
file_path_eco <- "C:/Users/ASUS/Desktop/R_Union/eco_database.csv"
file_path_mm <- "C:/Users/ASUS/Desktop/R_Union/moneymarket_database.csv"
file_path_comm <- "C:/Users/ASUS/Desktop/R_Union/commodity_database1.csv"
file_path_cftc <- "C:/Users/ASUS/Desktop/R_Union/cftc.csv"
file_path_credit <- "C:/Users/ASUS/Desktop/R_Union/credit_database.csv"
#file_path_rates_bdp <- "C:/Users/ASUS/Desktop/R_Union/bdp.csv"

equity_db <- fread(file_path_eq,header=T)
rates_db<-fread(file_path_rates)
vol_db<-fread(file_path_vol)
fx_db<-fread(file_path_fx,header = TRUE)
eco_db<-fread(file_path_eco,header = TRUE)
mm_db<-fread(file_path_mm,header = TRUE)
comm_db<-fread(file_path_comm,header = TRUE)[,1:3]
cftc_db<-fread(file_path_cftc,header = TRUE)
credit_db<-fread(file_path_credit,header = TRUE)
#bdp_db<-fread(file_path_rates_bdp,header = TRUE)

###build up equity_db better with COUNTRY and Name added----
suffix=c("COUNTRY","NAME")
library(countrycode)
tickers_eq <- equity_db[[2]]
w=bdp(tickers_eq,fields = c("COUNTRY_ISO","NAME","PX_LAST"))
colnames(w)=c("ticker","ISOCOUNTRY","NAME","PX_LAST")

# n=length(tickers_eq)
# field_eq=c(rep("COUNTRY_ISO",n),rep("NAME",n))
# t=get_bdp(c(tickers_eq,tickers_eq),field_eq)
w$CTY_F=countrycode(w$COUNTRY_ISO,origin = "iso2c", destination = "country.name")
w$ticker=rownames(w)
w[is.na(w$CTY_F)&w$COUNTRY_ISO=="EU",]$CTY_F="European"
w[is.na(w$CTY_F)&w$COUNTRY_ISO=="MULT",]$CTY_F="Region"
w$CTY_F %>% is.na()
w$DES=paste(w$CTY_F,w$NAME)
w1=select(w,ticker,DES)
w1=data.frame(ticker=w1$ticker,DES=w1$DES)
write.csv(w1, "equity_database.csv", row.names = FALSE)
ma#####
tickers_eq <- equity_db[[2]]  # 第二列
tickers_rates<-rates_db[[2]]
tickers_fx<-fx_db[[2]]
tickers_vol<-vol_db[[2]]
tickers_eco<-eco_db[[2]]
tickers_mm<-mm_db[[1]]
tickers_comm<-comm_db[[1]]
tickers_cftc<-cftc_db[[2]]
tickers_bdp<-bdp_db[[1]]
tickers_credit <- credit_db[[1]]

fields_eq=equity_db$Field
fields_rates=rates_db$Field
fields_fx=fx_db$Field
fields_vol=vol_db$Field
fields_eco=eco_db$Field
fields_mm=mm_db$Field
fields_comm=comm_db$Field
fields_cftc="PX_LAST"
fields_credit=credit_db$Fields
#fields_bdp=bdp_db$Fields

startDate <- as.Date("2000-01-01")
endDate   <- Sys.Date()
raw_eq=get_bbg(tickers_eq, fields_eq, startDate, endDate)
raw_rates=get_bbg(tickers_rates, fields_rates, startDate, endDate)
raw_fx=get_bbg(tickers_fx, fields_fx, startDate, endDate)
raw_vol=get_bbg(tickers_vol, fields_vol, startDate, endDate)
raw_eco=get_bbg(tickers_eco, fields_eco, startDate, endDate)
raw_mm=get_bbg(tickers_mm, fields_mm, startDate, endDate)
raw_comm=get_bbg(tickers_comm, fields_comm, startDate, endDate)
raw_cftc=get_bbg(tickers_cftc, fields_cftc, startDate, endDate)
#test
#s=sample(seq(1:nrow(credit_db)),20)
#raw_credit=get_bbg(tickers_credit[s], fields_credit[s], startDate, endDate)
raw_credit=get_bbg(tickers_credit, fields_credit, startDate, endDate)
raw_bdp=get_bdp(tickers_bdp,fields_bdp)
#raw_bdp$year <- str_extract(raw_bdp$ticker, "(?<=\\D)(\\d{1,2})(?=Y\\s)")
#write.csv(raw_bdp,file="Country.csv")

#clean up non PX_LAST
raw_rates[which(raw_rates$ticker=="LUMSTRUU Index"),c(1,4)]
raw_rates[which(raw_rates$ticker=="LUMSTRUU Index"),"PX_LAST"]=raw_rates[which(raw_rates$ticker=="LUMSTRUU Index"),c(1,4)][,2]
raw_rates=raw_rates[,-4]

#raw_rates[which(raw_rates$ticker=="GTGBP2Y Govt"),"PX_LAST"]
saveRDS(raw_eq,file = "RAW_EQ")
saveRDS(raw_rates,file="RAW_RATES")
saveRDS(raw_fx,file="RAW_FX")
# saveRDS(raw_vol,file="RAW_VOL")
#saveRDS(raw_eco,file="RAW_ECO")
# saveRDS(raw_mm,file="RAW_MM")
# saveRDS(raw_comm,file="RAW_COMM")
saveRDS(raw_credit,file="RAW_CREDIT")
saveRDS(raw_cftc,file="RAW_CFTC")
ReadRates<-readRDS(file="RAW_RATES")
ReadEq<-readRDS(file="RAW_EQ")
ReadFX<-readRDS(file="RAW_FX")
ReadVOL<-readRDS(file="RAW_VOL")
ReadECO<-readRDS(file="RAW_ECO")
ReadMM<-readRDS(file="RAW_MM")
ReadCOMM<-readRDS(file="RAW_COMM")
ReadCFTC<-readRDS(file="RAW_CFTC")
ReadCREDIT<-readRDS(file="RAW_CREDIT")



###merge credit+change ticker to DES for Credit----
merged_data_credit <- merge(ReadCREDIT,credit_db,by.x = "ticker", by.y = "ticker", all.x = TRUE)
merged_data_credit$ticker <- ifelse(!is.na(merged_data_credit$DES), merged_data_credit$DES, merged_data_credit$ticker)

###change ticker to des in csv for Equity----
# 先 merge
merged_data_eq <- merge(ReadEq, equity_db, by.x = "ticker", by.y = "ticker", all.x = TRUE)
# 用 des 替换 ticker
merged_data_eq$ticker <- ifelse(!is.na(merged_data_eq$DES), merged_data_eq$DES, merged_data_eq$ticker)

### change ticker to des in csv for Rates
# 先 merge
merged_data_rates <- merge(ReadRates, rates_db, by.x = "ticker", by.y = "BBG_Ticker", all.x = TRUE)
# 用 des 替换 ticker
merged_data_rates$ticker <- ifelse(!is.na(merged_data_rates$Name), merged_data_rates$Name, merged_data_rates$ticker)
merged_data_rates[, price := fifelse(!is.na(PX_LAST), PX_LAST,
                     fifelse(!is.na(YLD_CNV_MID), YLD_CNV_MID, NA_real_))]
merged_data_rates$PX_LAST=merged_data_rates$price

### change ticker to des in csv for Fx
# 先 merge
merged_data_fx <- merge(ReadFX, fx_db, by.x = "ticker", by.y = "BBG_Ticker", all.x = TRUE)
# 用 des 替换 ticker
merged_data_fx$ticker <- ifelse(!is.na(merged_data_fx$Name), merged_data_fx$Name, merged_data_fx$ticker)

### change ticker to des in csv for mm---
# 先 merge
merged_data_mm <- merge(ReadMM, mm_db, by.x = "ticker", by.y = "Ticker", all.x = TRUE)
# 用 des 替换 ticker
merged_data_mm$ticker <- ifelse(!is.na(merged_data_mm$des), merged_data_mm$des, merged_data_mm$ticker)
# 删除临时列 des
# merged_data$des <- NULL



### change ticker to des in csv for CFTC
# 先 merge
merged_data_cftc <- merge(ReadCFTC, cftc_db, by.x = "ticker", by.y = "ticker", all.x = TRUE,by=.EACHI)
# 用 des 替换 ticker
merged_data_cftc$ticker <- ifelse(!is.na(merged_data_cftc$DES), merged_data_cftc$DES, merged_data_cftc$ticker)
# 删除临时列 des
# merged_data$des <- NULL
---------------------------------------------------------
### change ticker to des in csv for ECO
# library(data.table)
# 
# # 假设 eco_db 是 data.table；如果不是先转换
# eco_db <- as.data.table(eco_db)
# ReadECO <- as.data.table(ReadECO)
# 
# # 找出在 eco_db 里重复的 BBG_Ticker
# dup_keys <- eco_db[, .N, by = BBG_Ticker][N > 1]
# dup_keys
# 
# # 列出这些 key 在 eco_db 的具体行（示例只看前 20 个重复的 key）
# if (nrow(dup_keys) > 0) {
#   example_dup <- dup_keys$BBG_Ticker[1:min(20, .N)]
#   eco_db[BBG_Ticker %in% example_dup][order(BBG_Ticker)]
# }
# 先 merge
merged_data_eco <- merge(ReadECO, eco_db, by.x = "ticker", by.y = "BBG_Ticker", all.x = TRUE)
# 用 des 替换 ticker
merged_data_eco$ticker <- ifelse(!is.na(merged_data_eco$Name), merged_data_eco$Name, merged_data_eco$ticker)
# 删除临时列 des

### change ticker to des in csv for Comm
library(stringi)
comm_db <- as.data.table(comm_db)

# # 方法 A：用 stringi 检测不是 UTF-8 的行（推荐）
# not_utf8_idx <- which(!stri_enc_isutf8(comm_db$des))
# length(not_utf8_idx)
# head(not_utf8_idx, 100)
# if (length(not_utf8_idx) > 0) {
#   comm_db[not_utf8_idx[1: min(10, length(not_utf8_idx))], .(Ticker, des)]
# }
# # 看原始字节，帮助判断是什么奇怪字符
# i <- 2
# raw <- charToRaw(as.character(comm_db$des[i]))
# raw
# # 也可以直接打印并用 repr 看不可见字符
# comm_db$des[i]
# fix_enc <- function(vec, from_cands = c("UTF-8","latin1","GBK","GB2312","CP936","BIG5")) {
#   # 先尝试把能直接认为是 UTF-8 的保留
#   out <- vec
#   ok <- stri_enc_isutf8(out)
#   # 对不是 UTF-8 的尝试多种来源编码
#   need_idx <- which(!ok)
#   if (length(need_idx) == 0) return(out)
#   
#   for (enc in from_cands) {
#     tmp <- iconv(vec[need_idx], from = enc, to = "UTF-8")
#     # 把成功转换（非 NA）的填回
#     succ <- !is.na(tmp)
#     if (any(succ)) {
#       out[need_idx[succ]] <- tmp[succ]
#       need_idx <- need_idx[!succ]
#     }
#     if (length(need_idx) == 0) break
#   }
#   # 对仍然无法转换的，按需处理（这里用空串）
#   if (length(need_idx) > 0) out[need_idx] <- "" 
#   return(out)
# }
# 
# # 应用到 des 和 Ticker（小心原地修改，先备份）
# comm_db_backup <- copy(comm_db)
# comm_db[, des := fix_enc(as.character(des))]
# comm_db[, Ticker := fix_enc(as.character(Ticker))]


#merged_data_comm <- merge(ReadCOMM, comm_db, by.x = "ticker", by.y = "Ticker", all.x = TRUE)
dup_keys <- comm_db[, .N, by = Ticker][N > 1] #csv duplicated row
dup_keys
dup_keys[grepl("^DOE", Ticker)]

library(data.table)
setDT(comm_db)
ReadCOMM<-readRDS(file="RAW_COMM")
# 1. 分成 DOE 开头和非 DOE 开头两部分
comm_doe <- comm_db[grepl("^DOE", Ticker)]
comm_other <- comm_db[!grepl("^DOE", Ticker)]

# 2. 对 DOE 开头的，按 ticker 分组，只保留 des 最长的一行
comm_doe_unique <- comm_doe[, .SD[which.max(nchar(des))], by = Ticker]

# 3. 合并回去
comm_db <- rbind(comm_other, comm_doe_unique)
# 保留每组 (Ticker, Field) 中 des 字符数最长的那一行（若 des 为 NA 视为空字符串；长度相同取第一条出现的）
comm_db <- comm_db[, .SD[which.max(nchar(ifelse(is.na(des), "", des)))], by = .(Ticker, Field)]


head(comm_db)
head(ReadCOMM)

# 确保是 data.table
setDT(comm_db)
setDT(ReadCOMM)

# 1) 先对 comm_db 按 (Ticker, Field) 去重：保留 des 最长的那条（防止 join 出现多对多）
comm_db_unique <- comm_db[, .SD[which.max(nchar(ifelse(is.na(des), "", des)))], by = .(Ticker, Field)]

# 2) 为 OPEN_INT / PX_SETTLE 分别做 lookup 表
open_lookup <- comm_db_unique[Field == "OPEN_INT", .(Ticker, des_open = des)]
px_lookup   <- comm_db_unique[Field == "PX_SETTLE", .(Ticker, des_px    = des)]
des_lookup  <- comm_db_unique[Field=="PX_LAST", .(Ticker, des_field = des)]

# 3) 把 lookup join 到 ReadCOMM（left join）
# 使用 on=.(Ticker) 来按 Ticker 匹配
ReadCOMM <- open_lookup[ReadCOMM, on = .(Ticker=ticker)]   # 此时会把 des_open 加到 ReadCOMM（列名在左表）
ReadCOMM <- px_lookup[ReadCOMM, on = .(Ticker)]     # 再把 des_px 加上来
ReadCOMM <- des_lookup[ReadCOMM, on = .(Ticker)] #加入 des_field（匹配双方的 Field）
# 4) 依据条件覆盖 Ticker（先处理 OPEN_INT，再处理 PX_SETTLE 覆盖）
# 保存原始 ticker 以防需要回滚
ReadCOMM[, orig_Ticker := Ticker]

# 如果 OPEN_INT 非 NA 并且有匹配的 des_open（非 NA / 非空字符串），用 des_open 覆盖
ReadCOMM[!is.na(OPEN_INT) & !is.na(des_open) & nchar(des_open) > 0, Ticker := des_open]

# 如果 PX_SETTLE 非 NA 并且有匹配的 des_px（非 NA / 非空字符串），用 des_px + " Settlement PX" 覆盖（优先级更高）
ReadCOMM[!is.na(PX_SETTLE) & !is.na(des_px) & nchar(des_px) > 0, Ticker := paste0(des_px, " Settlement PX")]

# 否则（既非 OPEN_INT 也非 PX_SETTLE），如果有 comm_db 对应 Field 的 des（des_field），就用它来替换
ReadCOMM[is.na(OPEN_INT) & is.na(PX_SETTLE) & !is.na(des_field) & nchar(des_field) > 0, Ticker := des_field]

# 5) 清理临时列（可选）
ReadCOMM[, c("des_open", "des_px","des_field") := NULL]
# 如果不想保留 orig_Ticker 也可以删掉：
setDT(ReadCOMM)

# 检查每行有多少个非 NA
ReadCOMM[, non_na_count := rowSums(!is.na(.SD)), .SDcols = c("PX_LAST", "PX_SETTLE", "OPEN_INT")]

# 找出冲突行
conflict_rows <- ReadCOMM[non_na_count > 1]

# 如果有冲突行就报错
if (nrow(conflict_rows) > 0) {
  stop(paste0("以下行存在多个非 NA 值，请检查:\n", paste(conflict_rows$Ticker, collapse = ", ")))
}

# 否则创建新列 value
ReadCOMM[, value := fifelse(!is.na(PX_LAST), PX_LAST,
                            fifelse(!is.na(PX_SETTLE), PX_SETTLE,
                                    fifelse(!is.na(OPEN_INT), OPEN_INT, NA_real_)))]

# 删除辅助列（可选）
ReadCOMM[, non_na_count := NULL]
comm_wide<- dcast(ReadCOMM, date ~ Ticker, value.var="value",fun.aggregate = mean)
saveRDS(comm_wide,file="WIDE_COMM")
ReadWideCOMM=readRDS(file="WIDE_COMM")

# plot(
#   comm_wide$date,                  # 横轴用日期列
#   comm_wide$`Nymex HO 总持仓`,               # 纵轴用目标列
#   type = "p",                      # 线图
#   xlab = "Date",                   # 横轴标签
#   ylab = "WTI 2st",                # 纵轴标签
#   main = "WTI 1st over Time",      # 图标题
#   col = "red"                     # 线条颜色（可选）
# )


# 现在 ReadCOMM 的 Ticker 已按你的规则被替换


library(dplyr)
# 
# ReadCOMM_clean <- ReadCOMM %>%
#   # 先区分出以DOE开头的行
#   mutate(is_doe = grepl("^DOE", ticker)) %>%
#   group_by(ticker) %>%
#   # 对每组ticker处理：
#   # 如果是以DOE开头的ticker，就取des最长的那一行；
#   # 否则保持原样（只保留一行）
#   filter(
#     if (all(is_doe)) {
#       nchar(des) == max(nchar(des), na.rm = TRUE)
#     } else TRUE
#   ) %>%
#   ungroup() %>%
#   select(-is_doe)


# 用 des 替换 ticker
merged_data_eco$ticker <- ifelse(!is.na(merged_data_eco$Name), merged_data_eco$Name, merged_data_eco$ticker)
# 删除临时列 des
EQ_wide <- dcast(merged_data_eq, date ~ ticker, value.var="PX_LAST")
saveRDS(EQ_wide,file="WIDE_EQ")
EQ_ReadWideEQ=readRDS(file="WIDE_EQ")
Rates_wide<- dcast(merged_data_rates, date ~ ticker, value.var="PX_LAST",fun.aggregate = mean)
### Read Wide----
saveRDS(Rates_wide,file="WIDE_RATES")
ReadWideRATES=readRDS(file="WIDE_RATES")
fx_wide<- dcast(merged_data_fx, date ~ ticker, value.var="PX_LAST",fun.aggregate = mean)
saveRDS(fx_wide,file="WIDE_FX")
ReadWideFX=readRDS(file="WIDE_FX")
vol_wide<- dcast(ReadVOL, date ~ ticker, value.var="PX_LAST")
saveRDS(vol_wide,file="WIDE_VOL")
ReadWideVOL=readRDS(file="WIDE_VOL")
eco_wide<- dcast(merged_data_eco, date ~ ticker, value.var="PX_LAST")
saveRDS(eco_wide,file="WIDE_ECO")
ReadWideECO=readRDS(file="WIDE_ECO")
mm_wide<- dcast(merged_data_mm, date ~ ticker, value.var="PX_LAST")
saveRDS(mm_wide,file="WIDE_MM")
ReadWideMM=readRDS(file="WIDE_MM")
cftc_wide<- dcast(merged_data_cftc, date ~ ticker, value.var="PX_LAST",fun.aggregate=mean)
saveRDS(cftc_wide,file="WIDE_CFTC")
ReadWideCFTC=readRDS(file="WIDE_CFTC")
credit_wide <- dcast(merged_data_credit,date ~ ticker, value.var="PX_LAST",fun.aggregate=mean)
saveRDS(credit_wide,file="WIDE_CREDIT")
ReadWideCREDIT=readRDS("WIDE_CREDIT")
###test credit data----
t=ReadWideCREDIT
tickers=names(t)[2:length(names(t))]

# names(comm_wide)[colSums(!is.na(comm_wide)) == 0]
# na_count <- data.frame(
#   Column = names(comm_wide)[-1],
#   NA_Count = colSums(is.na(comm_wide[,-1, drop = FALSE]))
# )
# na_count <- na_count[order(-na_count$NA_Count), ]
# print(na_count)

#####
gov_bond_tickers<- c(
  # US Treasuries
  "USGG2YR Index",     # US Treasury 2Y Yield
  "USGG5YR Index",     # US Treasury 5Y Yield
  "USGG10YR Index",    # US Treasury 10Y Yield
  "USGG30YR Index",    # US Treasury 30Y Yield
  
  # German Bunds
  "GDBR2 Index",       # German Bund 2Y Yield
  "GDBR5 Index",       # German Bund 5Y Yield
  "GDBR10 Index",      # German Bund 10Y Yield
  
  # France OAT
  "GFRN10 Index",      # French OAT 10Y Yield
  
  # Japan JGB
  "GTJPY2Y Govt",      # JGB 2Y Yield
  "GTJPY10Y Govt",     # JGB 10Y Yield
  "GTJPY30Y Govt",      # JGB 30Y Yield
  
  # UK Gilts
  "GTGBP2Y Govt",      # UK Gilt 2Y Yield
  "GTGBP5Y Govt",      # UK Gilt 5Y Yield
  "GTGBP10Y Govt",     # UK Gilt 10Y Yield
  
  # Switzerland
  "GTCHF2Y Govt",      # Swiss Govt 2Y Yield
  "GTCHF5Y Govt",      # Swiss Govt 5Y Yield
  "GTCHF10Y Govt",     # Swiss Govt 10Y Yield
  
  # China Govt Bonds
  "GTCNY2Y Govt",      # China Govt Bond 2Y
  "GTCNY5Y Govt",      # China Govt Bond 5Y
  "GTCNY10Y Govt",     # China Govt Bond 10Y
  
  # China Development Bank (Policy Bank)
  "GCDB2YR Index",     # CDB 2Y
  "GCDB5YR Index",     # CDB 5Y
  "GCDB10YR Index",    # CDB 10Y
  
  # Indonesia Govt Bonds
  "GTIDR2Y Govt",      # Indonesia Govt Bond 2Y
  "GTIDR5Y Govt",      # Indonesia Govt Bond 5Y
  "GTIDR10Y Govt"      # Indonesia Govt Bond 10Y
)


#Goverment group
gov_bond=Rates_wide[,gov_bond_tickers]

###----------process eco_wide data
eco_wide_v0=readRDS(file="WIDE_ECO")
eco_wide=eco_wide_v0
cols <- setdiff(names(eco_wide), "date")
eco_wide[, (cols) := lapply(.SD, function(x) na.locf(x, na.rm = FALSE)), .SDcols = cols]

cols <- setdiff(names(eco_wide), "date")  # 排除日期列
eco_z=eco_wide[, (cols) := lapply(.SD, function(x) {
  mu <- mean(x, na.rm = TRUE)
  s  <- sd(x, na.rm = TRUE)
  (x - mu) / s
}), .SDcols = cols]

# --- 
###----------optimize plot with date adjusted
plot_macro <- function(df, col_name, type="o", ...) {
  # df: 宽表，每列一个指标，第一列是Date
  # col_name: 要画的列名（字符串）
  # type: plot 类型，默认点线图
  # ...: 其他 plot 参数
  
  # 提取数据
  x <- df[[col_name]]
  date <- df[[1]]  # 假设第一列是日期
  
  # 找到第一个和最后一个非NA索引
  valid_idx <- which(!is.na(x))
  if(length(valid_idx) == 0) {
    warning("列全是 NA，无法绘图")
    return(NULL)
  }
  
  start <- min(valid_idx)
  end   <- max(valid_idx)
  
  # 截取有效数据
  x_plot <- x[start:end]
  date_plot <- date[start:end]
  
  # 绘图
  plot(date_plot, x_plot,
       type = type,
       xlab = "Date",
       ylab = col_name,
       main = col_name,
       ...)
}
plot_macro(ReadWideCOMM,colnames(ReadWideCOMM)[200])
par(mfrow=c(1,2))
plot_macro(ReadWideCFTC,colnames(ReadWideCFTC)[300])
plot_macro(ReadWideCFTC,colnames(ReadWideCFTC)[350])
locator()
edit(ReadWideECO)

###-----------CFTC Data
# 需要的包
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")

library(readxl)
library(stringr)

# --- 配置 ----
path_in  <- "C:/Users/ASUS/Desktop/R_Union/CFTCBBG.xlsx"
path_out <- "C:/Users/ASUS/Desktop/R_Union/CFTCBBG_output.csv"

# --- 读取整个sheet（不强制把第一行当header，方便按行号处理） ---
# col_names = FALSE 让我们可以用行/列索引精确取值
df <- read_excel(path_in, col_names = FALSE)

# 索引设置（按你说明）
des_col     <- 2
ticker_cols <- c(3,7:28)

# 检查文件是否至少有这些列/行
if (ncol(df) < max(ticker_cols)) stop("Excel 列数不足，确认第7到第28列存在。")
if (nrow(df) < 2) stop("Excel 行数不足，至少需要两行（因为要读取第二行作为拼接文本）。")

#---取出第2行第7:28列作为每个 ticker 列需要拼接的 text（按你的要求） ----
append_texts <- df[2, ticker_cols] %>% unlist() %>% as.character()
# 把 NA 替换成空字符串，避免 paste 出现 NA
append_texts[is.na(append_texts)] <- ""

## --- 从第2行开始（包含第2行）循环每一行，把第7:28列展开成多行 ----
out_list <- list()
out_idx <- 1

for (i in 3:nrow(df)) {
  des_i <- as.character(df[i, des_col])
  if (is.na(des_i)) des_i <- ""            # 若 DES 是 NA，换成空字符串
  for (k in seq_along(ticker_cols)) {
    col_idx <- ticker_cols[k]
    ticker_val <- as.character(df[i, col_idx])
    # 跳过空或 NA 的 ticker
    if (is.na(ticker_val) || str_trim(ticker_val) == "") next
    
    # 第一列拼接：des + 第2行对应列的 text（中间加一个空格）
    DES_combined <- str_trim(paste(des_i, append_texts[k]))
    
    out_list[[out_idx]] <- data.frame(
      DES = DES_combined,
      ticker = ticker_val,
      stringsAsFactors = FALSE
    )
    out_idx <- out_idx + 1
  }
}

## 合并并写 CSV----
if (length(out_list) == 0) {
  warning("没有有效的 ticker 被找到，未生成 CSV。")
} else {
  out_df <- do.call(rbind, out_list)
  write.csv(out_df, path_out, row.names = FALSE, fileEncoding = "UTF-8")
  message("完成：已写出 CSV 到 ", path_out)
}

###ECO-bbg-template ticker list for ALLX.csv----
setwd("C:/Users/ASUS/Desktop/R_Union")
AX <- "C:/Users/ASUS/Desktop/R_Union/ALLX.csv"
t=fread(AX,header=T)
v <- unlist(t, use.names = FALSE)
v <- v[!is.na(v) & v != ""]
v1=unique(v[grepl("Curncy|Index", v)])
fields=c("COUNTRY_FULL_NAME","LONG_COMP_NAME")
x=bdp(v1,fields)
x1=x
x$ticker=rownames(x)
x$DES=paste0(x$COUNTRY_FULL_NAME," ",x$LONG_COMP_NAME)
setDT(x)
x[, DES := sapply(DES, \(x) {
  # 拆分单词
  words <- unlist(strsplit(x, "\\s+"))
  # 去重（不分大小写），但保留第一个出现的版本
  unique_words <- words[!duplicated(tolower(words))]
  # 拼回字符串
  paste(unique_words, collapse = " ")
})]
x2=x[!rowSums(!(is.na(x) | x == "")) == 0, ]
x2$field="PX_LAST"
# x3=get_bbg(x2$ticker,x2$field,startDate,endDate)
saveRDS(x3,file="RAW_ALLX")
x3=readRDS(file="RAW_ALLX")
merged_data_ALLX <- merge(x3,x2,by.x = "ticker", by.y = "ticker", all.x = TRUE)
# 用 des 替换 ticker
merged_data_ALLX$ticker <- ifelse(!is.na(merged_data_ALLX$DES), merged_data_ALLX$DES, merged_data_ALLX$ticker)
# 删除临时列 des
ALLX_wide <- dcast(merged_data_ALLX, date ~ ticker, value.var="PX_LAST",fun.aggregate = mean)
saveRDS(ALLX_wide,file="WIDE_ALLX")
ALLX_wide=readRDS("WIDE_ALLX")
y <- as.numeric(ALLX_wide$`UNITED STATES University of Michigan Consumer Sentiment Index`)
str(y)

plot(ALLX_wide$date,y,col=rainbow(nrow(ALLX_wide)),pch=19,cex=1.1)

# 去掉 NA/NAN，避免 rank 出问题
valid <- !is.na(y) & !is.nan(y)
y_valid <- y[valid]

# 按 y 排名生成颜色
cols <- rainbow(length(y_valid))
cols_by_y <- cols[rank(y_valid)]

# 绘图，保留你原来的风格
plot(ALLX_wide$date[valid], y_valid, 
     col = cols_by_y, 
     pch = 19, 
     cex = 1.1,
     xlab = "Date", ylab = "Value",
     main = "Consumer Sentiment Index")
for(i in 1:(length(y_valid)-1)) {
  lines(ALLX_wide$date[valid][i:(i+1)], y_valid[i:(i+1)], col = cols_by_y[i], lwd = 2)
}
x0 <- as.Date("2024-06-01")
y0 <- 85

# 添加高亮点
points(x0, y0, pch=19, cex=2, col="red", lwd=2)
text(x0, y0, labels="Target", pos=3, col="red")
# locator()
hist(y,col=rainbow(10))
hist(y)


###CARRY and ROLL Data----
library(readxl)
library(stringr)
# 文件路径
path <- "C:/Users/ASUS/Desktop/R_Union/bbg excel/USDZeroRateTickers.xlsx"

# 读取 sheet "ALL" 的第二列
data <- read_excel(path, sheet = "ALL", range = cell_cols("B:B"))

ticker=data$ticker
ticker_filtered <- ticker[str_detect(ticker, "^(YCGT|YCSW)")]
w=bdp(ticker_filtered,c("LONG_COMP_NAME","COUNTRY_FULL_NAME","CRNCY"))
saveRDS(w,"ALLX_w")
setwd("C:/Users/ASUS/Desktop/R_Union")
w=readRDS("ALLX_w")
w$ticker=rownames(w)
w <- w[order(w$ticker),]
setDT(w)
w[, DES := paste(CRNCY,COUNTRY_FULL_NAME,LONG_COMP_NAME)]
w[, DES := sapply(strsplit(DES, "\\s+"), function(words) {
  # 按不区分大小写去重
  unique_words <- words[!duplicated(tolower(words))]
  paste(unique_words, collapse = " ")
})]
saveRDS(w,"ALLX_w")
# w[str_detect(w$LONG_COMP_NAME,"vs"),]

zerotickerfilter <- c("ois", "Basis", "Sovereign", "CCS", "Swap", "IRS","Treasury","vs")

# 合成正则表达式，并忽略大小写匹配
pattern <- str_c(zerotickerfilter, collapse = "|")

w_filtered <- w[str_detect(w$LONG_COMP_NAME, regex(pattern, ignore_case = TRUE)),]
w_others <- w[!str_detect(w$LONG_COMP_NAME, regex(pattern, ignore_case = TRUE)),]
wf=copy(w_filtered)
wf[, test := paste0(
  substr(ticker, 3, 3),        # 第3个字符
  substr(ticker, 5, 8), "Z",          # 第5到8个字符
  " 1Y BLC2 Curncy"              # 拼上固定字符串
)]
test_result=bdp(wf$test,c("LAST_UPDATE_DT"))
saveRDS(test_result,"TEST_RESULT")
test_result=readRDS("TEST_RESULT")
test_result$ticker=rownames(test_result)
wf_merged=merge(wf,test_result,by.x="test",by.y="ticker",all.x = T)
valid=!is.na(wf_merged$LAST_UPDATE_DT)
nw=wf_merged[valid,]
bdhcurveticker=wf_merged[valid,"ticker"]
#swap curve
bdhcurves <- bdhcurveticker[str_detect(bdhcurveticker$ticker, "YCSW")]
#soverign curve
bdhcurveg <- bdhcurveticker[str_detect(bdhcurveticker$ticker, "YCGT")]
cs=c("1W","2W","1M","2M","3M","6M","1Y","15M","18M","2Y","3Y","4Y","5Y","6Y",
     "7Y","8Y","9Y","10Y","12Y","14Y","16Y","18Y","20Y","25Y","30Y")
for (c in cs) {
  colname <- paste0(c, " ticker")  # 新列名
  bdhcurves[, (colname) := paste0(
    substr(ticker, 3, 3),       # 第3个字符
    substr(ticker, 5, 8),       # 第5到8个字符
    "Z ",                       # 固定 "Z "
    c,                          # 当前期限
    " BLC2 Curncy"              # 固定后缀
  )]
}
cols_to_melt <- names(bdhcurves)[2:ncol(bdhcurves)]

# melt 成短格式
bdhcurves_melt <- melt(
  bdhcurves[,2:ncol(bdhcurves)],
  measure.vars = cols_to_melt,
  variable.name = "cs_col",  # 原列名存放在这里
  value.name = "ticker_short" # 新列，存放 ticker 字符串
)
t=bdhcurves_melt$ticker_short
tenor=bdp(t,"MTY_YEARS_TDY")
midt=bdp(t,"PX_MID") # only latest not time series
test=bdh(t,"PX_LAST",as.Date("2024-01-11"),as.Date("2024-09-15"))
saveRDS(tenor,"ALL_TENOR")
saveRDS(midt,"ALL_MID_ZERO_RATE")
tenor=readRDS("ALL_TENOR")
midt=readRDS("ALL_MID_ZERO_RATE")
# t1=get_bbg(t[1:20],"PX_LAST",startDate,endDate)
# fields=rep("PX_MID",length(t))[1:20]
# saveRDS(t1,"SWAPZERO")
# readRDS(t1)$
tenor$ticker=rownames(tenor)
midt$ticker=rownames(midt)
merged <- left_join(midt,tenor,by="ticker")
new_merged <- merged %>% 
  mutate(key=substr(ticker,1,5)) %>% 
  left_join(
    wf %>% 
      mutate(key=substr(test,1,5)) %>% 
      group_by(key) %>% 
      slice(1) %>% 
      select(key,CRNCY,COUNTRY_FULL_NAME,DES),
    by="key"
  ) %>% 
  select(-key)
nrow(new_merged)
filter(new_merged,substr(ticker,1,5)=="S0004")# test
saveRDS(new_merged,"ZERORATE_CURVE")
