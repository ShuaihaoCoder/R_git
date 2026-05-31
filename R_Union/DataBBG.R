#equity
library(data.table)

# ===============================
# Equity 指标列表
# ===============================
equity_names <- c(
  "S&P 500 Index",
  "Nasdaq 100 Index",
  "CSI 300 Index",
  "Shanghai & Shenzhen Composite Index",
  "Hang Seng Index",
  "Nikkei 225 Index",
  "TOPIX Index",
  "KOSPI Index",
  "Russell 2000 Index",
  "FTSE 100 Index",
  "DAX Index"
)

# ===============================
# 对应 Bloomberg Ticker
# ===============================
equity_bbg_tickers <- c(
  "SPX Index",
  "NDX Index",
  "CSI300 Index",
  "SHSZ300 Index",  # 通常用 CSI 300 作为 SHSZ Composite
  "HSI Index",
  "NKY Index",
  "TPX Index",
  "KOSPI Index",
  "RUT Index",
  "UKX Index",
  "DAX Index"
)

# ===============================
# 国家和分类
# ===============================
equity_country <- c("US","US","China","China","HK","JP","JP","KR","US","UK","DE")
equity_category <- rep("Equity", length(equity_names))
equity_field <- rep("PX_LAST", length(equity_names))

# ===============================
# 生成 CSV
# ===============================
equity_db <- data.table(
  Name = equity_names,
  BBG_Ticker = equity_bbg_tickers,
  Field = equity_field,
  Country = equity_country,
  Category = equity_category
)

# ===============================
# 保存 CSV
# ===============================
output_path <- "C:/Users/ASUS/Desktop/R_Union/equity_database.csv"
fwrite(equity_db, output_path)
cat("Equity CSV 模板已生成:", output_path, "\n")

#Rates
library(data.table)

# ===============================
# Rates 指标列表
# ===============================
rates_names <- c(
  "US Treasury 2Y Yield","US Treasury 5Y Yield","US Treasury 10Y Yield","US Treasury 30Y Yield",
  "US OIS 2Y","US OIS 5Y","US OIS 10Y","US OIS 30Y",
  "US MBS Option-Adjusted Spread","US 5Y Real Yield","US 10Y Real Yield","US 2Y Real Yield",
  "US 5Y5Y Inflation Breakeven","US 10Y Swap Spread",
  "German Bund 2Y Yield","German Bund 5Y Yield","German Bund 10Y Yield",
  "French OAT 10Y Yield",
  "Japanese JGB 10Y Yield","Japanese JGB 2Y Yield","Japanese JGB 30Y Yield",
  "EUR 2Y Swap Rate","EUR 10Y Swap Rate",
  "UK Gilt 2Y Yield","UK Gilt 5Y Yield","UK Gilt 10Y Yield",
  "Swiss 2Y Swap Rate","Swiss 5Y Swap Rate","Swiss 10Y Swap Rate",
  "AUD 2Y Swap Rate","AUD 10Y Swap Rate","AUD BBSW 2Y","AUD BBSW 10Y",
  "CAD 2Y Swap Rate","CAD 10Y Swap Rate",
  "China Govt Bond 2Y","China Govt Bond 5Y","China Govt Bond 10Y",
  "China Development Bank 2Y","China Development Bank 5Y","China Development Bank 10Y",
  "CNY 2Y IRS","CNY 5Y IRS","CNY 10Y IRS",
  "CNY 2Y Non-Deliverable IRS","CNY 5Y Non-Deliverable IRS","CNY 10Y Non-Deliverable IRS",
  "KRW 2Y CD IRS","KRW 5Y CD IRS",
  "Indonesia Govt Bond 2Y","Indonesia Govt Bond 5Y","Indonesia Govt Bond 10Y",
  "INR 2Y NDOIS","INR 5Y NDOIS","INR 10Y NDOIS",
  "US 144A New Issue","US Registered New Issue",
  "CNY Shibor 5Y",
  "HKD 2Y IRS","HKD 5Y IRS","HKD 10Y IRS",
  "SGD 2Y OIS","SGD 5Y OIS","SGD 10Y OIS",
  "JPY 2Y OIS","JPY 5Y OIS","JPY 10Y OIS"
)

# ===============================
# Bloomberg Ticker（已知常用）
# ===============================
rates_bbg_tickers <- c(
  "USGG2YR Index","USGG5YR Index","USGG10YR Index","USGG30YR Index",
  "USSW2 Curncy","USSW5 Curncy","USSW10 Curncy","USSW30 Curncy",
  "MBS Index","US5YREY Index","US10YREY Index","US2YREY Index",
  "US5Y5YBEI Index","US10YSS Index",
  "GDBR2 Index","GDBR5 Index","GDBR10 Index",
  "FRTR10 Index",
  "JGB10YR Index","JGB2YR Index","JGB30YR Index",
  "EUR002Y Curncy","EUR010Y Curncy",
  "GUKG2 Index","GUKG5 Index","GUKG10 Index",
  "CHSW2 Curncy","CHSW5 Curncy","CHSW10 Curncy",
  "AUD002Y Curncy","AUD010Y Curncy","BBSW2 Curncy","BBSW10 Curncy",
  "CAD002Y Curncy","CAD010Y Curncy",
  "CGB2 Govt","CGB5 Govt","CGB10 Govt",
  "CDB2 Govt","CDB5 Govt","CDB10 Govt",
  "CNY2 IRS","CNY5 IRS","CNY10 IRS",
  "CNY2 NDF","CNY5 NDF","CNY10 NDF",
  "KRW2 IRS","KRW5 IRS",
  "ID2 Govt","ID5 Govt","ID10 Govt",
  "IN2 NDF","IN5 NDF","IN10 NDF",
  "US144A Index","USREG Index",
  "SHIBOR5 Curncy",
  "HKD2 IRS","HKD5 IRS","HKD10 IRS",
  "SGD2 OIS","SGD5 OIS","SGD10 OIS",
  "JPY2 OIS","JPY5 OIS","JPY10 OIS"
)

# ===============================
# Country 和 Category
# ===============================
rates_country <- c(
  rep("US",14), rep("DE",3),"FR",rep("JP",3),rep("EU",2),rep("UK",3),rep("CH",3),
  rep("AU",4),rep("CA",2),
  rep("China",6),rep("China",3),rep("China",3),rep("KR",2),
  rep("ID",3),rep("IN",3),
  rep("US",2),"China",
  rep("HK",3),rep("SG",3),rep("JP",3)
)

rates_category <- rep("Rates", length(rates_names))
rates_field <- rep("PX_LAST", length(rates_names))

# ===============================
# 生成 CSV
# ===============================
rates_db <- data.table(
  Name = rates_names,
  BBG_Ticker = rates_bbg_tickers,
  Field = rates_field,
  Country = rates_country,
  Category = rates_category
)

# ===============================
# 保存 CSV
# ===============================
output_path <- "C:/Users/ASUS/Desktop/R_Union/rates_database.csv"
fwrite(rates_db, output_path)
cat("Rates CSV 模板已生成:", output_path, "\n")

#FX
# 设置工作目录
setwd("C:/Users/ASUS/Desktop/R_Union")

# 1. FX Names
fx_names <- c(
  "EURUSD","USDJPY","GBPUSD","AUDUSD","USDCAD","NZDUSD","USDCHF","USDNOK",
  "USDCNH","USDSGD","USDTRY","USDHKD","USDZAR","USDMXN",
  "USDCNY 3m NDF","USDKRW 3m NDF","USDTWD 3m NDF","USDIDR 3m NDF","USDINR 3m NDF","USDPHP 3m NDF",
  "DXY",
  
  "EURUSD 3m forward point","USDJPY 3m forward point","GBPUSD 3m forward point",
  "AUDUSD 3m forward point","USDCAD 3m forward point","NZDUSD 3m forward point",
  "USDCHF 3m forward point","USDNOK 3m forward point","USDCNH 3m forward point",
  "USDSGD 3m forward point","USDTRY 3m forward point","USDHKD 3m forward point",
  "USDZAR 3m forward point","USDMXN 3m forward point",
  "USDCNY 3m NDF","USDKRW 3m NDF","USDTWD 3m NDF","USDIDR 3m NDF","USDINR 3m NDF","USDPHP 3m NDF",
  
  "EURUSD 3m implied yield","USDJPY 3m implied yield","GBPUSD 3m implied yield",
  "AUDUSD 3m implied yield","USDCAD 3m implied yield","NZDUSD 3m implied yield",
  "USDCHF 3m implied yield","USDNOK 3m implied yield","USDCNH 3m implied yield",
  "USDSGD 3m implied yield","USDTRY 3m implied yield","USDHKD 3m implied yield",
  "USDZAR 3m implied yield","USDMXN 3m implied yield",
  "USDCNY 3m NDF implied yield","USDKRW 3m NDF implied yield","USDTWD 3m NDF implied yield",
  "USDIDR 3m NDF implied yield","USDINR 3m NDF implied yield","USDPHP 3m NDF implied yield",
  
  "EURUSD 6m forward point","USDJPY 6m forward point","GBPUSD 6m forward point",
  "AUDUSD 6m forward point","USDCAD 6m forward point","NZDUSD 6m forward point",
  "USDCHF 6m forward point","USDNOK 6m forward point","USDCNH 6m forward point",
  "USDSGD 6m forward point","USDTRY 6m forward point","USDHKD 6m forward point",
  "USDZAR 6m forward point","USDMXN 6m forward point",
  "USDCNY 6m NDF","USDKRW 6m NDF","USDTWD 6m NDF","USDIDR 6m NDF","USDINR 6m NDF","USDPHP 6m NDF",
  
  "EURUSD 6m implied yield","USDJPY 6m implied yield","GBPUSD 6m implied yield",
  "AUDUSD 6m implied yield","USDCAD 6m implied yield","NZDUSD 6m implied yield",
  "USDCHF 6m implied yield","USDNOK 6m implied yield","USDCNH 6m implied yield",
  "USDSGD 6m implied yield","USDTRY 6m implied yield","USDHKD 6m implied yield",
  "USDZAR 6m implied yield","USDMXN 6m implied yield",
  "USDCNY 6m NDF implied yield","USDKRW 6m NDF implied yield","USDTWD 6m NDF implied yield",
  "USDIDR 6m NDF implied yield","USDINR 6m NDF implied yield","USDPHP 6m NDF implied yield",
  
  "EURUSD 12m forward point","USDJPY 12m forward point","GBPUSD 12m forward point",
  "AUDUSD 12m forward point","USDCAD 12m forward point","NZDUSD 12m forward point",
  "USDCHF 12m forward point","USDNOK 12m forward point","USDCNH 12m forward point",
  "USDSGD 12m forward point","USDTRY 12m forward point","USDHKD 12m forward point",
  "USDZAR 12m forward point","USDMXN 12m forward point",
  "USDCNY 12m NDF","USDKRW 12m NDF","USDTWD 12m NDF","USDIDR 12m NDF","USDINR 12m NDF","USDPHP 12m NDF",
  
  "EURUSD 12m implied yield","USDJPY 12m implied yield","GBPUSD 12m implied yield",
  "AUDUSD 12m implied yield","USDCAD 12m implied yield","NZDUSD 12m implied yield",
  "USDCHF 12m implied yield","USDNOK 12m implied yield","USDCNH 12m implied yield",
  "USDSGD 12m implied yield","USDTRY 12m implied yield","USDHKD 12m implied yield",
  "USDZAR 12m implied yield","USDMXN 12m implied yield",
  "USDCNY 12m NDF implied yield","USDKRW 12m NDF implied yield","USDTWD 12m NDF implied yield",
  "USDIDR 12m NDF implied yield","USDINR 12m NDF implied yield","USDPHP 12m NDF implied yield",
  
  "EURUSD 2w forward point","USDJPY 2w forward point","GBPUSD 2w forward point",
  "AUDUSD 2w forward point","USDCAD 2w forward point","NZDUSD 2w forward point",
  "USDCHF 2w forward point","USDNOK 2w forward point","USDCNH 2w forward point",
  "USDSGD 2w forward point","USDTRY 2w forward point","USDHKD 2w forward point",
  "USDZAR 2w forward point","USDMXN 2w forward point",
  
  "EURUSD  Carry Return","USDJPY  Carry Return","GBPUSD  Carry Return",
  "AUDUSD  Carry Return","USDCAD  Carry Return","NZDUSD  Carry Return",
  "USDCHF  Carry Return","USDNOK  Carry Return","USDCNH  Carry Return",
  "USDSGD  Carry Return","USDTRY  Carry Return","USDHKD  Carry Return",
  "USDZAR  Carry Return","USDMXN  Carry Return"
)

# 2. 对应 Bloomberg Ticker
bbg_tickers <- paste0(gsub(" ", "", fx_names), " Curncy")
bbg_tickers[fx_names=="DXY"] <- "DXY Index"

# 3. Field 列
fields <- rep("PX_LAST", length(fx_names))

# 4. Country 列（手工一条条对应非美元货币所在国家）
countries <- c(
  # Spot/Index
  "EU","JP","UK","AU","CA","NZ","CH","NO",
  "CN","SG","TR","HK","ZA","MX",
  "CN","KR","TW","ID","IN","PH",
  "US",
  
  # 3m forward point
  "EU","JP","UK","AU","CA","NZ","CH","NO",
  "CN","SG","TR","HK","ZA","MX",
  "CN","KR","TW","ID","IN","PH",
  
  # 3m implied yield
  "EU","JP","UK","AU","CA","NZ","CH","NO",
  "CN","SG","TR","HK","ZA","MX",
  "CN","KR","TW","ID","IN","PH",
  
  # 6m forward point
  "EU","JP","UK","AU","CA","NZ","CH","NO",
  "CN","SG","TR","HK","ZA","MX",
  "CN","KR","TW","ID","IN","PH",
  
  # 6m implied yield
  "EU","JP","UK","AU","CA","NZ","CH","NO",
  "CN","SG","TR","HK","ZA","MX",
  "CN","KR","TW","ID","IN","PH",
  
  # 12m forward point
  "EU","JP","UK","AU","CA","NZ","CH","NO",
  "CN","SG","TR","HK","ZA","MX",
  "CN","KR","TW","ID","IN","PH",
  
  # 12m implied yield
  "EU","JP","UK","AU","CA","NZ","CH","NO",
  "CN","SG","TR","HK","ZA","MX",
  "CN","KR","TW","ID","IN","PH",
  
  # 2w forward point
  "EU","JP","UK","AU","CA","NZ","CH","NO",
  "CN","SG","TR","HK","ZA","MX",
  
  # Carry Return
  "EU","JP","UK","AU","CA","NZ","CH","NO",
  "CN","SG","TR","HK","ZA","MX"
)

# 5. Category
category <- rep("FX", length(fx_names))

# 6. 合并成 data.frame
fx_df <- data.frame(
  Name = fx_names,
  BBG_Ticker = bbg_tickers,
  Field = fields,
  Country = countries,
  Category = category,
  stringsAsFactors = FALSE
)

# 7. 保存 CSV
write.csv(fx_df, "FX_Data.csv", row.names = FALSE)

cat("CSV 文件已保存到 C:/Users/ASUS/Desktop/R_Union/FX_Data.csv\n")
############Econmic#########
# Bloomberg economic tickers database in R

# 定义数据框
econ_data <- data.frame(
  Name = c(
    "US CPI YoY","US CPI MoM","US Core CPI YoY","US Core CPI MoM",
    "US PCE YoY","US PCE MoM","US PPI YoY","US GDP YoY",
    "US Non-Farm Payroll","US Unemployment Rate","US Initial Jobless Claims",
    "US Consumption","US Trade Balance","US Capital Flow","US Fiscal Deficit",
    "US Housing Price","US MBA Mortgage Applications","US Confidence Index",
    "US 30yr Mortgage Rate","US Durable Orders ex-Transport YoY","US Retail Sales",
    "US PMI Manufacturing","US Labor Force Participation","US Housing Starts",
    "US New Home Sales","US Building Permits","US Existing Home Sales",
    "US NAHB Housing Index","US Corporate Expenditure",
    "France PMI","German PMI","UK PMI","EU CPI YoY","EU CPI YoY Harmonised",
    "EU CPI MoM","EU ZEW Sentiment","EU PPI","EU GDP YoY",
    "Eurozone Retail Sales","Eurozone Consumer Confidence","UK Housing Price",
    "JP Industrial Production MoM","JP PPI YoY","JP CPI YoY","JP Core CPI YoY",
    "JP BoP Current Account Balance","JP Retail Sales","JP GDP YoY",
    "JP Jobs to Applicant Ratio","JP Tankan Manufacturing Index",
    "China CPI","China PPI","China PMI","China M2 YoY","China Trade Balance",
    "China FX Reserve","China Total Social Financing","China Industrial Production",
    "China GDP YoY","China Fixed Asset Investment","China Retail Sales",
    "China Housing Price","China New Yuan Loans",
    "AUD CPI YoY","NZD CPI YoY","KRW CPI YoY","CAD CPI YoY","NOK CPI YoY",
    "CHF CPI YoY","India CPI YoY","Indonesia CPI YoY","Taiwan CPI YoY",
    "PHP CPI YoY","ZAR CPI YoY","TRY CPI YoY","Thailand CPI YoY",
    "AUD PPI YoY","NZD PPI YoY","KRW PPI YoY","CAD PPI YoY","NOK PPI YoY",
    "CHF PPI YoY","India PPI YoY","Indonesia PPI YoY","Taiwan PPI YoY",
    "PHP PPI YoY","ZAR PPI YoY","TRY PPI YoY","Thailand PPI YoY",
    "AUD GDP YoY","NZD GDP YoY","KRW GDP YoY","CAD GDP YoY","NOK GDP YoY",
    "CHF GDP YoY","India GDP YoY","Indonesia GDP YoY","Taiwan GDP YoY",
    "PHP GDP YoY","ZAR GDP YoY","TRY GDP YoY","Thailand GDP YoY",
    "AUD Unemployment Rate","NZD Unemployment Rate","KRW Unemployment Rate",
    "CAD Unemployment Rate","NOK Unemployment Rate","CHF Unemployment Rate",
    "India Unemployment Rate","Indonesia Unemployment Rate","Taiwan Unemployment Rate",
    "PHP Unemployment Rate","ZAR Unemployment Rate","TRY Unemployment Rate",
    "Thailand Unemployment Rate"
  ),
  BBG_Ticker = c(
    "CPI YOY Index","CPI CHNG Index","CCPI YOY Index","CCPI CHNG Index",
    "PCE YOY Index","PCE CHNG Index","PPI YOY Index","USGDPYOY Index",
    "NFP TCH Index","USURTOT Index","INJCJC Index",
    "USCONS Index","USTBTOT Index","USPORTF Index","USFISDEF Index",
    "SPCS20Y% Index","MBAVPRCH Index","CONCCONF Index",
    "USMM30Y Index","DGNOXTCH Index","RSXFSN Index",
    "NAPMPMI Index","CIVPART Index","HSTTOT Index",
    "NHSLTOT Index","BPNTTOT Index","ETSLTOT Index",
    "NHAB Index","USCEXP Index",
    "PMIFRFA Index","PMIDE Index","PMIGB Index","ECCPESTY Index","ECCPEMUY Index",
    "ECCPESTM Index","GRZEWI Index","EPPPTOTY Index","EUGNEMUQ Index",
    "RSXEU Index","ECCONC Index","UKHPIYOY Index",
    "JNIPMOM Index","JPPPIYOY Index","JCPYOY Index","JCPXCPY Index",
    "JNBOPBAL Index","JNRSYOY Index","JNGDPYOY Index",
    "JNJNJAR Index","JNKTMI Index",
    "CNCPYOY Index","CNPPYOY Index","NAPMPMI Index","M2YOY Index","CNTRBAL Index",
    "CNFOREX Index","CNTFYOY Index","CNIPYOY Index",
    "CNGDPYOY Index","CNFAYOY Index","CNRSYOY Index",
    "CNHPYOY Index","CNNYL Index",
    "AUCPIYOY Index","NZCPIYOY Index","KRCPIYOY Index","CACPIYOY Index","NOCPIYOY Index",
    "CHFCPIYOY Index","INCPIYOY Index","IDCPIYOY Index","TWCPIYOY Index",
    "PHCPIYOY Index","ZACPIYOY Index","TRCPIYOY Index","THCPIYOY Index",
    "AUPPIYOY Index","NZPPIYOY Index","KRPPIYOY Index","CAPPIYOY Index","NOPPIYOY Index",
    "CHFPPIYOY Index","INPPIYOY Index","IDPPIYOY Index","TWPPIYOY Index",
    "PHPPPIYOY Index","ZAPPIYOY Index","TRPPIYOY Index","THPPIYOY Index",
    "AUGDPYOY Index","NZGDPYOY Index","KRGDPYOY Index","CAGDPYOY Index","NOGDPYOY Index",
    "CHFGDPYOY Index","INGDPYOY Index","IDGDPYOY Index","TWGDPYOY Index",
    "PHGDPYOY Index","ZAGDPYOY Index","TRGDPYOY Index","THGDPYOY Index",
    "AUURTOT Index","NZURTOT Index","KRURTOT Index",
    "CAURTOT Index","NOURTOT Index","CHFURTOT Index",
    "INURTOT Index","IDURTOT Index","TWURTOT Index",
    "PHURTOT Index","ZAURTOT Index","TRURTOT Index",
    "THURTOT Index"
  )
)

# 保存为 CSV
write.csv(econ_data, "eco_database.csv", row.names = FALSE)

#####
library(readxl)
library(writexl)

# 文件路径
file_path <- "C:/Users/ASUS/Desktop/R_Union/BBG_tickersV1.xlsx"

# 读取指定 sheet（假设 tab 名是 "Sheet1"，可改成你需要的名字）
sheet_name <- "Commodity"

# 读取该 sheet
data <- read_excel(file_path, sheet = sheet_name)

# 取前三列
data_subset <- data[, 1:3]

# 导出为 CSV 文件
write.csv(data_subset, "C:/Users/ASUS/Desktop/R_Union/commodity_database.csv", row.names = FALSE)

