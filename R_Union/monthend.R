library(lubridate)
library(dplyr)

# 确保月份序列是 Date
months_seq <- seq(from = floor_date(as.Date("2024-01-01"), "month"),
                  to   = floor_date(as.Date("2024-09-01"), "month"),
                  by   = "month")

result_list <- list()

for (sec in t) {
  for (m in months_seq) {
    # 直接用 Date，加 lubridate::days() 保持 Date 类型
    start <- as.Date(m + days(20) )                      # 每月21日左右
    end   <- as.Date(ceiling_date(as.Date(m), "month") - days(1) )# 月末
    # print(start)
  # print(end)
    tmp <- tryCatch({
      bdh(sec, "PX_LAST", start, end)
    }, error = function(e) NULL)

    if (!is.null(tmp)) {
      tmp <- tmp %>% arrange(date) %>% slice_tail(n = 2)  # 取最后两天
      result_list[[paste(sec, m, sep = "_")]] <- tmp
    }
  }
}

final <- bind_rows(result_list)

library(lubridate)

start_date <- as.Date("2024-01-01")
end_date   <- as.Date("2024-09-15")

months_seq <- seq(
  from = floor_date(start_date, "month"),
  to   = floor_date(end_date, "month"),
  by   = "month"
)

for (m in months_seq) {
  start <- as.Date(m + days(20))                 # 每月21日左右
  end   <- as.Date(ceiling_date(m, "month") - days(1))  # 月末
  
  # print(class(m))      # Date
  # print(class(start))  # Date
  # print(class(end))    # Date
  print(m)
  print(start)
  print(end)
}


