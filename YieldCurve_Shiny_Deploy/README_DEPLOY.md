# YieldCurve Trader 部署说明

这个目录是 `YieldCurve_Shiny` 的干净部署副本，专门用于 RStudio 发布到 shinyapps.io。

## 目录内容

- `app.R`：shinyapps.io 的线上入口。
- `run_app.R`：本地 RStudio 检查入口，方便发布前运行。
- `R/`：计算、数据读取和 package helper。
- `www/`：CSS 和前端样式。
- `data/`：线上随 app 一起上传的数据快照。
- `YieldCurve_Shiny_Deploy.Rproj`：RStudio 项目文件。

部署目录刻意不包含开发日志、截图、Word 对比文档、browser profile、`R_library/` 和 `tests/`。

## 1. 打开 RStudio 项目

1. 打开 RStudio。
2. 选择 `File -> Open Project...`。
3. 打开：

```text
C:\Users\PC\Desktop\R_git\YieldCurve_Shiny_Deploy\YieldCurve_Shiny_Deploy.Rproj
```

## 2. 发布前本地运行

在 RStudio Console 运行：

```r
source("run_app.R")
```

或者打开 `app.R`，点击右上角 `Run App`。

确认以下页面至少能跑通一次：

- Curve Explorer：点击 `Apply Curve`
- History & Changes：点击 `Run History Comparison`
- Forward Calculator：点击 `Calculate Forward`
- Carry & Roll：点击 `Calculate Carry & Roll`
- Carry & Roll / Curve Trade：点击 `Calculate Curve Trade`
- Diagnostics：检查最近一次 applied curve

## 3. 配置 shinyapps.io 账号

第一次部署前，在 RStudio Console 运行：

```r
install.packages("rsconnect")
library(rsconnect)
```

然后到 shinyapps.io 的 Tokens 页面复制账号命令，形如：

```r
rsconnect::setAccountInfo(name = "你的账号名", token = "你的token", secret = "你的secret")
```

这一步通常每台电脑只需要做一次。

## 4. RStudio 点击 Publish

1. 打开 `app.R`。
2. 点击 RStudio 右上角 `Publish`。
3. 选择 `shinyapps.io`。
4. App name 建议：

```text
yieldcurve-trader
```

5. App title 建议：

```text
YieldCurve Trader by Shuaihao
```

6. 文件列表确认包含：
   - `app.R`
   - `run_app.R`
   - `R/`
   - `www/`
   - `data/`
   - `README.md`
   - `CODE_GUIDE.md`

7. 文件列表不应该包含：
   - `R_library/`
   - `tests/`
   - `chrome-*profile/`
   - `.log`
   - `.png`
   - `.docx`

8. 点击 `Publish`。

## 5. 发布后检查

发布完成后打开线上 URL，检查：

- 顶部显示 `YieldCurve Trader by Shuaihao`
- Curve Explorer 能成功 Apply
- History、Forward、Carry、Curve Trade 都能计算
- Diagnostics 能显示 residual/table
- Plot、table、KPI card resize 不会撑爆页面

## 6. 后续更新数据或代码

如果源数据更新，需要重新复制快照到：

```text
data/WIDE_RATES
data/ZERORATE_CURVE
```

然后在 RStudio 里重新 `Publish` 到同一个 app。
