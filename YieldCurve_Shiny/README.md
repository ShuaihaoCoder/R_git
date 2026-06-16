# YieldCurve Trader Dashboard

独立的利率交易员 R Shiny dashboard。项目只读使用
`R_Union/YieldCurve.R`、`WIDE_RATES` 和 `ZERORATE_CURVE`，不会覆盖原文件。

中文代码运行线路图见 [`CODE_GUIDE.md`](CODE_GUIDE.md)。其中包含每个主要页面的具体
sidebar 输入、真实运行结果和独立 Mermaid 路线图。

## Start

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "C:\Users\PC\Desktop\R_git\YieldCurve_Shiny\run_app.R"
```

第一次启动会把 UI packages 安装到项目自己的 `R_library`，后续启动会复用。

## Pages

- 每个主要页面都有独立 `Apply/Calculate` 按钮；修改输入后保留旧结果，点击按钮才重新计算。
- 页面内嵌进度条显示计算阶段，不使用让整页变灰的遮罩进度条。
- **Curve Explorer**：观察正式 zero curve 或历史 Proxy，比较 Nelson-Siegel 与 Spline、RMSE 和参数。
- **History & Changes**：Curve 标签可用 `X` 删除；Base Date 独立；最多运行 30 个 Curve×Date 组合。
- **Forward Calculator**：独立选择曲线和 Historical Date，并显示 Requested/Effective Date。
- **Carry & Roll / Single Trade**：正值绿色、负值红色；Hold 顺序固定为 `1M, 3M, 6M, 1Y`。
- **Carry & Roll / Curve Trade**：显示每条 Leg 的 Carry/Roll 分解；默认 DV01-neutral，也可手动调整腿部 DV01。
- **Diagnostics**：检查市场点、拟合点、残差和模型口径。

## Analytics Policy

- 市场输入例如 `4.25` 表示 `4.25%`，内部计算统一转换成小数 `0.0425`。
- `ZERORATE_CURVE` 用于正式 zero-rate forward 和 carry/roll。
- `WIDE_RATES` 是历史市场/par quotes，因此所有相关结果明确标记为 **Proxy**。
- 历史曲线在请求日期数据不足时，自动选择前后距离最近的有效日期；距离相同时优先较早日期。
- 金额 P&L 是 `direction-adjusted total bp * user-provided DV01` 的估算。
- 当前版本不实现 Bloomberg 刷新、严格多曲线 bootstrap 或票息债现金流重估。
- 所有展示数字最多两位小数；输入框仍允许更高精度。

## Tests

完整测试和 Shiny server 测试：

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "C:\Users\PC\Desktop\R_git\YieldCurve_Shiny\tests\run_tests.R"
```

无需启动 UI packages 的真实市场计算矩阵：

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "C:\Users\PC\Desktop\R_git\YieldCurve_Shiny\tests\validation_matrix.R"
```

`tests/smoke_app.R` 用于启动本地 HTTP smoke test。
