# YieldCurve Trader Dashboard

`YieldCurve_Shiny` 是一个本地 R Shiny 利率曲线交易台，由 Shuaihao 制作。它把本地 RDS 市场数据整理成可交互的曲线拟合、历史对比、远期利率、Carry & Roll、Curve Trade 和 Diagnostics 页面。

中文代码路线图见 [CODE_GUIDE.md](CODE_GUIDE.md)。那份文档按真实运行路径解释 `run_app.R`、`app.R`、`R/data_loader.R`、`R/curve_engine.R` 和各个 tab 的 `input$... -> applied_* -> output$...` 流程。

## Start

在 PowerShell 里运行：

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "C:\Users\PC\Desktop\R_git\YieldCurve_Shiny\run_app.R"
```

`run_app.R` 会定位 `YieldCurve_Shiny` 项目目录，加载 `R/packages.R`，把项目自己的 `R_library` 放进 `.libPaths()`，然后启动 `app.R`。默认使用本机 `127.0.0.1`，端口可通过 `YIELDCURVE_PORT` 环境变量覆盖。

## Current UI

顶部导航显示 `YieldCurve Trader by Shuaihao`，导航背景带低调的 Shuaihao 水印。主要页面包括：

- **Curve Explorer**：选择本地 zero-rate snapshot 或 historical proxy curve，点击 `Apply Curve` 后更新拟合曲线、KPI、residual plot、Fitted Parameters 和 desk note。
- **History & Changes**：选择多条 curve、base date 和 compare dates，点击 `Run History Comparison` 后生成 absolute curves、change plot 和 quote details table。
- **Forward Calculator**：独立选择 curve/source/date/fit method，输入 start/end tenor 和 compounding，点击 `Calculate Forward` 后显示 forward rate、端点图和 sensitivity。
- **Carry & Roll**：左侧 `Single Trade` / `Curve Trade` workspace 切换；Single 计算 carry/roll/total/P&L，Curve Trade 计算 Steepener、Flattener、Butterfly、Short Fly 的 legs 和 portfolio summary。
- **Diagnostics**：跟随最近一次成功 `Apply Curve` 的曲线，检查 market points、fitted points、residuals 和 model path。

## UI Behavior

- 这个 app 使用 **applied-result 模式**：输入变化只把对应页面标记为 pending；点击 `Apply` / `Calculate` / `Run` 后，成功结果才写入 `applied_curve()`、`applied_history()`、`applied_forward()`、`applied_carry()` 或 `applied_trade()`。
- 如果计算失败，旧的成功结果保留，状态区显示 error，避免调参时图表和表格被半成品覆盖。
- Plotly、table、KPI、section card 都支持手动 resize；Plotly 使用 `ResizeObserver + Plotly.Plots.resize()` 跟随容器变化。
- Carry heatmap、Curve Trade、History table 等容易变高的区域有 `max-height` 和内部滚动，避免多次计算或 tab 切换后撑开整个页面。
- 数字显示按金融语义统一：利率百分比显示两位小数，bp 显示一位小数，P&L 四舍五入到个位；正负变化量用不同颜色显示。

## Data Policy

- 本地正式 zero-rate analytics 来自 `R_Union/ZERORATE_CURVE`。
- 历史 proxy quotes 来自 `R_Union/WIDE_RATES`，用于历史对比、proxy curve 和 fallback date 逻辑。
- 市场输入如 `4.25` 表示 `4.25%`，内部计算统一转换为 decimal rate `0.0425`。
- 当前版本不伪造 Bloomberg live、Save Workspace、实时市场 timestamp 或外部刷新；页面只显示本地 RDS 可支持的口径。
- 历史日期如果没有足够有效 tenor 点，会选择前后距离最近的有效 market quote date；距离相同时优先较早日期。

## Analytics Notes

- `calculate_forward()` 在拟合曲线上读取 start/end zero rate，再按 compounding 计算 implied forward。
- `calculate_carry_roll()` 复用 forward 计算，比较当前 forward、hold 后 rolled forward 和短端 funding rate，得到 carry/roll/total bp。
- `calculate_curve_trade()` 对每条 leg 调用 `calculate_carry_roll()`，再用该 leg 的 DV01 转成 P&L 并汇总 portfolio。
- Curve Trade 的 `trade_start` 是所有 legs 共用的 start tenor，默认 `0Y`；它必须小于所有 leg tenor，否则计算失败并保留旧结果。
- `build_carry_matrix()` 固定用 spot start `0Y`，用于 tenor by hold 的 heatmap / matrix 视图。

## Tests

完整回归测试：

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "C:\Users\PC\Desktop\R_git\YieldCurve_Shiny\tests\run_tests.R"
```

真实 RDS 计算矩阵验证：

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "C:\Users\PC\Desktop\R_git\YieldCurve_Shiny\tests\validation_matrix.R"
```

HTTP smoke test 可启动本地 Shiny app 并确认服务可访问：

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "C:\Users\PC\Desktop\R_git\YieldCurve_Shiny\tests\smoke_app.R" 7411
```

## Main Files

- `run_app.R`：本地启动入口。
- `app.R`：UI、server、reactive state、Plotly/DT 输出和 applied-result 控制。
- `R/packages.R`：项目 package 路径和依赖加载。
- `R/data_loader.R`：读取并规范化本地 RDS 数据，构造历史对比数据。
- `R/curve_engine.R`：拟合曲线、forward、carry/roll、DV01 P&L、curve trade 计算。
- `www/styles.css`：Shuaihao branding、dashboard layout、resizable cards、table/KPI/plot 样式。
- `tests/run_tests.R`：计算逻辑和 Shiny server 行为回归测试。
