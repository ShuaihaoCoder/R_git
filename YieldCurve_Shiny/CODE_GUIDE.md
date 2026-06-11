# YieldCurve Trader Dashboard 代码线路图

这份线路图按“代码实际如何运行”来讲，而不是只列出函数定义。

- `input$...`：用户在浏览器中选择或输入的值。
- `reactive()`：输入变化后，会通知依赖它的计算重新运行。
- `output$...`：server 计算完成后返回给浏览器的图、表或文字。
- **正式零息分析**来自 `ZERORATE_CURVE`；**历史 Proxy** 来自 `WIDE_RATES`。

---

## 1. 全项目运行路线

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    User["用户运行 run_app.R"]
    Run["run_app.R<br/>定位 YieldCurve_Shiny"]
    Packages["R/packages.R<br/>准备 Shiny / Plotly / DT"]
    App["app.R<br/>建立五个页面与 server"]
    Loader["R/data_loader.R<br/>只读加载并整理市场数据"]
    Engine["R/curve_engine.R<br/>拟合、forward、carry/roll、P&L"]
    Rates["R_Union/WIDE_RATES<br/>历史报价 Proxy"]
    Zero["R_Union/ZERORATE_CURVE<br/>正式零息快照"]
    Browser["浏览器<br/>图表、表格、计算器"]

    User --> Run --> Packages --> App
    App --> Loader
    Loader -- "readRDS() 后统一为 data.frame" --> Rates
    Loader -- "readRDS()" --> Zero
    Loader -- "tenor + decimal rate" --> App
    App -- "期限、利率、方法、方向" --> Engine
    Engine -- "拟合曲线和计算结果" --> App
    App -- "output$..." --> Browser
    Browser -- "input$..." --> App
```

`R_Union/YieldCurve.R` 是原始参考脚本，不在当前 Shiny 运行路线中，也不会被网页修改。

---

## 2. 启动时序

```mermaid
%%{init: {'sequence': {'actorMargin': 60, 'messageMargin': 45}, 'themeVariables': {'fontSize': '16px'}}}%%
sequenceDiagram
    participant U as 用户
    participant R as run_app.R
    participant P as R/packages.R
    participant A as app.R
    participant D as R/data_loader.R
    participant B as 浏览器

    U->>R: 运行 Rscript.exe run_app.R
    R->>R: find_launcher_project_dir()
    R->>P: install_and_load_packages(project_dir)
    R->>A: shiny::runApp(project_dir)
    A->>D: load_market_data(project_dir)
    D-->>A: wide_rates + zero_curve + loaded_at
    A-->>B: 打开 Curve Explorer 等五个页面
```

启动后，`market()` 保存本次读取的数据。点击 **Refresh local RDS** 后，
`market(): 旧数据 -> 新读取数据`，所有依赖它的曲线列表和结果跟着刷新。

---

## 3. 数据读取与日期回退

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    Load["load_market_data()"]
    Read["readRDS(WIDE_RATES)"]
    Frame["as.data.frame()<br/>固定动态选列行为"]
    Names["historical_curve_names()<br/>生成历史曲线下拉框"]
    Select["extract_historical_curve(curve_name, requested_date)"]
    Columns["matched_columns<br/>例如 USD SOFR OIS 的 24 个期限列"]
    Resolve["resolve_historical_curve_date()<br/>向前找最近有效日期"]
    Points["clean_curve_points()<br/>tenor + decimal rate"]
    Attr["附加 requested_date / effective_date"]

    Load --> Read --> Frame
    Frame --> Names
    Frame --> Select --> Columns --> Resolve --> Points --> Attr
```

具体例子：

```text
用户请求：EUR ESTR OIS，2025-10-22
该日有效期限点不足
resolve_historical_curve_date() 向前寻找
返回：最近一个至少有 3 个有效点的 effective_date
网页显示：Requested: 2025-10-22 | Effective: 实际日期
```

这一步也修复了原来的错误：`data.table` 不再把 `matched_columns` 错认为真实列名。

---

## 4. Curve Explorer 与共享拟合路线

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    Inputs["input$source_mode<br/>input$curve_name<br/>input$curve_date"]
    Current["current_points()"]
    Zero["extract_zero_curve()<br/>正式零息"]
    Historical["extract_historical_curve()<br/>历史 Proxy + 日期回退"]
    Methods["input$fit_methods"]
    Fits["current_fits()<br/>fit_curve()"]
    NS["Nelson-Siegel<br/>参数 + RMSE"]
    Spline["Spline<br/>平滑拟合 + RMSE"]
    Analytics["analytics_curve()<br/>优先 NS，否则当前拟合"]
    Plot["output$curve_plot<br/>output$fit_summary<br/>output$ns_parameters"]
    Shared["Forward / Carry / Diagnostics"]

    Inputs --> Current
    Current --> Zero
    Current --> Historical
    Current --> Fits
    Methods --> Fits
    Fits --> NS
    Fits --> Spline
    Fits --> Plot
    Fits --> Analytics --> Shared
```

关键点：Forward、Carry 和 Diagnostics 不各自重新拟合，而是共用
`analytics_curve()`，因此同一次选择在不同页面使用同一条曲线。

---

## 5. 五个页面如何响应输入

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    CurveInput["切换 source / curve / date / fit"]
    Points["current_points()"]
    Fits["current_fits()"]
    Explorer["Curve Explorer<br/>拟合图、RMSE、NS 参数"]
    Diagnostics["Diagnostics<br/>残差与原始点"]

    HistoryInput["切换 history curve / 两个日期"]
    History["history_data()"]
    HistoryPage["History & Changes<br/>两条曲线 + change bp"]

    ForwardInput["输入 start / end / compounding"]
    Forward["forward_result()"]
    ForwardPage["Forward Calculator<br/>forward %"]

    CarryInput["输入 start / end / hold / direction / DV01"]
    Carry["carry_result() + carry_matrix_data()"]
    CarryPage["Carry & Roll<br/>bp、P&L、热力图"]

    CurveInput --> Points --> Fits --> Explorer
    Fits --> Diagnostics
    HistoryInput --> History --> HistoryPage
    Fits --> Forward
    ForwardInput --> Forward --> ForwardPage
    Fits --> Carry
    CarryInput --> Carry --> CarryPage
```

### 具体操作例子

```text
Curve Explorer:
source_mode = zero
curve_name = USD UNITED STATES OIS
fit_methods = Nelson-Siegel + Spline
-> current_points() 返回 25 个点
-> current_fits() 返回两种拟合
-> 曲线图、RMSE 与参数表刷新

Forward Calculator:
forward_start = 1
forward_end = 5
compounding = annual
-> calculate_forward(analytics_curve(), 1, 5, "annual")
-> Forward Rate 卡片与结果表刷新

Carry & Roll:
carry_start = 0
carry_end = 5
carry_hold = 0.25
direction = Receive Fixed
dv01 = 10000
-> calculate_carry_roll() 返回 carry / roll / total bp
-> calculate_dv01_pnl(total_bp, 10000) 返回估算金额
-> 卡片、矩阵与热力图刷新
```

---

## 6. 核心变量传递表

| 变量 | 从哪里产生 | 内容示例 | 后来影响什么 |
|---|---|---|---|
| `market()` | `load_market_data()` | `wide_rates`, `zero_curve`, `loaded_at` | 所有曲线列表与页面结果 |
| `current_points()` | 当前 source、curve、date | `tenor + decimal rate` | 拟合、曲线图、Diagnostics |
| `current_fits()` | `fit_curve()` | NS/Spline 结果列表 | RMSE、参数、`analytics_curve()` |
| `analytics_curve()` | 优先选中的 NS 拟合 | `yield_curve_fit` | Forward、Carry、Diagnostics |
| `history_data()` | 两个历史曲线点按 tenor 合并 | base、compare、change bp | History 图和表 |
| `effective_date` | 日期回退函数 | 最近有效历史日期 | 页面日期提示与历史口径 |
| `carry_matrix_data()` | 多个 tenor × hold 组合 | carry、roll、total、P&L | Carry 表格和热力图 |

---

## 7. Sidebar 具体操作案例

> 注意：案例 A、B、F 仍对应当前 Curve Explorer / Diagnostics。案例 C、D、E 保留为早期版本
> 的演进记录，其中旧 input 名称不再由当前 app 使用。当前 History、Forward、Carry 与 Curve Trade
> 的可执行路线请直接阅读第 8 节案例 G-K。

这一章从用户真正点击 sidebar 开始讲。每个案例都写明：

1. sidebar 选择了什么；
2. 哪个 `input$...` 改变；
3. 哪些 reactive 和函数重新运行；
4. 返回什么真实结果；
5. 页面哪些区域跟着刷新。

### 案例 A：Curve Explorer 选择正式 USD OIS 曲线

#### Sidebar 输入

```text
Analytics source = Zero-rate snapshot (official analytics)
Curve             = USD UNITED STATES OIS
Curve fits        = Nelson-Siegel + Spline
```

这些控件分别变成：

```text
input$source_mode = "zero"
input$curve_name  = "USD UNITED STATES OIS"
input$fit_methods = c("nelson_siegel", "spline")
```

#### 实际运行路线

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    Sidebar["Curve Explorer sidebar<br/>source=zero<br/>curve=USD UNITED STATES OIS<br/>fits=NS + Spline"]
    Points["current_points()"]
    Extract["extract_zero_curve()<br/>筛选 DES = USD UNITED STATES OIS"]
    Clean["clean_curve_points()<br/>25 个 tenor + decimal rate"]
    Fits["current_fits()"]
    NS["fit_curve(method=nelson_siegel)<br/>RMSE = 3.54 bp"]
    Spline["fit_curve(method=spline)<br/>生成平滑曲线"]
    Analytics["analytics_curve()<br/>选择 NS 作为计算曲线"]
    Plot["output$curve_plot<br/>红色市场点 + 两条拟合线"]
    Tables["output$fit_summary<br/>output$ns_parameters"]
    Downstream["Forward / Carry / Diagnostics<br/>共用 USD OIS NS 曲线"]

    Sidebar --> Points --> Extract --> Clean --> Fits
    Fits --> NS --> Analytics --> Downstream
    Fits --> Spline
    NS --> Plot
    Spline --> Plot
    Fits --> Tables
```

#### 用户看到的结果

```text
市场点数量：25
Nelson-Siegel RMSE：3.54 bp
页面提示：Zero-rate analytics
Effective date：Snapshot source: no historical date
```

Curve Explorer 的曲线图、拟合误差表、NS 参数表刷新；Forward、Carry 和
Diagnostics 也立刻改为使用这条 USD OIS 曲线。

---

### 案例 B：Curve Explorer 选择历史 EUR ESTR，并发生日期回退

#### Sidebar 输入

```text
Analytics source = Historical market quotes (proxy)
Curve             = EUR ESTR OIS
Date              = 2025-10-22
Curve fits        = Nelson-Siegel + Spline
```

#### 实际运行路线

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    Sidebar["Curve Explorer sidebar<br/>source=historical<br/>curve=EUR ESTR OIS<br/>date=2025-10-22"]
    Current["current_points()"]
    Extract["extract_historical_curve()<br/>寻找 EUR ESTR OIS 的期限列"]
    Columns["matched_columns<br/>24 个期限列"]
    Requested["检查 2025-10-22<br/>有效点不足"]
    Resolve["resolve_historical_curve_date()<br/>向前逐日寻找"]
    Effective["effective_date = 2025-10-21<br/>24 个有效点"]
    Fits["current_fits()<br/>NS RMSE = 4.06 bp"]
    Banner["页面显示 Proxy analytics"]
    DateText["页面显示<br/>Requested: 2025-10-22<br/>Effective: 2025-10-21"]
    Shared["Forward / Carry / Diagnostics<br/>使用回退后的 Proxy 曲线"]

    Sidebar --> Current --> Extract --> Columns --> Requested --> Resolve --> Effective --> Fits
    Fits --> Banner
    Fits --> DateText
    Fits --> Shared
```

#### 用户看到的结果

```text
Requested date：2025-10-22
Effective date：2025-10-21
市场点数量：24
Nelson-Siegel RMSE：4.06 bp
1Y -> 5Y Forward：2.114%
0Y -> 5Y、持有 3M、Receive Fixed 的 Carry + Roll：5.90 bp
```

这里最重要的是：日期缺数据不会再使多个页面一起报错，而是明确回退并显示实际日期。

---

### 案例 C（历史版本）：History & Changes 比较 USD SOFR OIS 两个日期

#### Sidebar 输入

```text
Historical curve = USD SOFR OIS
Base date        = 2025-09-23
Compare date     = 2025-10-22
```

对应值：

```text
input$history_curve  = "USD SOFR OIS"
input$history_date_1 = "2025-09-23"
input$history_date_2 = "2025-10-22"
```

#### 实际运行路线

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    Sidebar["History sidebar<br/>USD SOFR OIS<br/>2025-09-23 vs 2025-10-22"]
    History["history_data()"]
    Base["extract_historical_curve()<br/>Base: 2025-09-23<br/>24 个点"]
    Compare["extract_historical_curve()<br/>Compare: 2025-10-22<br/>24 个点"]
    Merge["merge(..., by=tenor)<br/>把相同期限放到同一行"]
    Change["change_bp =<br/>(compare rate - base rate) × 10000"]
    Plot["output$history_plot<br/>两日曲线"]
    Table["output$history_changes<br/>逐期限 base / compare / change bp"]
    Dates["output$history_effective_dates<br/>显示两个实际日期"]

    Sidebar --> History
    History --> Base --> Merge
    History --> Compare --> Merge
    Merge --> Change
    Change --> Plot
    Change --> Table
    Change --> Dates
```

#### 用户看到什么

- 图中同时出现 `2025-09-23` 和 `2025-10-22` 两条 USD SOFR OIS 曲线。
- 表格每行代表一个共同期限，并显示该期限利率变化的 bp。
- 两个日期均有有效数据，因此 Effective Date 与用户选择一致。

History 页面使用自己的 `history_data()`，不会改变 Curve Explorer 当前用于
Forward 和 Carry 的 `analytics_curve()`。

---

### 案例 D（历史版本）：Forward Calculator 计算 USD OIS 的 1Y → 5Y Forward

这个页面没有曲线选择 sidebar。它使用 Curve Explorer 当前已经选好的
`analytics_curve()`。因此先在 Curve Explorer 选择：

```text
source = Zero-rate snapshot
curve  = USD UNITED STATES OIS
```

然后在 Forward Calculator 输入：

```text
Forward start = 1
Forward end   = 5
Compounding   = Annual
```

#### 实际运行路线

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    Curve["Curve Explorer 当前曲线<br/>USD UNITED STATES OIS"]
    Analytics["analytics_curve()<br/>USD OIS Nelson-Siegel"]
    Sidebar["Forward sidebar<br/>start=1<br/>end=5<br/>compounding=annual"]
    Inputs["input$forward_start = 1<br/>input$forward_end = 5<br/>input$forward_compounding = annual"]
    Calc["calculate_forward(curve, 1, 5, annual)"]
    Rates["读取曲线上的 1Y 与 5Y zero rate"]
    DF["discount_factor()<br/>计算 1Y 和 5Y 折现因子"]
    Result["forward_result()<br/>forward = 3.336%"]
    Card["output$forward_value<br/>Forward Rate 卡片"]
    Table["output$forward_result<br/>start/end/rate/source/proxy"]

    Curve --> Analytics --> Calc
    Sidebar --> Inputs --> Calc
    Calc --> Rates --> DF --> Result
    Result --> Card
    Result --> Table
```

#### 用户看到的结果

```text
Forward Rate：3.336%
Start：1 year
End：5 years
Compounding：annual
Source：ZERORATE_CURVE | USD UNITED STATES OIS
Proxy：FALSE
```

若回到 Curve Explorer 把曲线切换成 `AUD AUSTRALIA (vs. 6M Bank Bills)`，
Forward sidebar 不需要重新输入，`analytics_curve()` 改变后结果自动变为约 `4.549%`。

---

### 案例 E（历史版本）：Carry & Roll 计算 Receive Fixed 与 Pay Fixed

先在 Curve Explorer 选择：

```text
source = Zero-rate snapshot
curve  = USD UNITED STATES OIS
```

Carry & Roll sidebar 输入：

```text
Trade start = 0
Trade end   = 5
Hold period = 3M
Direction   = Receive Fixed
DV01 per bp = 10000
```

#### 实际运行路线

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    Curve["analytics_curve()<br/>USD OIS NS 曲线"]
    Sidebar["Carry sidebar<br/>start=0 / end=5 / hold=0.25<br/>Receive Fixed / DV01=10000"]
    Single["carry_result()<br/>calculate_carry_roll()"]
    Current["计算当前 0Y -> 5Y forward"]
    Rolled["计算持有 3M 后<br/>0Y -> 4.75Y rolled forward"]
    Funding["计算 3M 短端资金成本"]
    BP["carry / roll / total bp"]
    PNL["calculate_dv01_pnl()<br/>total bp × 10000"]
    Matrix["carry_matrix_data()<br/>9 个 tenor × 4 个 hold"]
    Cards["Carry / Roll / Total P&L 卡片"]
    Outputs["矩阵表格 + 热力图"]

    Curve --> Single
    Sidebar --> Single
    Single --> Current --> BP
    Single --> Rolled --> BP
    Single --> Funding --> BP --> PNL --> Cards
    Curve --> Matrix
    Sidebar --> Matrix --> Outputs
```

#### 用户看到的结果

```text
Receive Fixed:
Total Carry + Roll = -6.35 bp
Estimated P&L      = -63,538（使用 DV01 = 10,000）
```

把 sidebar 的 `Direction` 改成 `Pay Fixed` 后：

```text
input$carry_direction: "Receive Fixed" -> "Pay Fixed"
direction_sign(): +1 -> -1
Carry、Roll、Total bp 与 P&L 的符号全部反转
热力图和矩阵也一起刷新
```

---

### 案例 F：Diagnostics 如何跟随 Curve Explorer，以及 Refresh 如何传播

Diagnostics 没有自己的 sidebar。它始终检查 Curve Explorer 当前选择产生的
`current_points()` 和 `analytics_curve()`。

例如 Curve Explorer 选择：

```text
source = Historical market quotes (proxy)
curve  = AUD COR OIS
date   = 2025-10-22
fit    = Nelson-Siegel
```

#### 实际运行路线

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    Sidebar["Curve Explorer sidebar<br/>AUD COR OIS / 2025-10-22 / NS"]
    Points["current_points()<br/>22 个历史报价点"]
    Fit["analytics_curve()<br/>NS RMSE = 3.35 bp"]
    Diagnostics["Diagnostics 页面"]
    Residuals["output$diagnostics_table<br/>observed / fitted / residual bp"]
    Inputs["output$input_points<br/>tenor / decimal rate / rate percent"]
    Refresh["点击 Refresh local RDS"]
    Market["market(): 旧数据 -> 新读取数据"]
    Rerun["曲线列表、current_points()、拟合和页面结果重新运行"]

    Sidebar --> Points --> Fit --> Diagnostics
    Diagnostics --> Residuals
    Diagnostics --> Inputs
    Refresh --> Market --> Rerun --> Points
```

#### 用户看到的结果

```text
AUD COR OIS Effective Date：2025-10-22
市场点数量：22
Nelson-Siegel RMSE：3.35 bp
Diagnostics 表中每一行显示：
tenor、observed_percent、fitted_percent、residual_bp
```

点击 Refresh 后，网页重新只读加载 `WIDE_RATES` 和 `ZERORATE_CURVE`。
如果源文件内容没变，数值保持一致；如果源文件已更新，所有依赖当前曲线的页面一起刷新。

---

## 8. 当前版本新增交互案例

本节描述当前版本的多选 History、独立计算器曲线和 Curve Trade。下面数值来自
`tests/validation_matrix.R` 对真实本地 RDS 的实际运行结果。

### 案例 G：History 同时比较三条曲线和三个日期

```text
input$history_curves = USD SOFR OIS, AUD COR OIS, EUR ESTR OIS
input$history_dates = 2025-09-23, 2025-10-21, 2025-10-22
input$history_base_date = 2025-09-23
```

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    I["3 curves x 3 requested dates"]
    H["history_data()"]
    B["build_history_comparison()<br/>每条 curve 各自对比 2025-09-23"]
    F["EUR ESTR 2025-10-22<br/>fallback -> effective 2025-10-21"]
    A["output$history_absolute_plot<br/>9 组绝对曲线"]
    C["output$history_change_plot<br/>各期限 change bp"]
    T["output$history_comparison_table<br/>210 rows"]
    I --> H --> B
    B --> F
    B --> A
    B --> C
    B --> T
```

真实汇总结果：`USD SOFR OIS` 在 `2025-10-22` 相对 base date 平均变化
`-10.736 bp`；`AUD COR OIS` 为 `-5.288 bp`；EUR ESTR 请求最新日时自动回退。

### 案例 H：Forward 独立选择 EUR 曲线

```text
input$forward_source_mode = zero
input$forward_curve_name = EUR EUROZONE (vs. 6M EURIBOR)
input$forward_fit_method = spline
input$forward_start = 1
input$forward_end = 5
```

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    I["Forward sidebar<br/>EUR zero / Spline / 1Y -> 5Y"]
    P["forward_curve()<br/>prepare_curve_fit()"]
    F["calculate_forward()<br/>annual compounding"]
    R["forward = 2.5644%"]
    O["Forward card + result table<br/>selected curve/endpoints plot"]
    X["Curve Explorer 与 Carry 当前选择<br/>保持不变"]
    I --> P --> F --> R --> O
    I -. "不触发" .-> X
```

Forward 是当前曲线隐含的远期利率，不是利率预测。端点图用于确认计算使用的是哪条曲线和
哪两个期限。

### 案例 I：Carry 独立选择 AUD 曲线并读取四类图

```text
input$carry_curve_name = AUD AUSTRALIA (vs. 6M Bank Bills)
input$carry_start = 0
input$carry_end = 5
input$carry_hold = 0.25
input$carry_direction = Receive Fixed
input$dv01 = 10000
```

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    I["AUD zero / Receive Fixed<br/>0Y -> 5Y / hold 3M"]
    S["carry_result()<br/>carry=11.26 bp<br/>roll=2.25 bp<br/>total=13.51 bp"]
    M["carry_matrix_data()<br/>9 tenors x 4 holds"]
    D["单笔 Carry/Roll/Total 柱状图"]
    Spot["Spot curve<br/>解释 roll 来自曲线形状"]
    Stack["Stacked bars<br/>蓝色 Carry + 黄色 Roll<br/>红点 Total"]
    Heat["Heatmap + table<br/>快速找正负机会"]
    I --> S --> D
    I --> M
    M --> Spot
    M --> Stack
    M --> Heat
```

Carry 是持有期间收入减短端资金成本；Roll 是曲线保持不变时交易向短期限移动造成的变化。
图中 Total 是两者相加，金额 P&L 再乘用户输入 DV01。

### 案例 J：点击计算 2s10s Steepener

```text
input$trade_structure = steepener
short = 2Y, long = 10Y, hold = 3M
risk budget = 10000
默认 legs = Receive Fixed 2Y DV01 10000 + Pay Fixed 10Y DV01 10000
点击 Calculate Curve Trade
```

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    I["选择 Steepener + USD OIS"]
    N["curve_trade_legs()<br/>生成 DV01-neutral 两条腿"]
    Click["input$calculate_curve_trade 增加"]
    E["trade_result() eventReactive"]
    L["每条腿 calculate_carry_roll()<br/>再乘各自 DV01"]
    R["Portfolio total P&L = -143235<br/>Equivalent = -14.323 bp"]
    O["腿部 P&L 图 + Portfolio Carry/Roll 图 + 明细表"]
    I --> N --> Click --> E --> L --> R --> O
```

Steepener 的方向是收短端固定、付长端固定。Flattener 使用相反方向，因此相同输入下真实结果
为 `+143235`，符号正好相反。

### 案例 K：点击计算 2s5s10s Fly

```text
input$trade_structure = long_belly_fly
2Y DV01 = 5000, 5Y DV01 = 10000, 10Y DV01 = 5000
legs = Pay 2Y + Receive 5Y + Pay 10Y
```

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    I["Long-belly Fly<br/>2s5s10s"]
    N["默认 DV01 neutral<br/>belly DV01 = 两翼 DV01 总和"]
    C["calculate_curve_trade()<br/>三腿 carry/roll/P&L 相加"]
    R["Total P&L = -14978<br/>Equivalent = -1.498 bp"]
    Manual["手动改 Steepener DV01<br/>8000 / 12000"]
    MR["重新点击后<br/>Total P&L = -123811"]
    I --> N --> C --> R
    Manual --> C --> MR
```

Long-belly Fly 表示收 belly、付两翼；Short-belly Fly 方向相反，真实结果为 `+14978`。
手动 DV01 输入只有在再次点击计算按钮后才进入结果，避免调参数过程中页面不断跳动。

---

## 9. 测试路线

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart LR
    Tests["tests/run_tests.R"]
    Units["单位 / flat curve / 方向 / DV01"]
    Real["真实 RDS / data.table / 日期回退"]
    Server["shiny::testServer()<br/>驱动五页共享 reactive"]
    Smoke["tests/smoke_app.R<br/>启动真实 HTTP server"]
    Browser["浏览器逐页操作<br/>检查输出与错误"]

    Tests --> Units --> Real --> Server --> Smoke --> Browser
```

自动测试必须先加载真实网页 packages，确保测试环境与用户实际运行环境一致。
