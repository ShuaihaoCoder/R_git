# DataScience Shiny 项目结构说明

## 1. 项目目标

`DataScience_Shiny` 是一个独立的 R Shiny 项目，用来整理和展示
`DataScience.R` 中使用过的数据科学与统计方法。

这个项目主要解决三个问题：

1. 把原来集中在一个大型 R 文件中的方法整理成可查询的百科网页。
2. 通过方法目录、Method Navigator 网络图和 Source Map，快速找到适合的数据科学方法。
3. 保留原来的金融与宏观数据案例，并展示案例背景、变量含义、代码、图表和模型结果。

项目不会修改 `R_Union/DataScience.R` 或其他旧项目文件。

---

## 2. 整体目录结构

```text
DataScience_Shiny/
|
|-- app.R
|-- run_app.R
|-- DataScience_optimized.R
|-- DataScience_original_reference.R
|-- README.md
|-- PROJECT_STRUCTURE.md
|-- .gitignore
|
|-- R/
|   |-- packages.R
|   |-- data_loader.R
|   |-- catalog.R
|   `-- examples.R
|
|-- data/
|   |-- WIDE_RATES
|   |-- WIDE_FX
|   |-- WIDE_VOL
|   |-- WIDE_ECO
|   |-- WIDE_CFTC
|   |-- WIDE_MM
|   |-- WIDE_EQ
|   |-- WIDE_COMM
|   |-- WIDE_CREDIT
|   `-- WIDE_ALLX
|
|-- www/
|   `-- styles.css
|
`-- R_library/
```

---

## 3. 顶层文件说明

### `app.R`

这是 Shiny 网页的主要入口，也是 UI 和 Server 的连接中心。

它负责：

- 加载 `R/` 文件夹中的公共函数。
- 从 `data/` 读取数据库。
- 创建左侧两级方法目录。
- 创建 Method Navigator 方法网络图。
- 创建 Method Detail、Catalog 和 Source Map 页面。
- 根据用户选择的方法，调用对应案例函数。
- 展示英文方法说明、案例背景、变量含义、图表、结果表和代码。
- 处理点击网络图节点后跳转到对应方法页面的逻辑。

`app.R` 本身主要负责网页组织，不应该放大量模型计算代码。模型计算应放在
`R/examples.R` 中。

### `run_app.R`

这是推荐使用的网页启动文件。

它负责：

1. 找到当前 `DataScience_Shiny` 项目目录。
2. 把项目本地的 `R_library/` 加入 `.libPaths()`。
3. 调用 `shiny::runApp()` 启动 `app.R`。
4. 固定使用 `http://127.0.0.1:7411`。

它会优先使用与当前 R 版本对应的项目包目录，例如 `R_library/R-4.5`。
因此 VSCode 和 RStudio 使用不同 R 版本时，不会错误共用不兼容的二进制包。
如果缺少必需 package，`run_app.R` 会通过 `packages.R` 自动安装，然后继续启动网页。

推荐启动命令：

```powershell
& "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" "C:/Users/PC/Desktop/R_git/DataScience_Shiny/run_app.R"
```

然后在浏览器打开：

```text
http://127.0.0.1:7411
```

### `DataScience_optimized.R`

这是整理后的脚本式分析入口。

它与 Shiny 网页的区别是：

- `app.R` 用于交互式网页展示。
- `DataScience_optimized.R` 用于直接在 R 或 RStudio 中运行和研究代码。

它负责：

- 使用相对路径读取新项目中的数据。
- 使用清晰的数据对象名称，例如 `rates_data`、`fx_data` 和 `equity_data`。
- 调用公共数据准备函数。
- 演示如何直接运行案例函数。
- 保留中文注释，帮助理解代码流程。

这个文件不会包含原脚本的全部 2000 多行代码。它是新的、较清晰的运行入口。

### `DataScience_original_reference.R`

这是从原始 `R_Union/DataScience.R` 复制过来的参考副本。

它的用途是：

- 保留原始分析逻辑和历史代码。
- 在增加新的 Shiny 案例时，查找原来的模型与绘图代码。
- 对照 Source Map 中标记的原始行号和代码段。

这个文件属于参考资料，不是 Shiny 网页直接执行的主要脚本。

一般情况下不应直接重构或大量修改它。新的整理代码应写入 `R/` 文件夹。

### `README.md`

这是项目的快速使用说明。

它主要记录：

- 项目用途。
- 启动方式。
- 主要目录简介。
- 第一版网页已经提供的功能。

`README.md` 适合快速了解和启动项目；本文件 `PROJECT_STRUCTURE.md` 则负责详细解释架构。

### `PROJECT_STRUCTURE.md`

这是当前文件。

它负责详细说明：

- 每个文件和目录的作用。
- 文件之间的调用关系。
- 数据如何进入网页。
- 如何增加新的方法和案例。

### `.gitignore`

它告诉 Git 哪些本地文件不需要提交。

当前主要忽略：

- `R_library/`：机器相关的 R 包二进制文件。
- `.RData` 和 `.Rhistory`：R 会话文件。
- `.Rproj.user/`：RStudio 本地配置。
- `rsconnect/`：部署相关的本地配置。
- `shiny_*.log`：本地 Shiny 启动日志。

---

## 4. `R/` 文件夹说明

`R/` 文件夹包含 Shiny 网页和脚本入口共同使用的功能代码。

### `R/packages.R`

负责 R 包依赖管理。

主要对象和函数：

- `required_packages`
  - 记录项目运行需要的包，例如 `shiny`、`data.table`、`ggplot2` 和 `visNetwork`。
- `use_project_library(project_dir)`
  - 把项目本地 `R_library/` 加入 R 包搜索路径。
- `load_required_packages()`
  - 自动安装缺少的包，然后加载所有必需包。
- `install_missing_packages()`
  - 把当前 R 环境缺少的 package 自动安装到版本对应的项目本地包库。

调用关系：

```text
run_app.R
  -> 设置 R_library
  -> app.R
     -> packages.R
```

### `R/data_loader.R`

负责读取数据库和准备多个案例共用的数据。

主要对象和函数：

- `wide_data_files`
  - 定义数据名称与 `WIDE_*` 文件之间的对应关系。
- `load_wide_data(data_dir)`
  - 从 `data/` 读取全部 `WIDE_*` 文件。
  - 返回一个命名 list，称为 `data_bundle`。
- `safe_grep_columns()`
  - 根据列名模式安全寻找数据列。
- `prepare_cad_market_data(data_bundle)`
  - 准备多个案例共同使用的 CAD 市场数据。
  - 合并 USDCAD、加拿大利率、加拿大股指和 CAD 隔夜利率。
  - 计算 `USDCAD_ret`、`TSXC_ret` 和 `delta10y`。
- `describe_variables(example_id)`
  - 提供网页上使用的变量英文解释。

`data_bundle` 的主要结构：

```text
data_bundle$rates
data_bundle$fx
data_bundle$vol
data_bundle$eco
data_bundle$cftc
data_bundle$money_market
data_bundle$equity
data_bundle$commodity
data_bundle$credit
data_bundle$allx
```

### `R/catalog.R`

负责方法目录、方法说明、源代码映射和 Method Navigator 网络图。

主要函数：

- `get_method_catalog()`
  - 返回网页左侧的两级方法目录。
  - 每行代表一个方法页面。
  - 包含 `category`、`method_id`、`method_name` 和 `example_id`。
- `get_source_method_map()`
  - 把原始 `DataScience.R` 的代码段映射到对应 Shiny 方法页面。
  - 记录源代码行号、原方法、网页 category、method ID 和映射说明。
- `get_method_notes(method_id)`
  - 返回每个方法的英文说明。
  - 包含 When to use、Assumptions、Inputs、Outputs 和 Interpretation。
- `get_method_network()`
  - 返回 Method Navigator 网络图的节点和连接。
  - 网络图从数据问题、变量类型和分析目标引导到具体方法。

`method_id` 是整个项目连接不同文件的关键字段。

例如：

```text
method_id = "arima"
```

会同时连接：

- 左侧目录中的 ARIMA。
- Method Navigator 中的 ARIMA 节点。
- ARIMA 方法说明。
- `DataScience.R` 中 ARIMA 代码段的 Source Map。
- `examples.R` 中的 ARIMA 案例函数。

### `R/examples.R`

负责运行案例、生成模型、结果表和图表。

主要入口：

- `run_example(example_id, data_bundle)`
  - 根据 `example_id` 调用对应案例。
  - 返回统一结构的 list。

每个案例的返回结构：

```r
list(
  title = "...",
  background = "...",
  code = "...",
  table = result_table,
  plot = plot_object,
  model_summary = "..."
)
```

当前已经实现可运行案例的方法包括：

- Linear Regression
- Polynomial Regression
- Correlation
- Partial Correlation
- Logistic Regression
- ARIMA
- PCA
- Bayesian Scenario Analysis

其他方法目前使用 `method_placeholder()`：

- 网页中仍然可以找到方法目录和原脚本映射。
- 可以看到方法说明。
- 暂时不会运行完整实时案例。
- 以后可以从 `DataScience_original_reference.R` 中整理代码并补充。

---

## 5. 数据文件夹说明

### `data/`

这里保存从原项目复制过来的 `WIDE_*` 数据。

| 文件 | 在 `data_bundle` 中的名称 | 主要用途 |
|---|---|---|
| `WIDE_RATES` | `rates` | 利率和收益率数据 |
| `WIDE_FX` | `fx` | 外汇数据 |
| `WIDE_VOL` | `vol` | 波动率数据 |
| `WIDE_ECO` | `eco` | 经济数据 |
| `WIDE_CFTC` | `cftc` | CFTC 持仓数据 |
| `WIDE_MM` | `money_market` | 货币市场数据 |
| `WIDE_EQ` | `equity` | 股票和股指数据 |
| `WIDE_COMM` | `commodity` | 商品数据 |
| `WIDE_CREDIT` | `credit` | 信用数据 |
| `WIDE_ALLX` | `allx` | 汇总宏观和经济数据 |

这些数据通过 `load_wide_data()` 统一读取。

不要在案例函数中重复使用 `readRDS()`。新案例应从 `data_bundle` 中获取数据。

---

## 6. 其他目录说明

### `www/styles.css`

这是 Shiny 网页的样式文件。

它负责：

- 页面背景。
- 方法标题。
- 方法说明区域。
- 代码展示区域。
- 网页间距、颜色和文字样式。

修改网页外观时，优先修改这个文件，不要把大量 CSS 直接写入 `app.R`。

### `R_library/`

这是项目本地 R 包库。

它的作用是：

- 让当前项目可以找到所需 R 包。
- 避免依赖系统 R 的包搜索路径配置。
- 允许 `run_app.R` 在系统库缺少 Shiny 时仍然启动。

这个目录包含机器相关的二进制包，因此已经被 `.gitignore` 忽略。

---

## 7. 文件之间的调用关系

### Shiny 网页启动流程

```text
run_app.R
  |
  |-- 将 R_library/ 加入 .libPaths()
  |
  `-- 启动 app.R
        |
        |-- source R/packages.R
        |     `-- 检查并加载 R 包
        |
        |-- source R/data_loader.R
        |     `-- load_wide_data(data/)
        |           `-- 生成 data_bundle
        |
        |-- source R/catalog.R
        |     |-- 生成两级方法目录
        |     |-- 生成方法英文说明
        |     |-- 生成 Method Navigator 网络图
        |     `-- 生成 DataScience.R Source Map
        |
        |-- source R/examples.R
        |     `-- run_example(example_id, data_bundle)
        |
        `-- www/styles.css
              `-- 控制网页外观
```

### 用户在网页中选择方法后的流程

```text
用户选择方法或点击网络图节点
  |
  `-- method_id
        |
        |-- catalog.R: 找到方法名称、分类和英文说明
        |
        |-- catalog.R: 找到对应 DataScience.R Source Map
        |
        |-- examples.R: 根据 example_id 运行案例
        |
        `-- app.R: 展示背景、变量、图表、表格、模型摘要和代码
```

### 脚本式分析流程

```text
DataScience_optimized.R
  |
  |-- packages.R
  |-- data_loader.R
  |-- catalog.R
  `-- examples.R
```

因此，`DataScience_optimized.R` 和 `app.R` 使用同一套公共函数，但展示方式不同。

---

## 8. 如何新增一个方法

假设需要新增一个名为 `Stationarity Test` 的方法。

### 第一步：增加目录

在 `R/catalog.R` 的 `get_method_catalog()` 中增加：

```r
category = "Time Series"
method_id = "stationarity_test"
method_name = "Stationarity Test"
example_id = "stationarity_test"
```

### 第二步：增加方法说明

在 `get_method_notes()` 中增加：

```r
stationarity_test = c(
  when = "...",
  assumptions = "...",
  inputs = "...",
  outputs = "...",
  interpretation = "..."
)
```

### 第三步：增加原脚本映射

在 `get_source_method_map()` 中增加 `DataScience.R` 对应代码段的：

- 源代码行号。
- 源脚本 section。
- 具体方法名称。
- 网页 category。
- `method_id`。
- mapping note。

### 第四步：增加网络图节点

如果这个方法需要出现在 Method Navigator 中，在 `get_method_network()` 中增加：

- 方法节点。
- 从数据类型或分析目标到方法节点的边。

### 第五步：增加案例

在 `R/examples.R` 中：

1. 创建 `run_stationarity_test_example(data_bundle)`。
2. 在 `run_example()` 的 `switch()` 中加入映射。
3. 返回标准案例 list。

完成后，`app.R` 不需要额外增加单独页面。网页会根据 catalog 和案例函数自动展示。

---

## 9. 如何新增或修改变量解释

变量说明位于：

```text
R/data_loader.R
  -> describe_variables()
```

新增案例变量时，应补充：

- 变量名称。
- 英文含义。
- 在案例中的作用。

例如：

```r
variable = "delta10y"
meaning = "Daily change in the Canada 10-year yield."
```

---

## 10. 哪些文件应该编辑

### 经常需要编辑

- `R/catalog.R`
  - 增加方法、说明、网络图和 Source Map。
- `R/examples.R`
  - 增加或优化可运行案例。
- `R/data_loader.R`
  - 增加公共数据准备函数和变量解释。
- `www/styles.css`
  - 修改网页样式。
- `README.md`
  - 更新快速运行说明。

### 偶尔需要编辑

- `app.R`
  - 增加新的全局页面或改变网页布局时修改。
- `DataScience_optimized.R`
  - 增加脚本式研究入口时修改。
- `run_app.R`
  - 改变端口或启动方式时修改。

### 通常不要直接编辑

- `DataScience_original_reference.R`
  - 这是原始脚本参考副本。
- `data/WIDE_*`
  - 这些是数据库输入文件。
- `R_library/`
  - 这是安装生成的本地包库。

---

## 11. 当前设计的核心原则

### 原始数据结构保持不变

`WIDE_*` 数据仍然按原来的结构读取。整理工作主要发生在数据加载、案例函数和网页展示层。

### 一个 `method_id` 连接所有内容

每个方法使用唯一的 `method_id`。它连接目录、网络图、方法说明、Source Map 和案例。

### 公共计算逻辑不放在 `app.R`

`app.R` 负责交互和展示。可复用的数据处理和模型逻辑放在 `R/` 中。

### 原始脚本始终可以追溯

每个重要方法都应通过 Source Map 指向 `DataScience_original_reference.R` 中的原始代码段。

### 新案例使用统一返回结构

所有案例都通过 `run_example()` 返回相同结构，使 Shiny 页面不需要为每个方法单独编写 UI。
