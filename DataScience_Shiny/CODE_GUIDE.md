# DataScience Shiny 代码导引图

## 整体结构

项目可以理解为四层：启动、方法定义、数据与计算、网页展示。

```mermaid
flowchart TD
    User["用户在 VSCode 运行 run_app.R"]
    Launcher["run_app.R<br/>find_project_dir()"]
    Packages["R/packages.R<br/>包目录与依赖"]
    App["app.R<br/>UI + server"]
    Loader["R/data_loader.R<br/>读取与准备数据"]
    Catalog["R/catalog.R<br/>目录、说明、导航、映射"]
    Examples["R/examples.R<br/>运行具体案例"]
    Data["data/WIDE_*"]
    Browser["浏览器<br/>文字、网络图、表格、交互图"]

    User --> Launcher --> Packages --> App
    App --> Loader --> Data
    App --> Catalog
    App --> Examples --> Loader
    App --> Browser
```

## 文件引用与返回值

```mermaid
flowchart LR
    Run["run_app.R"]
    Packages["R/packages.R"]
    App["app.R"]
    Loader["R/data_loader.R"]
    Catalog["R/catalog.R"]
    Examples["R/examples.R"]
    Wide["data/WIDE_*"]

    Run -- "source(); 获得包管理函数" --> Packages
    Run -- "shiny::runApp(project_dir)" --> App
    App -- "source(); 获得数据函数" --> Loader
    App -- "source(); 获得目录与网络函数" --> Catalog
    App -- "source(); 获得案例函数" --> Examples
    Loader -- "readRDS(); 返回 data_bundle" --> Wide
    Examples -- "prepare_cad_market_data()" --> Loader
    Catalog -- "返回目录、说明、nodes、edges、source map" --> App
    Examples -- "返回文字、表、图、模型摘要" --> App
```

| 文件与函数 | 输入 | 返回 | 返回给谁 |
|---|---|---|---|
| `run_app.R / find_project_dir()` | 无 | 项目根目录路径 | `run_app.R` |
| `packages.R / project_library_path()` | 项目路径 | 当前 R 版本的包目录 | `run_app.R` |
| `data_loader.R / load_wide_data()` | `data/` 路径 | `data_bundle` 命名 list | `app.R`、案例函数 |
| `data_loader.R / prepare_cad_market_data()` | `data_bundle` | CAD 市场分析表 | 多个案例函数 |
| `catalog.R / get_method_catalog()` | 无 | 方法目录 data.frame | 左侧目录、方法详情 |
| `catalog.R / get_method_network()` | 无 | `nodes` 和 `edges` | Method Navigator |
| `catalog.R / get_source_method_map()` | 无 | 原脚本映射表 | Source Map |
| `examples.R / run_example()` | `example_id`、`data_bundle` | 标准案例 list | `app.R` |

## 网页启动顺序

```mermaid
sequenceDiagram
    participant U as 用户
    participant R as run_app.R
    participant P as packages.R
    participant A as app.R
    participant D as data_loader.R
    participant C as catalog.R
    participant B as 浏览器

    U->>R: source("run_app.R")
    R->>R: find_project_dir()
    R->>P: use_project_library() / install_missing_packages()
    R->>A: shiny::runApp(project_dir)
    A->>D: load_wide_data(data_dir)
    D-->>A: data_bundle
    A->>C: get_method_catalog / network / source map
    C-->>A: 方法定义与导航数据
    A->>B: 发送网页 UI
    B-->>U: 显示网页
```

- `run_app.R` 不做统计分析，只负责启动。
- `app.R` 启动时读取一次数据和目录定义。
- 用户选择具体方法时，才运行对应案例函数。

## 例子一：选择 Linear Regression

```mermaid
sequenceDiagram
    participant U as 用户
    participant A as app.R server
    participant C as catalog.R
    participant E as examples.R
    participant D as data_loader.R
    participant B as 浏览器

    U->>A: 选择 linear_regression
    A->>C: 用 method_id 找目录与说明
    C-->>A: 方法名称、分类、说明
    A->>E: run_example("linear_regression", data_bundle)
    E->>D: prepare_cad_market_data(data_bundle)
    D-->>E: cad_data
    E->>E: lm(USDCAD_ret ~ TSXC_ret + delta10y)
    E-->>A: 标题、背景、系数表、图、summary
    A->>B: renderUI / renderDT / renderPlotly / renderText
    B-->>U: 显示案例页面
```

具体逻辑：

1. `selected_method()` 得到 `"linear_regression"`。
2. `selected_catalog_row()` 从目录找到方法信息。
3. `selected_example()` 调用 `run_example()`。
4. `run_example()` 路由到 `run_linear_regression_example()`。
5. 案例函数准备数据、运行 `lm()`，返回标准案例 list。
6. `app.R` 将 list 的各部分放入网页。

## 例子二：点击 Method Navigator 的 ARIMA

```mermaid
flowchart TD
    Click["点击 ARIMA 节点"]
    Input["input$method_network_selected = arima"]
    Observe["observeEvent() 监听点击"]
    Select["updateSelectInput()<br/>选择器改为 ARIMA"]
    Tab["updateTabsetPanel()<br/>跳到 Method Detail"]
    Reactive["selected_method() 自动更新"]
    Example["run_example('arima', data_bundle)"]
    Result["显示 ARIMA 结果"]

    Click --> Input --> Observe --> Select --> Tab --> Reactive --> Example --> Result
```

普通 R 脚本通常从上到下执行；Shiny 使用 `reactive()` 和 `observeEvent()`，
在用户点击或选择发生时自动触发相关计算。

## 例子三：Source Map 如何连接原脚本

```mermaid
flowchart LR
    Original["DataScience_original_reference.R<br/>原方法代码段"]
    Map["catalog.R<br/>get_source_method_map()"]
    MethodID["method_id"]
    Detail["Method Detail 页面"]
    SourceTab["Source Map tab"]

    Original --> Map --> MethodID --> Detail
    Map --> SourceTab
```

`get_source_method_map()` 不执行原始代码。它记录原代码的位置、方法内容、
对应网页 `method_id` 和映射理由，使原脚本可以追溯。

## 标准案例返回结构

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

```mermaid
flowchart LR
    Example["run_*_example()"]
    List["标准案例 list"]
    Header["method_header"]
    Plot["example_plot"]
    Table["result_table"]
    Summary["model_summary"]
    Code["code_snippet"]

    Example --> List
    List --> Header
    List --> Plot
    List --> Table
    List --> Summary
    List --> Code
```

所有案例返回相同结构，因此新增案例时通常不需要修改网页布局。

## 新增方法的代码路径

```mermaid
flowchart TD
    A["catalog.R<br/>get_method_catalog()<br/>增加 method_id"]
    B["catalog.R<br/>get_method_notes()<br/>增加说明"]
    C["catalog.R<br/>get_source_method_map()<br/>增加原脚本映射"]
    D["examples.R<br/>增加 run_*_example()"]
    E["examples.R<br/>run_example() 增加路由"]
    F["可选：get_method_network()<br/>增加节点和边"]
    G["网页自动出现新方法"]

    A --> B --> C --> D --> E --> F --> G
```

新增代码时遵守两个规则：

1. 数据读取与公共准备逻辑放在 `data_loader.R`。
2. 模型计算与绘图放在 `examples.R`，不要堆进 `app.R`。

