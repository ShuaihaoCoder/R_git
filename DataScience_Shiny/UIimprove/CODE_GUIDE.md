# UIimprove 代码导引图

`UIimprove` 是 Data Science Encyclopedia 的图片筛选工作区。它把原始参考脚本中的图和当前 App 中的图统一导出到一个 HTML 图册，方便逐方法比较。

它不会在用户平时运行 `run_app.R` 时自动执行，也不会直接改变正式 Encyclopedia 页面。只有手动运行 `generate_gallery.R` 时，才会重新生成图册文件。

---

## 1. 文件之间的总关系

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    User["用户运行<br/>UIimprove/generate_gallery.R"]

    Generator["generate_gallery.R<br/>图册生成总入口"]
    Packages["install_gallery_packages.R<br/>需要时单独安装图册 packages"]
    Optimized["DataScience_reference_optimized.R<br/>可运行的原始脚本副本"]

    Original["../DataScience_original_reference.R<br/>只读取原始行号和原始绘图代码"]
    Data["../data/WIDE_*<br/>原始分析数据"]
    Catalog["../R/catalog.R<br/>24 个方法和原代码映射"]
    App["../app.R + ../R/examples_complete.R<br/>当前 App 的 24 个案例和 60 张图"]
    Library["../R_library/R-4.5<br/>项目自己的 package 文件夹"]

    Images["images/<br/>114 张原始图 + 60 张 App 图"]
    Manifest["plot_manifest.csv<br/>每张图的来源、方法、名称、路径和说明"]
    Errors["execution_errors.csv<br/>非绘图步骤的错误记录"]
    Gallery["reference_plot_gallery.html<br/>按 24 个方法分组的最终图册"]

    User --> Generator
    User -. "缺 package 时先手动运行" .-> Packages
    Packages -- "安装 package" --> Library

    Generator -- "source packages.R 并使用" --> Library
    Generator -- "parse() + 逐段 eval()" --> Optimized
    Optimized -- "readRDS()" --> Data
    Generator -- "读取原始行和代码文字" --> Original
    Generator -- "get_method_catalog() + get_source_method_map()" --> Catalog
    Generator -- "source(app.R)，读取 example_cache" --> App

    Optimized -- "原始绘图表达式" --> Generator
    App -- "case$plots + case$plot_notes" --> Generator
    Generator -- "保存 PNG" --> Images
    Generator -- "汇总每张图记录" --> Manifest
    Generator -- "记录普通执行错误" --> Errors
    Images --> Gallery
    Manifest --> Gallery
```

### 哪些文件会直接参与图册生成

| 文件 | 类型 | 谁使用它 | 输入 | 返回或产生什么 |
|---|---|---|---|---|
| `generate_gallery.R` | 实际运行入口 | 用户手动运行 | 项目路径、参考脚本、当前 App 案例 | PNG、CSV 和 HTML |
| `DataScience_reference_optimized.R` | 实际运行脚本 | `generate_gallery.R` 逐段执行 | `data/WIDE_*` 和图册 packages | 原始模型对象和原始图 |
| `install_gallery_packages.R` | 独立安装工具 | 用户需要时手动运行 | package 清单、项目 library 路径 | 安装到 `R_library/R-4.5` 的 packages |
| `README.md` | 说明文档 | 用户阅读 | 无 | 简短运行方式 |
| `CODE_GUIDE.md` | 导引文档 | 用户阅读 | 无 | 文件关系和运行路线 |

### 哪些文件是生成结果

| 文件 | 谁生成 | 内容 | 后来给谁使用 |
|---|---|---|---|
| `images/` | `generate_gallery.R` | 174 张 PNG | `reference_plot_gallery.html` |
| `plot_manifest.csv` | `generate_gallery.R` | 174 张图的 metadata | HTML 生成、人工筛选、自动检查 |
| `execution_errors.csv` | `generate_gallery.R` | 非绘图表达式错误 | 调试优化副本；当前结果为 0 条错误 |
| `reference_plot_gallery.html` | `generate_gallery.R` | 24 个方法的图片对比图册 | 用户浏览并选择正式 App 要保留的图 |

---

## 2. `generate_gallery.R`：整个流程如何运行

`generate_gallery.R` 是本文件夹中最重要的文件。它先处理原始参考图，再处理当前 App 图，最后把两类图片合并成一个图册。

```mermaid
%%{init: {'sequence': {'actorMargin': 60, 'messageMargin': 45}, 'themeVariables': {'fontSize': '16px'}}}%%
sequenceDiagram
    actor User as 用户
    participant Generator as generate_gallery.R
    participant Reference as DataScience_reference_optimized.R
    participant Original as DataScience_original_reference.R
    participant App as app.R / example_cache
    participant Output as UIimprove 输出文件

    User->>Generator: Rscript.exe UIimprove/generate_gallery.R
    Generator->>Generator: find_project_dir() -> project_dir
    Generator->>Original: readLines() -> original_lines
    Generator->>Reference: parse(keep.source=TRUE) -> expressions + source line

    loop 每一个参考脚本表达式
        Generator->>Generator: is_plot_call(expression)
        alt 是绘图表达式
            Generator->>Reference: eval(expression, run_environment)
            Reference-->>Generator: ggplot / base plot / plot object
            Generator->>Output: 保存 original_*.png + manifest row
        else 是普通计算表达式
            Generator->>Reference: eval(expression, run_environment)
            Reference-->>Generator: 更新 run_environment 中的变量或模型
            Generator->>Output: 失败时记录 execution_errors.csv
        end
    end

    Generator->>App: source(app.R) -> 预计算 24 个案例
    App-->>Generator: example_cache 中的 case$plots + case$plot_notes

    loop 每个方法的每张当前 App 图
        Generator->>Output: ggsave() -> app_方法ID_编号.png
        Generator->>Output: 添加 Current App manifest row
    end

    Generator->>Output: write.csv() -> plot_manifest.csv
    Generator->>Output: write_gallery_html() -> reference_plot_gallery.html
    Output-->>User: 打开图册比较原始图与当前 App 图
```

### 四个实际执行阶段

```text
Step 1/4: Running original reference plots
Step 2/4: Exporting current App plots
Step 3/4: Writing manifest
Step 4/4: Writing HTML gallery
```

最后一次验证结果：

```text
Original Reference: 114 / 114 张成功
Current App:        60 张成功
Manifest:           174 行
Missing images:     0
Execution errors:   0
```

---

## 3. 原始参考图如何从代码变成 PNG

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    Parse["parse(DataScience_reference_optimized.R,<br/>keep.source = TRUE)"]
    Expressions["expressions<br/>按原脚本顺序排列的代码表达式"]
    PlotCheck["is_plot_call(expression)<br/>递归检查是否包含 ggplot()、plot()、acf() 等"]

    Normal["普通计算表达式"]
    EvalNormal["eval(expression, run_environment)"]
    Environment["run_environment<br/>不断累积数据、模型和中间变量"]

    Plot["绘图表达式"]
    Method["infer_method_id(source_line, code)<br/>映射到 ARIMA、PCA、VAR 等方法"]
    Name["expression_name()<br/>取得图对象名或生成 source_plot_编号"]
    Save["save_result_plot()<br/>打开 PNG -> eval() -> print() -> dev.off()"]
    Png["images/original_编号_图名.png"]
    Row["Original Reference manifest row"]

    Parse --> Expressions --> PlotCheck
    PlotCheck -- "FALSE" --> Normal --> EvalNormal --> Environment
    Environment -- "后续表达式继续使用已有变量" --> PlotCheck
    PlotCheck -- "TRUE" --> Plot --> Method --> Name --> Save
    Environment -- "绘图依赖的数据和模型" --> Save
    Save --> Png
    Method --> Row
    Name --> Row
    Png --> Row
```

### 变量如何逐步变化

`run_environment` 是原始脚本执行过程中共享的工作空间。普通表达式创建的变量会留在里面，后面的绘图表达式才能继续使用。

```text
开始：
run_environment = 空环境

执行数据准备后：
run_environment = WIDE 数据 + test1 + merged_dt + test_cad_date + ...

执行模型后：
run_environment = 前面的数据 + fit1 + afit2 + var_model + pca_res + ...

执行绘图后：
run_environment = 前面的数据和模型 + NFP_RISK_Effect + PCA_Biplot + ...
images/          = 新增对应 PNG
manifest         = 新增对应图片记录
```

### 例子：原始 PCA 图如何生成

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    Data["data/WIDE_*"]
    Prepare["优化副本准备 pca_data"]
    Fit["prcomp(pca_data, scale.=TRUE)<br/>返回 pca_res"]
    Loadings["pca_res$rotation<br/>变量载荷"]
    Scores["pca_res$x<br/>样本主成分得分"]
    Plot["PCA_Biplot = ggplot(...)<br/>输入 Scores + Loadings"]
    Save["save_result_plot()"]
    Image["images/original_*_PCA_Biplot.png"]
    Manifest["plot_manifest.csv 新增一行<br/>method_id = pca<br/>source = Original Reference"]
    Html["HTML 的 PCA 区域显示图片、代码和说明"]

    Data --> Prepare --> Fit
    Fit --> Loadings --> Plot
    Fit --> Scores --> Plot
    Plot --> Save --> Image --> Html
    Plot --> Manifest --> Html
```

---

## 4. 当前 App 图片如何进入图册

当前 App 图片不是从 HTML 截图取得，而是直接读取 `app.R` 预计算后的 `example_cache`。

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    Generator["build_current_app_gallery()"]
    SourceApp["source(../app.R)"]
    Preload["preload_all_examples()<br/>运行 24 个案例"]
    Cache["example_cache<br/>名称是 method_id"]
    LoopMethod["依次读取每个 method_id"]
    Case["case = get(method_id, example_cache)"]
    Plots["case$plots<br/>当前方法的命名图 list"]
    Notes["case$plot_notes<br/>与图片同名的说明"]
    Save["ggsave()<br/>保存 app_方法ID_编号.png"]
    AppRow["Current App manifest row"]
    Html["reference_plot_gallery.html"]

    Generator --> SourceApp --> Preload --> Cache --> LoopMethod --> Case
    Case --> Plots --> Save --> AppRow --> Html
    Case --> Notes --> AppRow
```

### 例子：导出当前 App 的 VAR 图片

```text
method_id = "var"

get("var", envir = example_cache) 返回：
case$plots      = VAR 动态关系相关图片
case$plot_notes = 每张 VAR 图片下方的教学说明

保存结果：
images/app_var_01.png
images/app_var_02.png
images/app_var_03.png

manifest 中 source：
"Current App"
```

---

## 5. `DataScience_reference_optimized.R` 的作用

这个文件是 `DataScience_original_reference.R` 的可运行副本。图册生成器实际执行的是优化副本，但仍读取原始文件来展示原始代码文字和参考位置。

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart LR
    Original["DataScience_original_reference.R<br/>只读参考，不修改"]
    Optimized["DataScience_reference_optimized.R<br/>保留原始算法和绘图逻辑"]
    Changes["为运行图册做的必要兼容<br/>项目相对路径 / package 兼容 / 旧数据替代"]
    Generator["generate_gallery.R<br/>逐段执行优化副本"]
    Gallery["图册中仍标记<br/>Original Reference"]

    Original -- "复制并保持主要逻辑" --> Optimized
    Changes --> Optimized
    Optimized --> Generator --> Gallery
    Original -- "提供代码文字和原始参考信息" --> Gallery
```

主要兼容处理：

- 删除原脚本中的硬编码 `setwd("G:/...")`，改读 `DataScience_Shiny/data/WIDE_*`。
- 不连接 Bloomberg，也不依赖图册生成不需要的外部环境。
- `ggbiplot` 无稳定 Windows package 时，使用统计含义相同的 PCA biplot 实现。
- `robust` 示例数据不可用时，使用 `MASS::epil` 建立相同字段结构的 Poisson 教学案例。

---

## 6. `install_gallery_packages.R` 的独立路线

这个文件只负责安装原始参考脚本绘图需要的额外 packages。它不是每次生成图册都必须运行。

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    User["用户手动运行 install_gallery_packages.R"]
    Find["找到 project_dir"]
    Packages["gallery_packages<br/>原始参考分析需要的 package 名称"]
    Library["project_dir/R_library/R-4.5"]
    Check["检查 package 及依赖是否已安装"]
    Install["install.packages(..., dependencies=TRUE)"]
    Ready["generate_gallery.R 后续可以加载并使用"]

    User --> Find --> Packages --> Check
    Library --> Check
    Check -- "缺少 package" --> Install --> Library
    Check -- "已经存在" --> Ready
    Library --> Ready
```

它不会把 package 安装到其他项目目录，主要目标是让 VSCode、RStudio 和命令行运行图册时使用同一套项目环境。

---

## 7. 输出文件如何互相引用

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    Manifest["plot_manifest.csv<br/>174 行"]
    ImagePath["image_path<br/>例如 images/app_var_01.png"]
    Images["images/<br/>174 张 PNG"]
    Method["method_id<br/>例如 var、pca、garch"]
    Source["source<br/>Original Reference / Current App"]
    Explanation["explanation<br/>图片用途和视觉元素"]
    HtmlWriter["write_gallery_html(manifest)"]
    Sections["按 24 个 method_id 建立 HTML section"]
    Cards["每一行 manifest 建立一张 plot-card"]
    Gallery["reference_plot_gallery.html"]

    Manifest --> ImagePath --> Images
    Manifest --> Method --> Sections
    Manifest --> Source --> Cards
    Manifest --> Explanation --> Cards
    Images --> Cards
    Manifest --> HtmlWriter --> Sections --> Cards --> Gallery
```

`reference_plot_gallery.html` 本身不把图片复制进去，而是通过 `image_path` 引用 `images/` 中的 PNG。因此移动 HTML 时，也要一起保留 `images/` 文件夹。

---

## 8. 一次完整重新生成案例

用户在项目目录运行：

```powershell
& "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" "UIimprove/generate_gallery.R"
```

```mermaid
%%{init: {'flowchart': {'nodeSpacing': 55, 'rankSpacing': 85}, 'themeVariables': {'fontSize': '17px'}}}%%
flowchart TB
    Start["运行命令"]
    Project["find_project_dir()<br/>返回 DataScience_Shiny"]
    Reference["逐段执行优化参考脚本"]
    OriginalPng["生成或覆盖 114 张 original_*.png"]
    Current["source(app.R)<br/>预计算 24 个案例"]
    AppPng["生成或覆盖 60 张 app_*.png"]
    Combine["reference_manifest + app_manifest"]
    Csv["覆盖 plot_manifest.csv<br/>174 行"]
    Errors["覆盖 execution_errors.csv<br/>当前 0 条错误"]
    Html["覆盖 reference_plot_gallery.html"]
    Review["用户按方法浏览图册<br/>决定哪些原始图加入正式 App"]

    Start --> Project --> Reference --> OriginalPng
    OriginalPng --> Current --> AppPng --> Combine
    Combine --> Csv --> Html --> Review
    Reference --> Errors
```

重新生成只覆盖图册产物，不会修改：

- `DataScience_original_reference.R`
- 正式 App 的案例代码
- `data/WIDE_*`
- 项目外的任何文件

---

## 9. 阅读这个文件夹的推荐顺序

1. 先打开 `reference_plot_gallery.html`，理解这个文件夹最终解决什么问题。
2. 查看 `plot_manifest.csv`，理解 HTML 每张图片的信息来自哪里。
3. 阅读 `generate_gallery.R`，重点看 `build_reference_gallery()`、`build_current_app_gallery()` 和 `write_gallery_html()`。
4. 阅读 `DataScience_reference_optimized.R`，查看原始模型和绘图逻辑。
5. 只有 package 缺失时，再查看或运行 `install_gallery_packages.R`。

