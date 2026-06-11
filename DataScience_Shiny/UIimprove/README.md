# UIimprove 原始图表图册

这个文件夹用于比较 `DataScience_original_reference.R` 的原始图和当前 Shiny App 图。
图册只帮助选择图片，目前不会把原始图批量加入正式 Encyclopedia。

## 主要文件

- `DataScience_reference_optimized.R`：原始参考脚本的可运行副本。使用项目 `data/` 路径，并为无法安装的旧 package 提供兼容实现。
- `install_gallery_packages.R`：将图册需要的 package 安装到项目自己的 `R_library/R-4.5`。
- `generate_gallery.R`：逐段运行参考脚本、保存图片、导出当前 App 图片并生成 HTML。
- `plot_manifest.csv`：记录每张图的方法、名称、来源、代码行、状态、说明和图片路径。
- `execution_errors.csv`：记录不属于绘图表达式的运行错误，方便继续检查原始脚本。
- `reference_plot_gallery.html`：按 24 个方法分组的单页图片图册。
- `images/`：图册引用的原始图和当前 App 图。

## 重新生成

在项目目录运行：

```powershell
& "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" "UIimprove/generate_gallery.R"
```

生成器会继续执行后续步骤，即使原始脚本中的某个普通计算步骤失败。最终应检查
`plot_manifest.csv`，确认所有有效绘图表达式都有非空 PNG 和清楚说明。

