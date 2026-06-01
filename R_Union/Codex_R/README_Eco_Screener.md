# Eco Screener Optimized

This folder contains the optimized Eco Screener work only. The original files in
`C:/Users/PC/Desktop/R_git/R_Union` were not modified.

## Files

- `Eco_screener_optimized.R`: function-based script version.
- `app.R`: Shiny dashboard version.
- `output/`: generated CSV and image outputs when the script is run.

## Required Packages

Install these once in R:

```r
install.packages(c(
  "data.table",
  "dplyr",
  "tidyr",
  "zoo",
  "scales",
  "ggplot2",
  "stringr",
  "shiny",
  "plotly",
  "DT"
))
```

## Run The Script

From R:

```r
setwd("C:/Users/PC/Desktop/R_git/R_Union/Codex_R")
source("Eco_screener_optimized.R")

result <- run_eco_screener("2025-11-03")
save_eco_outputs(result)
```

If no date is provided in an interactive R session, the script can show a date
selection dialog through `select_eco_date()`.

From a terminal:

```powershell
& "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" `
  "C:/Users/PC/Desktop/R_git/R_Union/Codex_R/Eco_screener_optimized.R" `
  "2025-11-03"
```

## Run The Dashboard

From R:

```r
shiny::runApp("C:/Users/PC/Desktop/R_git/R_Union/Codex_R")
```

The dashboard reads valid dates directly from `WIDE_ALLX` and shows them in the
date selector. It includes:

- interactive scoreboard heatmap,
- radar chart with country multi-select,
- faceted indicator chart,
- searchable/exportable data table.
