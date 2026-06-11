# DataScience Shiny Encyclopedia

This folder is an independent Shiny project built from `R_Union/DataScience.R`.
It does not modify the original project folders.

## Run

Recommended on this machine:

```powershell
& "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" "C:/Users/PC/Desktop/R_git/DataScience_Shiny/run_app.R"
```

Then open:

```text
http://127.0.0.1:7411
```

`run_app.R` first precomputes all 24 case studies. The terminal prints
`Precomputing case 01/24` through `24/24`, then Chrome opens automatically.
After the page opens, sidebar and Method Navigator clicks read prepared results directly.

If Shiny is installed in your normal R library, this also works:

```r
shiny::runApp("C:/Users/PC/Desktop/R_git/DataScience_Shiny")
```

The app uses a project-local package library such as `R_library/R-4.5`.
When a required package is missing, `run_app.R` automatically installs it before starting the webpage.

If `Rscript` is not on PATH on this machine, the installed executable was found at:

```text
C:/Program Files/R/R-4.5.2/bin/Rscript.exe
```

## Structure

- `app.R`: Shiny application entry point.
- `DataScience_optimized.R`: optimized script-style reference using the new path logic.
- `DataScience_original_reference.R`: copied original script for reference only.
- `R/`: package checks, data loading, method catalog, network metadata, and examples.
- `data/`: copied `WIDE_*` data files.
- `www/`: CSS.
- `UIimprove/`: original-reference plot gallery, manifest, generated PNG files, and gallery generator.

Detailed documentation:

- `PROJECT_STRUCTURE.md`: explains every file and directory.
- `CODE_GUIDE.md`: contains architecture diagrams, function connections, return values, and worked execution examples.
- `UIimprove/reference_plot_gallery.html`: compares every active original-reference plot with the current App plots.

## Notes

The current version provides a searchable, runnable personal encyclopedia:

- expandable two-level method index,
- complete `DataScience.R` source-method mapping table,
- clickable method navigator network,
- English method explanations,
- financial / macro case backgrounds,
- case-specific variable explanations,
- reusable code snippets,
- complete live case studies for all 24 catalog methods,
- multiple plots, result tables, tests, and step-by-step interpretation,
- a short dedicated explanation below every plot,
- keyword search across methods, categories, variables, plot titles, and original reference methods,
- startup-time precomputation of all 24 cases, instant cached navigation, and a fixed runtime progress bar.

## Complete Case Coverage

| Category | Methods | Main presentation |
|---|---|---|
| Statistical Relationship | Independence, Correlation, Partial Correlation | Contingency results, heatmaps, scatterplots, controlled relationships |
| Regression Models | Linear, Polynomial, Subset | Fitted relationships, residuals, coefficients, model comparison |
| Group Comparison | ANOVA, ANCOVA, MANOVA | Group distributions, Tukey comparisons, adjusted relationships, joint outcomes |
| Generalized Models | Poisson, Logistic, Confusion Matrix, ROC | Count diagnostics, probabilities, classification errors, AUC |
| Time Series | ARIMA, SARIMA, ARCH/GARCH, VAR, Granger | ACF/PACF, forecasts, volatility, dynamic systems, predictive information |
| Dimension Reduction | EFA, PCA, Rolling PCA, Cluster | Loadings, scores, explained variance, changing structure, grouping |
| Decision & Probability | Power, Bayesian Scenario | Power curves, posterior probabilities, expected and realized outcomes |

No existing files outside this folder are edited.
