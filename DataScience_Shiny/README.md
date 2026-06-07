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

## Notes

The first version focuses on a searchable personal encyclopedia:

- two-level method index,
- complete `DataScience.R` source-method mapping table,
- clickable method navigator network,
- English method explanations,
- financial / macro case backgrounds,
- variable explanations,
- reusable code snippets,
- selected live examples.

No existing files outside this folder are edited.
