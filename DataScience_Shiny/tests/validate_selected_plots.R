# ============================================================
# Selected dashboard plot integration validation
# ============================================================
# Overall role: run the covered cases outside Shiny and verify their selected
# plot counts, English notes, teaching sections, and rendered PNG files.

source(file.path("R", "packages.R"))
use_project_library(getwd())
load_required_packages()
source(file.path("R", "data_loader.R"))
source(file.path("R", "catalog.R"))
source(file.path("R", "case_helpers.R"))
source(file.path("R", "selected_plots.R"))
source(file.path("R", "examples_complete.R"))

requests <- get_selected_plot_requests(file.path("UIimprove", "plot_manifest_toKeep.csv"))
stopifnot(sum(requests$keep & requests$source == "Original Reference") == 19)
stopifnot(sum(requests$keep & requests$source == "Current App") == 14)
stopifnot(identical(
  format_display_number(c(0, 10, 1.23456, 0.00012, 1000000)),
  c("0", "10", "1.235", "1.20e-04", "1.00e+06")
))

expected_counts <- c(
  independence_test = 3,
  correlation = 4,
  partial_correlation = 4,
  linear_regression = 3,
  polynomial_regression = 2,
  subset_regression = 2,
  anova = 9,
  ancova = 6
)

catalog <- get_method_catalog()
data_bundle <- load_wide_data("data")
cache_dir <- file.path(tempdir(), "datascience-selected-plot-validation")

for (method_id in names(expected_counts)) {
  example_id <- catalog$example_id[catalog$method_id == method_id]
  case <- run_example(example_id, data_bundle)
  case <- enrich_case_with_selected_plots(method_id, case, data_bundle, requests)
  files <- render_case_plot_cache(method_id, case, cache_dir)

  stopifnot(length(case$plots) == expected_counts[[method_id]])
  stopifnot(identical(names(case$plots), names(case$plot_notes)))
  stopifnot(all(nzchar(case$plot_notes)))
  stopifnot(setequal(unlist(case$visual_sections, use.names = FALSE), names(case$plots)))
  stopifnot(length(files) == expected_counts[[method_id]])
  stopifnot(all(file.exists(files)))
  stopifnot(all(file.info(files)$size > 1000))

  message(method_id, ": ", length(files), " valid PNG files")
}

message("Selected dashboard plot integration validation passed.")
