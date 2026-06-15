# ============================================================
# Full startup-cache validation
# ============================================================
# Overall role: execute app.R exactly through its startup preparation and verify
# that every method has a calculated case plus ready PNG files before Shiny runs.

app <- source("app.R")$value

method_ids <- get_method_catalog()$method_id
stopifnot(inherits(app, "shiny.appobj"))
stopifnot(all(vapply(method_ids, exists, logical(1), envir = example_cache, inherits = FALSE)))
stopifnot(all(vapply(method_ids, exists, logical(1), envir = plot_file_cache, inherits = FALSE)))

for (method_id in method_ids) {
  case <- get(method_id, envir = example_cache, inherits = FALSE)
  files <- get(method_id, envir = plot_file_cache, inherits = FALSE)
  stopifnot(identical(names(case$plots), names(files)))
  stopifnot(all(file.exists(files)))
  stopifnot(all(file.info(files)$size > 1000))
}

all_plot_files <- unlist(lapply(method_ids, get, envir = plot_file_cache, inherits = FALSE), use.names = FALSE)
names(all_plot_files) <- rep(method_ids, vapply(method_ids, function(id) length(get(id, envir = plot_file_cache)), integer(1)))
timestamps_before_click <- file.info(all_plot_files)$mtime

shiny::testServer(server, {
  session$setInputs(method_link_correlation = 1)
  session$flushReact()
  selected_case()
})
stopifnot(identical(timestamps_before_click, file.info(all_plot_files)$mtime))

Sys.sleep(1)
shiny::testServer(server, {
  session$setInputs(method_link_correlation = 1)
  session$flushReact()
  session$setInputs(rerun_case = 1)
  session$flushReact()
  selected_case()
})
timestamps_after_rerun <- file.info(all_plot_files)$mtime
stopifnot(all(timestamps_after_rerun[names(all_plot_files) == "correlation"] > timestamps_before_click[names(all_plot_files) == "correlation"]))
stopifnot(identical(
  timestamps_after_rerun[names(all_plot_files) != "correlation"],
  timestamps_before_click[names(all_plot_files) != "correlation"]
))

message("All 24 cases and their PNG files are ready before Shiny starts.")
message("Method switching preserved the cache; Re-run case replaced only the selected method.")
