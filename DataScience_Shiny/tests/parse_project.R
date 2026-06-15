# Parse every project R file without executing the Shiny app.
r_files <- list.files(".", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
r_files <- r_files[!grepl("R_library", r_files, fixed = TRUE)]

for (path in r_files) {
  parse(path, encoding = "UTF-8")
  message("Parse OK: ", path)
}
