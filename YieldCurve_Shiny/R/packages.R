# ============================================================
# Project package environment
# ============================================================
# This file prepares the R package library before app.R starts.
# The project library is kept first, but existing user/site libraries are also
# preserved so VSCode, Rscript, and RStudio see the same installed packages.

required_packages <- c("shiny", "bslib", "DT", "ggplot2", "plotly", "httpuv")

project_library_path <- function(project_dir) {
  version <- paste0("R-", R.version$major, ".", strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1])
  file.path(project_dir, "R_library", version)
}

use_project_library <- function(project_dir) {
  # .libPaths() controls where the current R session looks for packages.
  # Keep the project library first, then explicitly add R_LIBS_USER/R_LIBS_SITE
  # because some VSCode/Rscript sessions start with only the base R library.
  library_path <- project_library_path(project_dir)
  dir.create(library_path, recursive = TRUE, showWarnings = FALSE)

  user_library <- file.path(
    Sys.getenv("LOCALAPPDATA"),
    "R", "win-library",
    paste0(R.version$major, ".", strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1])
  )
  env_libraries <- unlist(strsplit(
    paste(Sys.getenv("R_LIBS_USER"), Sys.getenv("R_LIBS_SITE"), sep = .Platform$path.sep),
    .Platform$path.sep,
    fixed = TRUE
  ), use.names = FALSE)
  sibling_root <- dirname(project_dir)
  sibling_libraries <- c(
    file.path(sibling_root, "DataScience_Shiny", "R_library", basename(library_path)),
    file.path(sibling_root, "DataScience_Shiny", "R_library")
  )

  candidate_libraries <- unique(c(sibling_libraries, user_library, env_libraries, .libPaths()))
  candidate_libraries <- candidate_libraries[nzchar(candidate_libraries)]
  candidate_libraries <- normalizePath(candidate_libraries, winslash = "/", mustWork = FALSE)
  available_libraries <- candidate_libraries[dir.exists(candidate_libraries)]

  .libPaths(unique(c(
    normalizePath(library_path, winslash = "/", mustWork = TRUE),
    available_libraries
  )))
  invisible(library_path)
}

install_and_load_packages <- function(project_dir) {
  library_path <- use_project_library(project_dir)
  missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("Installing missing packages into ", library_path, ": ", paste(missing, collapse = ", "))
    options(timeout = max(600, getOption("timeout")))
    install.packages(
      missing,
      lib = library_path,
      repos = "https://cloud.r-project.org",
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }
  unavailable <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(unavailable) > 0) stop("Packages unavailable: ", paste(unavailable, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}
