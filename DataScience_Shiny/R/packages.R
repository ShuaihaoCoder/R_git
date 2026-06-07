required_packages <- c(
  "shiny",
  "bslib",
  "DT",
  "data.table",
  "ggplot2",
  "plotly",
  "visNetwork",
  "zoo"
)

project_library_path <- function(project_dir = normalizePath(".", winslash = "/", mustWork = TRUE)) {
  file.path(
    project_dir,
    "R_library",
    paste0("R-", R.version$major, ".", strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1])
  )
}

use_project_library <- function(project_dir = normalizePath(".", winslash = "/", mustWork = TRUE)) {
  version_library <- project_library_path(project_dir)
  legacy_library <- file.path(project_dir, "R_library")

  dir.create(version_library, recursive = TRUE, showWarnings = FALSE)
  .libPaths(unique(c(
    normalizePath(version_library, winslash = "/", mustWork = TRUE),
    normalizePath(legacy_library, winslash = "/", mustWork = TRUE),
    .libPaths()
  )))
  invisible(.libPaths())
}

install_missing_packages <- function(packages = required_packages, project_dir = normalizePath(".", winslash = "/", mustWork = TRUE)) {
  use_project_library(project_dir)
  target_library <- project_library_path(project_dir)
  package_directories <- file.path(target_library, packages)
  missing_packages <- packages[!dir.exists(package_directories)]

  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    message("Target library: ", target_library)
    options(timeout = max(600, getOption("timeout")))
    install.packages(
      missing_packages,
      lib = target_library,
      repos = "https://cloud.r-project.org",
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }

  load_errors <- vapply(
    packages,
    function(package) {
      tryCatch(
        {
          loadNamespace(package)
          ""
        },
        error = function(error) conditionMessage(error)
      )
    },
    character(1)
  )
  load_errors <- load_errors[nzchar(load_errors)]

  if (length(load_errors) > 0) {
    package_details <- vapply(
      names(load_errors),
      function(package) {
        package_location <- find.package(package, quiet = TRUE)
        if (!nzchar(package_location)) {
          package_location <- "<not found in .libPaths()>"
        }
        paste0(
          "- ", package, "\n",
          "  Location: ", package_location, "\n",
          "  Error: ", load_errors[[package]]
        )
      },
      character(1)
    )

    stop(
      "Required packages cannot load:\n",
      paste(package_details, collapse = "\n"), "\n\n",
      "Project library: ", target_library, "\n",
      ".libPaths(): ", paste(.libPaths(), collapse = " | "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

load_required_packages <- function(packages = required_packages, project_dir = normalizePath(".", winslash = "/", mustWork = TRUE)) {
  install_missing_packages(packages, project_dir)
  invisible(lapply(packages, library, character.only = TRUE))
}
