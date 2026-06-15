# Run the real app on a validation-only port without opening another browser.
source(file.path("R", "packages.R"))
use_project_library(getwd())
install_missing_packages(required_packages, getwd())
shiny::runApp(".", host = "127.0.0.1", port = 7412, launch.browser = FALSE)
