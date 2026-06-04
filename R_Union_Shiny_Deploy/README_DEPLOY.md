# ECO Screener shinyapps.io Deployment

This folder is the deployment copy. The original files under `R_Union/Codex_R`
are intentionally not modified.

## Local Run

```r
shiny::runApp("C:/Users/PC/Desktop/R_git/R_Union_Shiny_Deploy")
```

## First-Time shinyapps.io Setup

Install `rsconnect` if needed:

```r
install.packages("rsconnect")
```

In shinyapps.io, open the Tokens page, click Show, and paste the generated
command into your own R console:

```r
rsconnect::setAccountInfo(
  name = "your-account-name",
  token = "your-token",
  secret = "your-secret"
)
```

Do not save the token or secret in this folder.

## Deploy

```r
rsconnect::deployApp(
  appDir = "C:/Users/PC/Desktop/R_git/R_Union_Shiny_Deploy",
  appName = "eco-screener"
)
```

