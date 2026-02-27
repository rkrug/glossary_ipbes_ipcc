# Deploy helper for shinyapps.io
#
# Usage:
#   SHINYAPPS_ACCOUNT=... SHINYAPPS_TOKEN=... SHINYAPPS_SECRET=... \
#   Rscript scripts/deploy_shinyapps.R
#
# Optional:
#   SHINYAPPS_APP_NAME=glossary-ipbes-ipcc

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Package 'rsconnect' is required. Install with install.packages('rsconnect').")
}

account <- "rmkrug"
app_name <- "glossary-ipbes-ipcc"

if (!nzchar(account)) {
  stop(
    "Missing shinyapps.io credentials. Set SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN, and SHINYAPPS_SECRET."
  )
}

rsconnect::deployApp(
  appDir        = ".",
  appPrimaryDoc = "app.R",
  appName       = app_name,
  account       = account,
  forceUpdate   = TRUE
)
