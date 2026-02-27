# Deploy helper for shinyapps.io
#
# Usage:
#   SHINYAPPS_ACCOUNT=... SHINYAPPS_TOKEN=... SHINYAPPS_SECRET=... \
#   Rscript scripts/deploy_shinyapps.R
#
# Optional:
#   SHINYAPPS_APP_NAME=glossary-ipbes-ipcc
#   GLOSSARY_ENABLE_LIVE_UPDATE=0

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Package 'rsconnect' is required. Install with install.packages('rsconnect').")
}

account <- Sys.getenv("SHINYAPPS_ACCOUNT", "")
token   <- Sys.getenv("SHINYAPPS_TOKEN", "")
secret  <- Sys.getenv("SHINYAPPS_SECRET", "")
app_name <- Sys.getenv("SHINYAPPS_APP_NAME", "glossary-ipbes-ipcc")

if (!nzchar(account) || !nzchar(token) || !nzchar(secret)) {
  stop(
    "Missing shinyapps.io credentials. Set SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN, and SHINYAPPS_SECRET."
  )
}

# Hosted-safe default: disable live IPCC scraping unless explicitly overridden.
if (!nzchar(Sys.getenv("GLOSSARY_ENABLE_LIVE_UPDATE", ""))) {
  Sys.setenv(GLOSSARY_ENABLE_LIVE_UPDATE = "0")
}

rsconnect::setAccountInfo(name = account, token = token, secret = secret)

rsconnect::deployApp(
  appDir        = ".",
  appPrimaryDoc = "app.R",
  appName       = app_name,
  account       = account,
  envVars       = c("GLOSSARY_ENABLE_LIVE_UPDATE"),
  forceUpdate   = TRUE
)
