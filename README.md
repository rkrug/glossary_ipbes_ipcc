# glossary.ipbes.ipcc

<!-- badges: start -->
[![R CMD CHECK](https://github.com/ipbes/glossary_ipbes_ipcc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ipbes/glossary_ipbes_ipcc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R package containing a Shiny app for comparing the
[IPBES](https://www.ipbes.net/) biodiversity glossary and the
[IPCC](https://www.ipcc.ch/) climate change glossary side-by-side.

## Features

- **Side-by-side comparison** table with one row per glossary term
- **Expandable rows**: click any term to see definitions broken down by IPBES
  assessment and IPCC report
- **Word-level text diff**: colour-coded differences between IPBES and IPCC
  definitions
- **Similarity scoring**: within-source and between-source text similarity
- **Sort alphabetically** (default) and sortable similarity columns
- **Update IPCC glossary** button that re-scrapes the live
  [IPCC Glossary website](https://apps.ipcc.ch/glossary/) with real-time
  progress feedback

## Installation

```r
# Install from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ipbes/glossary_ipbes_ipcc")
```

## Usage

```r
glossary.ipbes.ipcc::run_app()
```

The app stores its cache (updated IPCC data, merged table cache) in
`tools::R_user_dir("glossary.ipbes.ipcc", "cache")`.  No manual setup is
required.

## Deploying to shinyapps.io

Use the deploy helper script:

```bash
SHINYAPPS_ACCOUNT=... SHINYAPPS_TOKEN=... SHINYAPPS_SECRET=... \
Rscript scripts/deploy_shinyapps.R
```

This deploys `app.R` and sets hosted-safe mode by default
(`GLOSSARY_ENABLE_LIVE_UPDATE=0`), which disables live IPCC scraping on hosted
instances. Local runs keep live update enabled by default.

## Data sources

| Source | URL | Bundled snapshot |
|--------|-----|-----------------|
| IPBES Glossary | https://www.ipbes.net/glossary | `inst/extdata/ipbes_glossary.csv` (2026-02-23, 2,228 terms) |
| IPCC Glossary  | https://apps.ipcc.ch/glossary/ | `inst/extdata/ipcc_glossary.csv` (run `data-raw/prepare_data.R` to regenerate) |

## Developer notes

### Regenerating the bundled IPCC data

The bundled IPCC CSV must be generated before building the package:

```r
source("data-raw/prepare_data.R")
```

This runs the IPCC scraper (takes several minutes) and writes
`inst/extdata/ipcc_glossary.csv`.  Commit the result to git.

### AI development log

This package was developed with AI assistance. See `AI_PROMPTS.md` for the
full prompt history and design decisions, enabling any AI agent to continue
development.

## License

MIT © 2026 IPBES TSU Knowledge and Data
