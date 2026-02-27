# glossary.ipbes.ipcc (development version)

# glossary.ipbes.ipcc 0.5.0

## Improvements

* Added detailed technical background documentation as a package vignette:
  - `vignettes/background.Rmd`
  - rendered app-accessible HTML at `inst/www/background.html`
* Added in-app `Info` button opening the technical background in a new tab.
* Added app branding and attribution:
  - SIB logo
  - copyright notice for Rainer M Krug
* Added repository and issue links:
  - updated all links to `https://github.com/rkrug/glossary_ipbes_ipcc`
  - added top-of-app GitHub repo/issues links
* Added dynamic app version display under the main title:
  - shown as `Version <DESCRIPTION version>`
* Removed the `Sort Alphabetically` button and its server wiring to avoid
  crash-on-click behavior.
* Addressed package check/documentation quality items:
  - fixed roxygen argument documentation mismatches
  - regenerated affected `man/*.Rd` files
  - normalized R source to ASCII where required
  - cleaned `.Rbuildignore` entries for local hidden/development directories
  - aligned DESCRIPTION metadata (`Author`/`Maintainer`, URL/BugReports)

# glossary.ipbes.ipcc 0.2.1

## Improvements

* Added hosted-safe runtime behavior for shinyapps.io deployments:
  - automatic hosted runtime detection
  - live IPCC refresh can be disabled in hosted mode
  - explicit override via `GLOSSARY_ENABLE_LIVE_UPDATE`
* Added a top-level `app.R` deployment entrypoint for shinyapps.io/rsconnect.
* Added `scripts/deploy_shinyapps.R` to support reproducible CLI deployments.
* Added `.rsconnect/` to `.gitignore`.
* Updated deployment documentation in `README.md`.

# glossary.ipbes.ipcc 0.2.0

## Improvements

* Major startup performance improvements:
  - Bundled merged cache snapshot in `inst/extdata/merged_glossary_cache.rds`
  - Startup cache loading and invalidation metadata
  - Source-package path fallback to use local `inst/` data when running from checkout
* IPCC scraper switched to the full `search.php` endpoint family (all reports).
* Overview table and detail rendering updated:
  - Grouped identical definitions in overview
  - Pairwise similarity matrix in expanded rows
  - Lazy detail rendering to avoid large initial payloads
* Similarity display and layout refined:
  - Within-similarity indicators inside definition columns
  - Between-similarity shown in dedicated Similarity column
  - Similarity column moved to second position
* Removed `Sort by Similarity` control from the toolbar (table remains sortable by column headers).
* Removed unused Wikipedia similarity wiring and legacy cache helpers.
* Updated footer IPCC link to `https://apps.ipcc.ch/glossary/search.php`.
* Updated styling and alignment for definition/assessment-report text.

# glossary.ipbes.ipcc 0.1.0

## New features

* Initial release of the IPBES/IPCC glossary comparison Shiny app.
* Loads IPBES glossary from bundled CSV (2,228 terms across 13+ assessments).
* Bundles pre-scraped IPCC glossary snapshot (run `data-raw/prepare_data.R`
  to regenerate).
* Side-by-side comparison table with expandable rows (click any row to expand).
* Per-assessment IPBES definitions and per-report IPCC definitions in detail
  view.
* Word-level text diff (LCS algorithm) with colour-coded `<del>`/`<ins>`
  highlighting.
* "Update IPCC Glossary" button with live `withProgress()` feedback that
  re-scrapes the IPCC website.
* "Sort Alphabetically" (default) button.
* `run_app()` entry point accepting a `cache_dir` argument (defaults to
  `tools::R_user_dir("glossary.ipbes.ipcc", "cache")`).
