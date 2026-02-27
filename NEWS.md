# glossary.ipbes.ipcc (development version)

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
