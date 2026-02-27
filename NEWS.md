# glossary.ipbes.ipcc (development version)

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
* Similarity scoring via Wikipedia REST API (TF-IDF cosine similarity, cached
  to disk across sessions).
* "Update IPCC Glossary" button with live `withProgress()` feedback that
  re-scrapes the IPCC website.
* "Sort Alphabetically" (default) and "Sort by Similarity" buttons.
* `run_app()` entry point accepting a `cache_dir` argument (defaults to
  `tools::R_user_dir("glossary.ipbes.ipcc", "cache")`).
