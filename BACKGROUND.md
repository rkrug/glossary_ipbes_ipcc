# Detailed Background: IPBES <-> IPCC Glossary Comparison

## 1. Purpose

This app compares glossary terms and definitions from:

- IPBES glossary
- IPCC glossary

It aligns terms, computes similarity metrics, and shows word-level differences
between definitions.

## 2. Data Sources and Artifacts

### 2.1 Bundled package snapshots

- `inst/extdata/ipbes_glossary.csv`
- `inst/extdata/ipcc_glossary.csv`
- `inst/extdata/merged_glossary_cache.rds`

These are shipped with the package and available on first startup.

### 2.2 Runtime cache (user cache dir)

The app also uses:

- `tools::R_user_dir("glossary.ipbes.ipcc", "cache")/ipcc_glossary.csv`
- `tools::R_user_dir("glossary.ipbes.ipcc", "cache")/startup_merged_cache.rds`

`ipcc_glossary.csv` in the user cache is created/updated by the "Update IPCC
Glossary" button (when enabled). It does not modify `inst/extdata`.

## 3. Scraping (IPCC)

The app and `data-raw/prepare_data.R` scrape the IPCC glossary via the
`search.php` endpoint family (all reports):

1. Collect term IDs:
   - `https://apps.ipcc.ch/glossary/ajax/ajax.searchbyindex.php?q=<PREFIX>`
   - Parses `span.alllink[data-phraseid]`
2. Fetch full occurrences/definitions:
   - `https://apps.ipcc.ch/glossary/ajax/ajax.searchalloccurance.php?q=<PHRASE_ID>&r=`
   - Reads definition from `dd p` (fallback: `dd`)
   - Reads reports from `data-report` attributes
3. Save rows:
   - columns: `id`, `prefix`, `term`, `definition`, `reports`, `downloaded_at`

Politeness/rate-limiting:

- `Sys.sleep(0.3)` between requests
- request timeout set (15s)

Term cleaning:

- Strips suffixes like `<<WGI>>` / guillemet forms from term labels.

## 4. Loading and Cleaning

### 4.1 IPBES loading

- Reads CSV from bundled snapshot.
- Normalizes key columns (concept, definition, deliverables, etc.).
- Splits multi-assessment entries into one row per assessment.
- Cleans HTML/entities with `clean_html()`.

### 4.2 IPCC loading

- Uses cache file first, then bundled snapshot.
- Ensures required columns exist.
- Cleans definitions with `clean_html()`.
- Expands semicolon-separated report list into one row per report for details.

## 5. Merging Logic

Merge is a full outer merge over normalized term keys:

1. Exact key match: `normalise_term(ipbes_concept)` vs `normalise_term(ipcc_term)`
2. Fallback for unmatched IPBES term:
   - removes parenthetical qualifier, e.g. `abundance (ecological)` -> `abundance`
3. Remaining unmatched rows are kept as IPBES-only or IPCC-only entries.

Output includes:

- term labels and counts (`ipbes_n_assessments`, `ipcc_n_reports`)
- detailed list-columns (`ipbes_data`, `ipcc_data`)
- similarity metrics (`sim_within_ipbes`, `sim_within_ipcc`, `sim_between_all`)

## 6. Similarity Calculation (Method)

All similarity scores are text-based cosine similarity on term-frequency vectors
(no external API, no Wikipedia).

### 6.1 Tokenization

For each definition text:

- lower-case
- remove non-letters (`[^a-z ]`)
- split on whitespace
- remove stopwords (internal `STOPWORDS` list)
- keep term frequencies

### 6.2 Pair similarity

For two token-frequency vectors `a` and `b`:

- dot product over shared tokens
- cosine:
  - `sim(a,b) = dot(a,b) / (||a|| * ||b||)`
- range:
  - `0` if no shared tokens
  - `NA` if an input is empty after cleaning

### 6.3 Three displayed metrics

1. Within IPBES:
   - mean of all pairwise similarities among IPBES definitions of a term
   - `NA` if fewer than 2 usable IPBES definitions
2. Within IPCC:
   - mean of all pairwise similarities among IPCC definitions of a term
   - `NA` if fewer than 2 usable IPCC definitions
3. Between All Definitions:
   - mean of all cross pairs `IPBES x IPCC`
   - `NA` if one side has no usable definitions

Important detail:

- Duplicates are intentionally kept in the similarity inputs, so repeated
  assessment/report definitions contribute to the averages.

## 7. Word-by-Word Difference (LCS Diff)

Word-level diff uses Longest Common Subsequence (LCS) over word tokens.

For a compared pair `(text_a, text_b)`:

- unchanged words -> `same`
- words only in `text_a` -> `del` (rendered as `<del>`)
- words only in `text_b` -> `ins` (rendered as `<ins>`)

Interpretation:

- "Deleted" means present only on the left/first text (`text_a`).
- "Inserted" (added) means present only on the right/second text (`text_b`).

In matrix cells, labels clarify orientation:

- `label_a` is the row/first item
- `label_b` is the column/second item

So "added" is always relative to the first (left-labeled) text.

## 8. Overview vs Expanded Detail

### 8.1 Overview table

- One row per merged term.
- Column order:
  - Term
  - Between similarity
  - IPBES definition column (includes within-IPBES similarity bar)
  - IPCC definition column (includes within-IPCC similarity bar)
- Definitions with identical text are grouped; associated
  assessments/reports are shown together (`;` separated, bold).

### 8.2 Expanded row

- Shows a single pairwise similarity matrix over grouped definitions.
- Axis order:
  - all grouped IPBES definitions first
  - then grouped IPCC definitions
- Upper triangle contains scores + word diff blocks.
- Lower triangle is empty by design (to avoid duplicate pair display).
- Very low-similarity pairs can show "Too different" based on threshold.

## 9. Startup and Caching Behavior

Startup data load order:

1. packaged merged cache (`inst/extdata/merged_glossary_cache.rds`) if valid
2. user startup cache (`startup_merged_cache.rds`) if source signatures match
3. full rebuild (load IPBES, load IPCC, merge, compute metrics)

After load, precomputed table HTML/cache fields are prepared and cached for
faster rendering.

This is why current startup is much faster than full recompute on every load.

## 10. Local vs Hosted Behavior

By default:

- Local run: live IPCC update button enabled
- shinyapps.io hosted runtime: live update disabled ("Hosted mode" label shown)

Override via env var:

- `GLOSSARY_ENABLE_LIVE_UPDATE=1` -> force enable
- `GLOSSARY_ENABLE_LIVE_UPDATE=0` -> force disable

Hosted-safe deploy script (`scripts/deploy_shinyapps.R`) sets hosted default
to disabled unless explicitly overridden.

## 11. Running Locally

### 11.1 Run app (normal)

```r
glossary.ipbes.ipcc::run_app()
```

### 11.2 Run app and force update behavior

```r
# disable update button
glossary.ipbes.ipcc::run_app(enable_live_update = FALSE)

# or via env var
Sys.setenv(GLOSSARY_ENABLE_LIVE_UPDATE = "0")
glossary.ipbes.ipcc::run_app()
```

### 11.3 Refresh packaged snapshots for release

For package-maintained snapshots (not just runtime cache):

```r
source("data-raw/prepare_data.R")
```

This regenerates:

- `inst/extdata/ipbes_glossary.csv`
- `inst/extdata/ipcc_glossary.csv`
- `inst/extdata/merged_glossary_cache.rds`

Then rebuild/reinstall package as needed.
