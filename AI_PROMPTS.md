# AI Development Prompts

This file records all prompts and design decisions made with AI assistance,
so that development can be resumed at any time with any AI agent.

---

## Session 1 — 2026-02-27

**Tool**: Claude Code
**Model**: claude-sonnet-4-6
**Mode**: Plan mode (plan written and approved before any code was generated)

### Initial request (verbatim)

> I would like to have a shiny app which compares the IPCC and IPBES
> glossaries. The IPBES Glossary is available in an exported csv file in the
> folder IPBES, while the IPCC glossary needs to be scraped from the website.
> Claude gave me the code in IPCC/download_ipcc_glossary.R to do that. The
> Shiny App should do the following:
>
> 1. Give a table where one row is one definition with the following columns:
>    1. IPBES definition
>       1. one sub column for each definition of an assessment if multiple ones are there
>    2. IPCC definition
>       1. one sub column for each definition of different reports
>    3. Summarising the differences between the definitions
> 2. Sort the table alphabetically
> 3. Add a button to sort the table by "similarity" of the term. This should
>    not use the definitions provided, but definitions provided in general
>    (e.g. wikipedia or other resources)
> 4. Add a button to update the IPCC glossary from the one packaged
>
> Package the shiny app in a package form and prepare it for publishing it.

### Clarifying questions and answers

**Q: Table layout?**
A: Collapsed with expandable rows
- Main row: Term | IPBES summary | IPCC summary | Similarity %
- Expanded: per-assessment IPBES defs, per-report IPCC defs, word-level diff

**Q: Summarising differences column approach?**
A: Text diff + similarity score (algorithmic, no API key needed)

**Q: Similarity computation approach?**
A: On-demand via Wikipedia API (fetch summary per term, compute TF-IDF cosine
similarity, cache results to disk across sessions)

**Q: Publishing target?**
A: GitHub only (install via `remotes::install_github()`)

**Q: Package name?** (requested after initial plan)
A: `glossary.ipbes.ipcc`

### Additional features requested (after initial plan)

- R-conform `NEWS.md` (updated each session)
- R-conform `CONTRIBUTORS.md` mentioning Claude Code with model version and mode
- Claude Code listed in `Authors@R` in DESCRIPTION with model/mode in comment
- This `AI_PROMPTS.md` file documenting all prompts for AI continuity
- Both `NEWS.md` and `AI_PROMPTS.md` kept updated each session

### Key design decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Table component | `reactable` | Native expandable row support via `details` |
| Text similarity | TF-IDF cosine (base R) | No API key, works offline |
| Wikipedia role | Neutral reference for similarity | Avoids circular domain-specific comparison |
| IPCC caching | `tools::R_user_dir()` cache | Never mutates `inst/extdata/` at runtime |
| `diffobj` dep | `Suggests` (optional) | Falls back to built-in LCS diff |
| `shinyjs` dep | `Suggests` (optional) | Graceful degradation without it |
| Publishing | GitHub only | No CRAN compliance overhead |

### Architecture summary

```
run_app()
  ├── load_ipbes()      # reads inst/extdata/ipbes_glossary.csv
  ├── load_ipcc()       # reads cache or inst/extdata/ipcc_glossary.csv
  ├── merge_glossaries() # outer join on normalised term names
  └── shinyApp(
        ui     = build_ui(),
        server = build_server(cache_dir, initial_data)
      )
          ├── mod_table_server()       # reactable with detail rows
          └── mod_update_ipcc_server() # scrape button with progress
```

### Critical files

- `data-raw/IPBES/glossary_2026-02-23.csv` — IPBES source (moved from `IPBES/`)
- `data-raw/IPCC/download_ipcc_glossary.R` — IPCC scraper (moved from `IPCC/`)
- `data-raw/prepare_data.R` — **must be run once** before `devtools::build()`
  to populate `inst/extdata/ipcc_glossary.csv`
- `inst/extdata/ipcc_glossary.csv` — bundled IPCC snapshot (generated, then committed)
- `R/data_merge.R` — core join logic
- `R/mod_table.R` — main UI component

---

## Session 2 — 2026-02-27

**Tool**: Codex
**Model**: GPT-5
**Mode**: Default mode

### Main requests handled

- Fix HTML/tag rendering issues in overview and expanded diff blocks
- Improve typography/readability and table layout
- Switch IPCC scraper/update flow to `search.php` endpoint family (all reports)
- Clarify and refine similarity metrics:
  - within IPBES
  - within IPCC
  - between all definitions
- Rework expanded detail view into a pairwise matrix and reduce duplication
- Improve startup and refresh performance with packaged and startup caches
- Keep runtime update behavior while preserving package snapshots
- Remove unstable/unused similarity UI controls and Wikipedia-specific wiring
- Final layout refinements (column ordering, alignment, compact indicators)

### Key implementation decisions

- Startup cache precedence:
  1. packaged merged cache (`inst/extdata/merged_glossary_cache.rds`)
  2. user startup cache (`startup_merged_cache.rds`)
  3. rebuild from source CSVs
- Source-run compatibility:
  - added fallback path resolution to local `inst/` when `system.file()` is empty
- Table performance:
  - precompute table HTML fragments with cache versioning
  - lazy-load expanded detail content
- Similarity model simplification:
  - use in-package text similarity metrics from glossary definitions
  - remove unused Wikipedia similarity runtime path

### Files substantially updated in this session

- `R/app.R`
- `R/data_ipcc.R`
- `R/mod_table.R`
- `R/mod_update_ipcc.R`
- `R/server.R`
- `R/ui.R`
- `R/data_merge.R`
- `inst/www/custom.css`
- `data-raw/prepare_data.R`
- `inst/extdata/merged_glossary_cache.rds`

### Release target

- Package release prepared as `0.2.0`

---

## How to continue development with another AI agent

Provide this file and the approved plan at
`.claude/plans/async-roaming-hinton.md` to the AI, along with the current
repo state. The AI should:

1. Read `AI_PROMPTS.md` (this file) for full context and decisions
2. Read `.claude/plans/async-roaming-hinton.md` for the implementation plan
3. Check `NEWS.md` to understand what has been implemented so far
4. Run `data-raw/prepare_data.R` if `inst/extdata/ipcc_glossary.csv` is a stub
5. Continue implementing from the next incomplete step in the plan
6. Update `NEWS.md` and `AI_PROMPTS.md` at the end of the session

---

*This file is part of the package and ships to end users as documentation.*
