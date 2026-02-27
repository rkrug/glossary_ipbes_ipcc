# IPCC Glossary Downloader (R version)
# =====================================
# Downloads all terms and definitions from the IPCC Glossary at:
# https://apps.ipcc.ch/glossary/
#
# Discovered from the site's source code:
#   Terms by letter: ajax/ajax.searchbyindex.php?q={LETTER}
#   Term detail:     ajax/ajax.searchalloccurance.php?q={ID}&r=
#
# Usage:
#   install.packages(c("httr", "rvest", "jsonlite", "dplyr"))
#   source("download_ipcc_glossary.R")
#
# Output:
#   ipcc_glossary.json  - All terms with full definitions
#   ipcc_glossary.csv   - Flat CSV version

library(httr)
library(rvest)
library(jsonlite)
library(dplyr)

BASE_URL <- "https://apps.ipcc.ch/glossary"

PREFIXES <- c("123", "A", "B", "C", "D", "E", "F", "G", "H", "I",
              "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
              "T", "U", "V", "W", "Y", "Z")

SESSION_HEADERS <- add_headers(
  `User-Agent` = "Mozilla/5.0 (research/data-access; contact: your@email.com)",
  `Referer`    = "https://apps.ipcc.ch/glossary/search.php"
)


# -- Fetch raw HTML for a letter prefix ---------------------------------------

fetch_terms_by_letter <- function(prefix) {
  url <- paste0(BASE_URL, "/ajax/ajax.searchbyindex.php?q=", prefix)
  tryCatch({
    resp <- GET(url, SESSION_HEADERS, timeout(15))
    stop_for_status(resp)
    content(resp, as = "text", encoding = "UTF-8")
  }, error = function(e) {
    message("  Error fetching '", prefix, "': ", conditionMessage(e))
    NULL
  })
}


# -- Parse list of terms from letter-index HTML -------------------------------

parse_term_list <- function(html, prefix) {
  page <- read_html(html)

  # Primary: <span> or any tag with data-id + data-term (matches JS showterm())
  nodes <- page |> html_elements("[data-id]")

  if (length(nodes) > 0) {
    ids   <- html_attr(nodes, "data-id")
    terms <- html_attr(nodes, "data-term")
    # Fall back to element text if data-term is missing
    terms <- ifelse(is.na(terms), html_text(nodes, trim = TRUE), terms)
    valid <- !is.na(ids) & nchar(ids) > 0
    return(data.frame(id = ids[valid], term = terms[valid],
                      prefix = prefix, stringsAsFactors = FALSE))
  }

  # Fallback: <a> tags with data-id
  links <- page |> html_elements("a[data-id]")
  if (length(links) > 0) {
    ids   <- html_attr(links, "data-id")
    terms <- html_text(links, trim = TRUE)
    valid <- !is.na(ids) & nchar(ids) > 0
    return(data.frame(id = ids[valid], term = terms[valid],
                      prefix = prefix, stringsAsFactors = FALSE))
  }

  data.frame(id = character(), term = character(),
             prefix = character(), stringsAsFactors = FALSE)
}


# -- Fetch full definition HTML for a single term ID -------------------------

fetch_term_detail <- function(term_id) {
  url <- paste0(BASE_URL, "/ajax/ajax.searchalloccurance.php?q=", term_id, "&r=")
  tryCatch({
    resp <- GET(url, SESSION_HEADERS, timeout(15))
    stop_for_status(resp)
    content(resp, as = "text", encoding = "UTF-8")
  }, error = function(e) {
    message("    Error fetching detail for id=", term_id, ": ", conditionMessage(e))
    NULL
  })
}


# -- Parse definition and metadata from detail HTML --------------------------

parse_term_detail <- function(html) {
  page <- read_html(html)

  # Try named definition containers first
  def_node <- page |>
    html_element(".definition, .desc, .content, [class*='definition'], [class*='desc']")

  if (!is.null(def_node) && length(def_node) > 0) {
    definition <- html_text(def_node, trim = TRUE)
  } else {
    definition <- page |> html_text(trim = TRUE)
    definition <- gsub("\\s+", " ", definition)
  }

  # Collect report references from data-report attributes
  report_nodes <- page |> html_elements("[data-report]")
  reports <- unique(html_attr(report_nodes, "data-report"))
  reports <- reports[!is.na(reports) & nchar(reports) > 0]

  list(
    definition = definition,
    reports    = paste(reports, collapse = "; "),
    raw_html   = html
  )
}


# =============================================================================
# Main
# =============================================================================

cat("=== IPCC Glossary Downloader (R) ===\n\n")

# -- Step 1: Collect all term IDs by letter -----------------------------------

cat("Step 1: Collecting term list by letter...\n")
term_stubs <- data.frame(id = character(), term = character(),
                         prefix = character(), stringsAsFactors = FALSE)

for (prefix in PREFIXES) {
  cat(sprintf("  Fetching letter '%s'... ", prefix))
  html <- fetch_terms_by_letter(prefix)

  if (!is.null(html)) {
    terms <- parse_term_list(html, prefix)
    cat(sprintf("%d terms\n", nrow(terms)))

    if (nrow(terms) == 0) {
      debug_file <- paste0("debug_", prefix, ".html")
      writeLines(html, debug_file)
      cat(sprintf("    (saved %s for inspection)\n", debug_file))
    }

    term_stubs <- bind_rows(term_stubs, terms)
  } else {
    cat("failed\n")
  }

  Sys.sleep(0.3)  # be polite
}

# Remove duplicates
term_stubs <- term_stubs[!duplicated(term_stubs$id), ]
cat(sprintf("\nTotal unique terms found: %d\n\n", nrow(term_stubs)))

if (nrow(term_stubs) == 0) {
  stop(
    "No terms found! The site structure may differ from expected.\n",
    "Check any debug_*.html files to inspect the raw HTML returned.\n",
    "Alternative: try fetching a detail page directly:\n",
    "  GET ", BASE_URL, "/ajax/ajax.searchalloccurance.php?q=1&r=\n"
  )
}

# -- Step 2: Fetch full definition for each term ------------------------------

cat("Step 2: Fetching definitions...\n")

all_terms <- vector("list", nrow(term_stubs))

for (i in seq_len(nrow(term_stubs))) {
  stub <- term_stubs[i, ]
  cat(sprintf("  [%d/%d] %s (id=%s)\n", i, nrow(term_stubs), stub$term, stub$id))

  detail_html <- fetch_term_detail(stub$id)
  if (!is.null(detail_html)) {
    detail <- parse_term_detail(detail_html)
    all_terms[[i]] <- c(as.list(stub), detail)
  } else {
    all_terms[[i]] <- c(as.list(stub),
                        list(definition = NA, reports = NA, raw_html = NA))
  }

  Sys.sleep(0.3)
}

# -- Step 3: Combine into a data frame ----------------------------------------

glossary_df <- bind_rows(lapply(all_terms, function(x) {
  data.frame(
    id         = x$id,
    prefix     = x$prefix,
    term       = x$term,
    definition = x$definition,
    reports    = x$reports,
    stringsAsFactors = FALSE
  )
}))

# -- Step 4: Save outputs -----------------------------------------------------

# JSON (includes raw_html for full fidelity)
json_list <- lapply(all_terms, function(x) {
  x["raw_html"] <- NULL  # omit raw HTML from JSON to keep file size down
  x
})
write(toJSON(json_list, pretty = TRUE, auto_unbox = TRUE), "ipcc_glossary.json")
cat(sprintf("\nSaved %d terms to ipcc_glossary.json\n", length(json_list)))

# CSV
write.csv(glossary_df, "ipcc_glossary.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("Saved CSV to ipcc_glossary.csv\n"))

cat("\nDone!\n")
