# IPCC glossary data loading and scraping
# =============================================================================

# Resolve active IPCC source path (cache first, then bundled snapshot)
resolve_ipcc_source_path <- function(
    cache_dir    = tools::R_user_dir("glossary.ipbes.ipcc", which = "cache"),
    bundled_path = {
      p <- system.file("extdata", "ipcc_glossary.csv",
                       package = "glossary.ipbes.ipcc")
      if (nzchar(p)) p else file.path(getwd(), "inst", "extdata",
                                      "ipcc_glossary.csv")
    }
) {
  cache_file <- file.path(cache_dir, "ipcc_glossary.csv")

  if (file.exists(cache_file) && file.info(cache_file)$size > 10) {
    return(cache_file)
  }

  if (nzchar(bundled_path) && file.exists(bundled_path) &&
      file.info(bundled_path)$size > 10) {
    return(bundled_path)
  }

  ""
}

#' Load the IPCC glossary
#'
#' Looks for a user-updated CSV in `cache_dir` first; falls back to the bundled
#' snapshot in `inst/extdata/`.
#'
#' @param cache_dir Directory where the user-updated IPCC CSV may be stored.
#'   Defaults to [tools::R_user_dir()] cache.
#' @param bundled_path Path to the bundled IPCC CSV.
#' @return A [dplyr::tibble()] with columns `id`, `term`, `definition`,
#'   `reports` (character, semicolons between report names), `downloaded_at`.
#'   Returns an empty tibble with the correct columns if no data is available.
#' @export
load_ipcc <- function(
    cache_dir    = tools::R_user_dir("glossary.ipbes.ipcc", which = "cache"),
    bundled_path = {
      p <- system.file("extdata", "ipcc_glossary.csv",
                       package = "glossary.ipbes.ipcc")
      if (nzchar(p)) p else file.path(getwd(), "inst", "extdata",
                                      "ipcc_glossary.csv")
    }
) {
  path <- resolve_ipcc_source_path(cache_dir, bundled_path)
  if (!nzchar(path)) {
    return(.empty_ipcc_tibble())
  }

  df <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8"),
    error = function(e) {
      message("Could not read IPCC CSV: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(df) || nrow(df) == 0) return(.empty_ipcc_tibble())

  # Ensure expected columns exist
  for (col in c("id", "term", "definition", "reports", "downloaded_at")) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  df$definition <- clean_html(df$definition)
  tibble::as_tibble(df[, c("id", "term", "definition", "reports",
                            "downloaded_at")])
}

.empty_ipcc_tibble <- function() {
  tibble::tibble(
    id           = character(),
    term         = character(),
    definition   = character(),
    reports      = character(),
    downloaded_at = character()
  )
}

# =============================================================================

#' Summarise IPCC glossary to one row per term
#'
#' @param ipcc_df Output of [load_ipcc()].
#' @return A [dplyr::tibble()] with columns `term`, `n_reports`,
#'   `summary_definition`, `ipcc_data` (list-column of report × definition
#'   tibbles).
#' @export
summarise_ipcc <- function(ipcc_df) {
  if (nrow(ipcc_df) == 0) {
    return(tibble::tibble(
      term               = character(),
      n_reports          = integer(),
      summary_definition = character(),
      ipcc_data          = list()
    ))
  }

  terms <- unique(ipcc_df$term)

  rows <- lapply(terms, function(trm) {
    sub <- ipcc_df[ipcc_df$term == trm, , drop = FALSE]

    # Expand semicolon-separated reports into per-report rows
    detail_rows <- do.call(rbind, lapply(seq_len(nrow(sub)), function(i) {
      rpts <- trimws(strsplit(sub$reports[i], ";\\s*")[[1]])
      rpts <- rpts[nzchar(rpts)]
      if (length(rpts) == 0) rpts <- NA_character_
      data.frame(report     = rpts,
                 definition = sub$definition[i],
                 stringsAsFactors = FALSE)
    }))

    detail <- unique(tibble::as_tibble(detail_rows))

    # Summary: prefer shortest non-trivial
    defs <- sub$definition[!is.na(sub$definition) & nchar(sub$definition) >= 20]
    if (length(defs) == 0) defs <- sub$definition[!is.na(sub$definition)]
    summary_def <- if (length(defs) > 0) defs[which.min(nchar(defs))] else NA_character_

    n_reports <- length(unique(detail$report[!is.na(detail$report)]))

    list(
      term               = trm,
      n_reports          = n_reports,
      summary_definition = summary_def,
      ipcc_data          = detail
    )
  })

  tibble::tibble(
    term               = vapply(rows, `[[`, character(1), "term"),
    n_reports          = vapply(rows, `[[`, integer(1),   "n_reports"),
    summary_definition = vapply(rows, `[[`, character(1), "summary_definition"),
    ipcc_data          = lapply(rows, `[[`, "ipcc_data")
  )
}

# =============================================================================

#' Scrape the IPCC glossary from the live website
#'
#' Adapted from `data-raw/IPCC/download_ipcc_glossary.R`.  Downloads all terms
#' and definitions from <https://apps.ipcc.ch/glossary/> and saves the result
#' to `cache_dir`.
#'
#' @param cache_dir Directory to write `ipcc_glossary.csv`.
#' @param progress_callback Optional function `function(current, total, term)`
#'   called after each term is fetched.  Use to drive a Shiny progress bar.
#' @return Invisible path to the written CSV.
#' @export
scrape_ipcc <- function(cache_dir, progress_callback = NULL) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  base_url <- "https://apps.ipcc.ch/glossary"

  prefixes <- c("123", "A", "B", "C", "D", "E", "F", "G", "H", "I",
                "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S",
                "T", "U", "V", "W", "Y", "Z")

  session_headers <- httr::add_headers(
    `User-Agent` = paste0("glossary.ipbes.ipcc R package/",
                          utils::packageVersion("glossary.ipbes.ipcc"),
                          " (https://github.com/ipbes/glossary_ipbes_ipcc)"),
    `Referer`    = "https://apps.ipcc.ch/glossary/search.php"
  )

  # ---- Helper: clean term name (strip WG suffixes like « WGI,WGII ») ------
  .clean_term <- function(raw_text) {
    cleaned <- gsub("\\s*\u00ab[^\u00bb]*\u00bb\\s*", "", raw_text)
    cleaned <- gsub("\\s*<<[^>]*>>\\s*", "", cleaned)
    trimws(cleaned)
  }

  # ---- Step 1: collect term stubs ------------------------------------------
  term_stubs <- data.frame(id = character(), term = character(),
                           prefix = character(), stringsAsFactors = FALSE)

  for (prefix in prefixes) {
    # search.php endpoint family (all reports)
    url  <- paste0(base_url, "/ajax/ajax.searchbyindex.php?q=", prefix)
    html <- tryCatch({
      resp <- httr::GET(url, session_headers, httr::timeout(15))
      httr::stop_for_status(resp)
      httr::content(resp, as = "text", encoding = "UTF-8")
    }, error = function(e) {
      message("  Error fetching '", prefix, "': ", conditionMessage(e)); NULL
    })

    if (!is.null(html)) {
      page  <- rvest::read_html(html)
      # IPCC uses data-phraseid on span.alllink elements
      nodes <- rvest::html_elements(page, "span.alllink[data-phraseid]")
      if (length(nodes) > 0) {
        ids   <- rvest::html_attr(nodes, "data-phraseid")
        terms <- vapply(rvest::html_text(nodes, trim = TRUE),
                        .clean_term, character(1), USE.NAMES = FALSE)
        valid <- !is.na(ids) & nchar(ids) > 0
        term_stubs <- rbind(term_stubs,
                            data.frame(id = ids[valid], term = terms[valid],
                                       prefix = prefix, stringsAsFactors = FALSE))
      }
    }
    Sys.sleep(0.3)
  }

  term_stubs <- term_stubs[!duplicated(term_stubs$id), ]
  total <- nrow(term_stubs)

  if (total == 0) {
    stop("No IPCC terms found. The site structure may have changed.")
  }

  # ---- Step 2: fetch full definitions ---------------------------------------
  # The detail endpoint for all reports (search.php family)
  all_rows <- vector("list", total)

  for (i in seq_len(total)) {
    stub <- term_stubs[i, ]
    url  <- paste0(base_url,
                   "/ajax/ajax.searchalloccurance.php?q=",
                   stub$id, "&r=")

    detail_html <- tryCatch({
      resp <- httr::GET(url, session_headers, httr::timeout(15))
      httr::stop_for_status(resp)
      httr::content(resp, as = "text", encoding = "UTF-8")
    }, error = function(e) { NULL })

    definition <- NA_character_
    reports    <- NA_character_

    if (!is.null(detail_html) && nchar(trimws(detail_html)) > 10) {
      page <- rvest::read_html(detail_html)

      # Definitions are in <p> tags within <dd> elements
      def_nodes <- rvest::html_elements(page, "dd p")
      if (length(def_nodes) > 0) {
        definition <- rvest::html_text(def_nodes[[1]], trim = TRUE)
      } else {
        dd_nodes <- rvest::html_elements(page, "dd")
        if (length(dd_nodes) > 0) {
          definition <- rvest::html_text(dd_nodes[[1]], trim = TRUE)
        }
      }

      report_nodes <- rvest::html_elements(page, "[data-report]")
      rpts <- unique(rvest::html_attr(report_nodes, "data-report"))
      rpts <- rpts[!is.na(rpts) & nchar(rpts) > 0]
      reports <- paste(rpts, collapse = "; ")
    }

    all_rows[[i]] <- data.frame(
      id           = stub$id,
      prefix       = stub$prefix,
      term         = stub$term,
      definition   = definition,
      reports      = reports,
      downloaded_at = format(Sys.Date()),
      stringsAsFactors = FALSE
    )

    if (!is.null(progress_callback)) {
      tryCatch(progress_callback(i, total, stub$term), error = function(e) NULL)
    }

    Sys.sleep(0.3)
  }

  # ---- Step 3: save ---------------------------------------------------------
  glossary_df <- do.call(rbind, all_rows)
  out_path    <- file.path(cache_dir, "ipcc_glossary.csv")
  utils::write.csv(glossary_df, out_path, row.names = FALSE, fileEncoding = "UTF-8")

  invisible(out_path)
}
