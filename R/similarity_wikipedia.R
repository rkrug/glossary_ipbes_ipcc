# Wikipedia-grounded similarity computation
# =============================================================================

#' Fetch the Wikipedia plain-text summary for a term
#'
#' Uses the Wikipedia REST API:
#' `https://en.wikipedia.org/api/rest_v1/page/summary/{term}`
#'
#' @param term  Character scalar — the glossary term to look up.
#' @param lang  Wikipedia language code (default `"en"`).
#' @param timeout_s Request timeout in seconds (default `10`).
#' @return Plain-text extract string, or `NA_character_` if the article is not
#'   found or the request fails.
#' @keywords internal
fetch_wikipedia_summary <- function(term, lang = "en", timeout_s = 10) {
  if (is.na(term) || !nzchar(trimws(term))) return(NA_character_)

  encoded <- utils::URLencode(term, reserved = TRUE)
  url     <- sprintf("https://%s.wikipedia.org/api/rest_v1/page/summary/%s",
                     lang, encoded)

  resp <- tryCatch(
    httr::GET(
      url,
      httr::add_headers(`User-Agent` = paste0(
        "glossary.ipbes.ipcc/",
        utils::packageVersion("glossary.ipbes.ipcc"),
        " (https://github.com/ipbes/glossary_ipbes_ipcc)"
      )),
      httr::timeout(timeout_s)
    ),
    error = function(e) NULL
  )

  if (is.null(resp)) return(NA_character_)
  if (httr::status_code(resp) == 404) {
    # Retry with stripped qualifier
    term2   <- strip_qualifier(term)
    if (!identical(term2, term) && nzchar(term2)) {
      return(fetch_wikipedia_summary(term2, lang, timeout_s))
    }
    return(NA_character_)
  }
  if (httr::http_error(resp)) return(NA_character_)

  parsed <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                       simplifyVector = TRUE),
    error = function(e) NULL
  )

  if (is.null(parsed)) return(NA_character_)
  extract <- parsed[["extract"]]
  if (is.null(extract) || !nzchar(trimws(extract))) return(NA_character_)
  extract
}

# =============================================================================

#' Compute Wikipedia-grounded similarity scores for all matched terms
#'
#' For each row in `merged_data` that has both an IPBES and an IPCC definition:
#' 1. Fetches the Wikipedia summary for the term.
#' 2. Computes cosine similarity between each definition and the Wikipedia text.
#' 3. Stores `(sim_ipbes_wiki + sim_ipcc_wiki) / 2` as the score.
#' 4. Falls back to direct IPBES↔IPCC cosine if Wikipedia is unavailable.
#'
#' Scores are saved to `cache_dir/similarity_cache.rds` incrementally so that
#' an interrupted run can be resumed.
#'
#' @param merged_data Output of [merge_glossaries()] (or with partial scores).
#' @param cache_dir   Directory for caching scores.
#' @param progress_callback Optional `function(current, total, term)` called
#'   after each term.
#' @return `merged_data` with `text_similarity` column populated.
#' @export
compute_wiki_similarity <- function(
    merged_data,
    cache_dir,
    progress_callback = NULL
) {
  # Only score rows where both definitions exist and are non-empty
  has_ipbes <- !is.na(merged_data$ipbes_summary_def) &
    nzchar(trimws(merged_data$ipbes_summary_def))
  has_ipcc  <- !is.na(merged_data$ipcc_summary_def) &
    nzchar(trimws(merged_data$ipcc_summary_def))
  eligible  <- has_ipbes & has_ipcc
  to_score  <- which(eligible)

  # Ensure one-sided rows do not show stale similarity scores
  merged_data$text_similarity[!eligible] <- NA_real_

  total <- length(to_score)
  if (total == 0) return(merged_data)

  # Load existing cache
  scores <- load_similarity_cache(cache_dir)

  for (idx in seq_along(to_score)) {
    i    <- to_score[idx]
    term <- merged_data$matched_term[i]

    # Skip if already cached
    if (!is.null(scores) && term %in% names(scores)) {
      merged_data$text_similarity[i] <- scores[[term]]
      if (!is.null(progress_callback)) {
        tryCatch(progress_callback(idx, total, term), error = function(e) NULL)
      }
      next
    }

    ipbes_def <- merged_data$ipbes_summary_def[i]
    ipcc_def  <- merged_data$ipcc_summary_def[i]

    wiki_text <- fetch_wikipedia_summary(term)

    score <- if (!is.na(wiki_text) && nzchar(wiki_text)) {
      # Average proximity of both definitions to the Wikipedia reference
      sim_ipbes <- compute_text_similarity(ipbes_def, wiki_text)
      sim_ipcc  <- compute_text_similarity(ipcc_def, wiki_text)
      out <- mean(c(sim_ipbes, sim_ipcc), na.rm = TRUE)
      if (is.nan(out)) NA_real_ else out
    } else {
      # Fallback: direct cosine between IPBES and IPCC
      compute_text_similarity(ipbes_def, ipcc_def)
    }

    merged_data$text_similarity[i] <- score

    # Persist incrementally
    scores[term] <- score
    save_similarity_cache(scores, cache_dir)

    if (!is.null(progress_callback)) {
      tryCatch(progress_callback(idx, total, term), error = function(e) NULL)
    }

    Sys.sleep(0.1)   # be polite to Wikipedia
  }

  merged_data
}
