# Shiny server definition
# =============================================================================

#' Build the Shiny application server function
#'
#' @param cache_dir   Path to the cache directory.
#' @param initial_data A pre-loaded merged glossary [tibble::tibble()] from
#'   [merge_glossaries()].
#' @return A Shiny `server` function.
#' @keywords internal
build_server <- function(cache_dir, initial_data) {
  function(input, output, session) {

    # -- Shared reactive state -----------------------------------------------
    merged_rv  <- shiny::reactiveVal(initial_data)

    output$similarity_status <- shiny::renderText({
      data <- merged_rv()
      if (is.null(data) || nrow(data) == 0) return("")
      n_available <- sum(!is.na(data$sim_between_all))
      paste0("Between similarity available for ", n_available, " terms")
    })

    # -- Sort button observers -----------------------------------------------
    shiny::observeEvent(input$sort_alpha, {
      shiny::updateActionButton(session, "sort_alpha",
                                class = "btn btn-default active")
    })

    # -- IPCC update module --------------------------------------------------
    mod_update_ipcc_server("update_ipcc", cache_dir, merged_rv)

    # -- Table module --------------------------------------------------------
    mod_table_server(
      id          = "main_table",
      merged_rv   = merged_rv,
      cache_dir   = cache_dir
    )
  }
}
