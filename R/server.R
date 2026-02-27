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
    sort_mode  <- shiny::reactiveVal("alpha")

    output$similarity_status <- shiny::renderText({
      data <- merged_rv()
      if (is.null(data) || nrow(data) == 0) return("")
      metric <- input$similarity_metric
      if (is.null(metric) || !metric %in% c("sim_between_all", "sim_within_ipbes",
                                            "sim_within_ipcc")) {
        metric <- "sim_between_all"
      }
      metric_label <- switch(metric,
                             sim_within_ipbes = "Within IPBES",
                             sim_within_ipcc  = "Within IPCC",
                             "Between All Definitions")
      n_available <- sum(!is.na(data[[metric]]))
      paste0(metric_label, " available for ", n_available, " terms")
    })

    # -- Sort button observers -----------------------------------------------
    shiny::observeEvent(input$sort_alpha, {
      sort_mode("alpha")
      shiny::updateActionButton(session, "sort_alpha",
                                class = "btn btn-default active")
      shiny::updateActionButton(session, "sort_similarity",
                                class = "btn btn-default")
    })

    shiny::observeEvent(input$sort_similarity, {
      sort_mode("similarity")
      shiny::updateActionButton(session, "sort_alpha",
                                class = "btn btn-default")
      shiny::updateActionButton(session, "sort_similarity",
                                class = "btn btn-default active")
    })

    # -- IPCC update module --------------------------------------------------
    mod_update_ipcc_server("update_ipcc", cache_dir, merged_rv)

    # -- Table module --------------------------------------------------------
    mod_table_server(
      id          = "main_table",
      merged_rv   = merged_rv,
      sort_mode   = sort_mode,
      sort_metric = shiny::reactive({
        metric <- input$similarity_metric
        if (is.null(metric) || !metric %in% c("sim_between_all",
                                              "sim_within_ipbes",
                                              "sim_within_ipcc")) {
          "sim_between_all"
        } else {
          metric
        }
      }),
      cache_dir   = cache_dir
    )
  }
}
