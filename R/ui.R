# Shiny UI definition
# =============================================================================

#' Build the Shiny application UI
#'
#' @param enable_live_update Logical; if `FALSE`, the IPCC live update control
#'   is replaced with a hosted-safe informational label.
#' @return A [shiny::fluidPage()] UI object.
#' @keywords internal
build_ui <- function(enable_live_update = TRUE) {
  update_ui <- if (isTRUE(enable_live_update)) {
    mod_update_ipcc_ui("update_ipcc")
  } else {
    htmltools::span(
      class = "update-disabled",
      "Hosted mode: IPCC live update disabled"
    )
  }

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$link(
        rel  = "stylesheet",
        href = "custom/custom.css"
      )
    ),

    shiny::titlePanel(
      title = NULL,
      windowTitle = "IPBES \u2194 IPCC Glossary Comparison"
    ),

    htmltools::div(
      htmltools::h2(
        "IPBES \u2194 IPCC Glossary Comparison",
        style = "margin-bottom:0;"
      ),
      htmltools::p(
        style = "color:#666; font-size:1rem; margin-top:4px;",
        "Side-by-side comparison of IPBES and IPCC glossary definitions."
      )
    ),

    # ---- Toolbar: sort controls + update button ----------------------------
    shiny::fluidRow(
      shiny::column(
        width = 12,
        htmltools::div(
          class = "glossary-toolbar",

          # Sort buttons
          shiny::actionButton(
            "sort_alpha",
            label = "Sort Alphabetically",
            icon  = shiny::icon("sort-alpha-down"),
            class = "btn btn-default active"
          ),

          # Separator
          htmltools::span(style = "margin: 0 0.5rem; color: #ddd;", "|"),

          # IPCC update module UI
          update_ui,

          # Right-aligned status message
          htmltools::div(
            class = "update-status",
            shiny::textOutput("similarity_status", inline = TRUE)
          )
        )
      )
    ),

    shiny::hr(style = "margin:0.5rem 0 1rem 0;"),

    # ---- Main table --------------------------------------------------------
    shiny::fluidRow(
      shiny::column(
        width = 12,
        mod_table_ui("main_table")
      )
    ),

    # ---- Footer ------------------------------------------------------------
    shiny::hr(),
    htmltools::div(
      style = "color:#888; font-size:0.9rem; padding-bottom:1rem;",
      "Data: ",
      htmltools::a("IPBES Glossary", href = "https://www.ipbes.net/glossary",
                   target = "_blank"),
      " | ",
      htmltools::a("IPCC Glossary", href = "https://apps.ipcc.ch/glossary/search.php",
                   target = "_blank"),
      " | Package: ",
      htmltools::a("glossary.ipbes.ipcc",
                   href = "https://github.com/ipbes/glossary_ipbes_ipcc",
                   target = "_blank")
    )
  )
}
