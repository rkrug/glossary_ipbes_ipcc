\name{run_app}
\alias{run_app}
\title{Run the IPBES/IPCC Glossary Comparison Shiny App}
\usage{
run_app(
  cache_dir = tools::R_user_dir("glossary.ipbes.ipcc", which = "cache"),
  ...
)
}
\arguments{
\item{cache_dir}{Directory used to store the user-updated IPCC glossary CSV
and pre-computed Wikipedia similarity scores.  Defaults to a
package-specific subdirectory of the OS user cache directory as returned
by \code{\link[tools]{R_user_dir}}.  Created automatically if it does not
exist.}

\item{...}{Additional arguments passed to \code{\link[shiny]{runApp}}
(e.g. \code{launch.browser}, \code{port}).}
}
\value{
Invisibly, the \code{\link[shiny:shinyApp]{shinyApp}} object.
}
\description{
Launches the interactive Shiny application that displays IPBES and IPCC
glossary definitions side-by-side with similarity scores and word-level
diffs.
}
\examples{
\dontrun{
glossary.ipbes.ipcc::run_app()
}
}
