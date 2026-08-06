#' Launch the ONgeoR Shiny app
#'
#' Launches a thin Shiny UI over the package: pick a source and a target
#' layer, and the geometry pair alone decides the link (intersection,
#' nearest, containment, or raster sampling). View the map and table and
#' download `target.gpkg` (the target geometry with the joined attributes
#' merged on), `mapping.csv`, `pairs.csv`, `map.html`, and `reproduce.R`.
#'
#' @param ... Passed to [shiny::runApp()].
#'
#' @return Called for its side effect (launches the Shiny app). Invisibly
#'   returns `NULL`.
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
#' @export
run_app <- function(...) {
  rlang::check_installed(
    c("shiny", "bslib", "DT", "promises", "future"),
    reason = "to run the ONgeoR Shiny app."
  )
  app_dir <- system.file("shiny", package = "ONgeoRapp")
  if (!nzchar(app_dir)) {
    rlang::abort("Could not find the ONgeoR Shiny app directory. Try re-installing the package.")
  }
  shiny::runApp(app_dir, ...)
}
