#' Launch the shiny app for VFtree Package
#'
#' A function that launches the shiny app for this package.
#' The code has been placed in \code{./inst/shiny-scripts}.
#'
#' @return No return value but open up a shiny page.
#'
#' @examples
#' \dontrun{
#' runVFtreeApp()
#' }
#'
#' @export
#' @importFrom shiny runApp

runVFtreeApp <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "VFtree")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}

# [END]
