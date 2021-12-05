#' Launch Shiny App For Package MyoManager
#'
#' A function that launches the shiny app for this package.
#' The shiny app allows users to perform microscopy image
#' analysis of muscle stem cells and other similar cell types.
#' The actual script has been placed in \code{./inst/shiny-scripts}.
#'
#' @return No return value but open up a shiny page.
#'
#' @examples
#' \dontrun{
#' runMyoManager()
#' }
#'
#' @author Yinni Kuang, \email{kareen.kuang@mail.utoronto.ca}
#'
#' @references
#' Grolemund, G. (2015). Learn Shiny - Video Tutorials. \href{https://shiny.rstudio.com/tutorial/}{Link}
#'
#'
#' @export
#' @importFrom shiny runApp
runMyoManager <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "MyoManager")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}
# [END]
