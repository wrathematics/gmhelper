#' gmhelper
#' 
#' @importFrom graphics title
#' @importFrom stats rchisq rnorm runif
#' @importFrom utils tail
#' 
#' @name ngram-package
#' @docType package
#' @author Drew Schmidt \email{wrathematics AT gmail.com}
#' @keywords Package
NULL



#' gmhelper
#' 
#' Run the gmhelper webapp.
#' 
#' The app is written in shiny and uses a bunch of internal rollers
#' and random tables.
#'
#' @importFrom shiny runApp
#' @export
gmhelper <- function()
{
  shiny::runApp(file.path(system.file("shinyapp", package="gmhelper")))
}
