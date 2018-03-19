#' #' Adds the content of www to sF/
#' #' 
#' #' @importFrom shiny addResourcePath
#' #' 
#' #' @noRd
#' #' 
#' .onLoad <- function(...) {
#'   addResourcePath('sF', system.file('www', package='shinyFiles'))
#' }

#' Run a simple example app using the eda4plant functionality
#' 
#' 
#' @importFrom shiny runApp
#' 
#' @export
#' 
# shinyed4plant <- function() {
#   runApp(system.file('inst/app', package='eda4plant', mustWork=T), display.mode='showcase')
# }

shinyeda <- function() {
  appDir <- system.file("app", package = "eda4plant")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `eda4plant`.",
         call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}









