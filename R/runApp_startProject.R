#' Launch startProject Shiny App
#'
#' @description
#'     `runApp_startProject` will open a viewer window that will allow you
#'     to run \code{\link{startProject}} as a local shiny App.
#'
#' @param browser A character string that specifies the browser in which the app
#'     is launched. Valid options are "PaneViewer" to run in RStudio Viewer tab,
#'     "WindowViewer" to run in RStudio Window or "WindowExternal" to run in
#'     default web browser (case is ignored).
#'     Defaults to "WindowViewer".
#'
#' @details
#' The browser argument only takes effect if running within RStudio.
#'
#' @seealso \code{\link{startProject}} for details on inputs.
#'
#' @examples
#' \dontrun{runApp_startProject(browser = "PaneViewer")}
#'
#' @author Rocio Lopez, \email{lopezr@@ccf.org}
#'
#' @export
runApp_startProject <- function(browser = "WindowViewer")
{
  ## Make sure that {rstudioapi} is available in order to change browser option
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("viewer"))
    {

    ## Define browser to use
    if (tolower(browser) == "windowexternal") {
      useBrowser <- .rs.invokeShinyWindowExternal
    }
    else if (tolower(browser) == "windowviewer") {
      useBrowser <- .rs.invokeShinyWindowViewer
    }
    else {
      useBrowser <- .rs.invokeShinyPaneViewer
    }

    options(shiny.launch.browser = useBrowser)

  }

  ## Launch app
  shiny::runApp(appDir = system.file("application", package = "startProject"))
}
