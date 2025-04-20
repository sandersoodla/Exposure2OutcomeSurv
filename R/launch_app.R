#' Launch the Exposure2OutcomeSurv Shiny Application
#'
#' @description
#' This function runs the main Shiny application interface for exploring
#' exposure-outcome survival relationships using matched cohort data from an OMOP CDM.
#'
#' @details
#' The function calls `shiny::shinyApp`, using the `app_ui` and `app_server`
#' functions defined within the 'Exposure2OutcomeSurv' package to create the application.
#'
#' @param launch.browser Logical; if TRUE, the app will open in the system's
#'   default web browser. Defaults to TRUE. Passed via the `options` argument
#'   to `shiny::shinyApp`.
#' @param ... Additional arguments passed as options to `shiny::shinyApp`
#'   (e.g., `port`, `host`). See `?shiny::shinyApp` for details on available options.
#'
#' @return Invisibly returns the result of `shiny::shinyApp` (launches the app)
#' @export
#'
#' @import shiny
#'
#' @examples
#' \dontrun{
#'   # To run the application
#'   launchApp()
#'
#'   # To run on a specific port and host
#'   # launchApp(port = 1234, host = "0.0.0.0")
#' }
launchApp <- function(launch.browser = TRUE, ...) {
  
  # Combine launch.browser and any other arguments passed via ...
  # into the options list for shinyApp
  appOptions <- list(
    launch.browser = launch.browser,
    ...
  )
  
  # --- Launch App ---
  shiny::shinyApp(
    ui = app_ui(),
    server = app_server,
    options = appOptions
  )
}