# Helper function for conditional messaging (depending if inside or outside of a shiny session)
.notifyUser <- function(messageText, duration = NULL, type = "message", id = NULL, session = NULL) {
  if (!is.null(session)) {
    shiny::showNotification(messageText, duration = duration, type = type, id = id, session = session)
  } else {
    # Simple console message, ignoring duration, type, id
    message(paste0("[", Sys.time(), "] ", messageText))
  }
}

# Helper function for conditional notification removal
.removeUserNotify <- function(id, session = NULL) {
  if (!is.null(session) && !is.null(id)) {
    shiny::removeNotification(id = id, session = session)
  }
  # No equivalent action needed for console messages
}