error_alert <- function(msg, title = "An Error Occurred") {
  custom_alert(title, msg, "error")
}


#' Custom Alert
#' 
#' Internal function to display an alert
#' uses if the alert is called outside of an RShiny Session the alert
#' is printed to the console.
#'
#' @param title Title for alert window
#' @param msg Message for alert window
#' @param type type of alert see shiny::alert type
#'
#' @return a shinyalert::shinyalert
#'
#' @examples
custom_alert <- function(title, msg, type) {
  tryCatch({
    shinyalert::shinyalert(
      title = title, text = msg, type = type, confirmButtonText = "Close"
    )
  },
  error = function(e) {
    # Do Nothing
  },
  finally = {
    print(paste(title, msg, sep = ": "))
  })
}

warning_alert <- function(msg) {
  shiny::renderUI(
    shiny::tags$div(
      class = "alert alert-warning", role = "alert", paste0("Warning: ", msg)
    )
  )
}

message_alert <- function(msg) {
  shiny::renderUI(
    shiny::tags$div(
      class = "alert alert-info", role = "alert", paste0("Message: ", msg)
    )
  )
}