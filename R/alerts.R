error_alert <- function(msg, title = "An Error Occurred") {
  custom_alert(title, msg, "error")
}


#' Custom Alert
#'
#' Internal function to display an alert
#' if this function is called in from a shiny session
#' `shinyalert::shinyalert()` will be called with the
#' `title` and `message`. Otherwise the error will be
#' printed to the terminal.
#'
#' @param title Title for alert window
#' @param msg Message for alert window
#' @param type type of alert see `shinyalert::shinyalert` type
#'
#' @return a `shinyalert::shinyalert` if called from
#' a shiny session
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

#' Warning Alert
#'
#' Function to return a shiny `div` tag with a formatted
#' `bootstrap` warning message, to be used with `shiny::renderUI`
#'
#' @param msg the warning message as a character vector (String)
#'
#' @return a `shiny::div` tag with the formatted warning message
#' @examples
#' warning_alert("NA values in data")
warning_alert <- function(msg) {
  shiny::tags$div(
    class = "alert alert-warning", role = "alert", paste0("Warning: ", msg)
  )
}

#' Message Alert
#'
#' Function to return a shiny `div` tag with a formatted
#' `bootstrap` info message, to be used with `shiny::renderUI`
#'
#' @param msg the message as a character vector (String)
#'
#' @return a `shiny::div` tag with the formatted message
#' @examples
#' message_alert("NA values in data")
message_alert <- function(msg) {
  shiny::tags$div(
    class = "alert alert-info", role = "alert", paste0("Message: ", msg)
  )
}