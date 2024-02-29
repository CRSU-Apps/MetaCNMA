# This file contains functions to produce alerts.

#' @title Error Alert
#' @description Display an error alert
#' If called from outside an RShiny session the
#' error will be printed to the console
#' @param msg Error Message
#' @param title Alert Title, Default: 'An Error Occurred'
#' @return NULL
#' @details This is a wrapper function of custom_alert
#' @examples
#' \dontrun{
#' if(interactive()){
#'  error_alert("Data Not Found")
#'  }
#' }
#' @rdname error_alert
error_alert <- function(msg, title = "An Error Occurred") {
  custom_alert(title, msg, "error")
}


#' Custom Alert
#'
#' Internal function to display an alert
#' If the alert is called outside of an RShiny Session the alert
#' is printed to the console.
#'
#' @param title Title for alert window
#' @param msg Message for alert window
#' @param type type of alert see `shinyalert::shinyalert` type
#'
#' @return a `shinyalert::shinyalert` if called from
#' a shiny session
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  custom_alert("Error", "Data Not Found", "error")
#'  }
#' }
#' @rdname custom_alert
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
  invisible()
}

#' @title Warning Alert
#' @description Returns a formatted warning message
#' @param msg The warning message to be displayed
#' @return a shiny::div syled as an alert using BS3 classes
#' @details The alert will be a full length box with a background colour of red
#' @examples
#' \dontrun{
#' if(interactive()){
#'  warning_alert("NA's Detected Ignoring")
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{reexports}}
#' @rdname warning_alert
#' @importFrom shiny tags
warning_alert <- function(msg) {
  shiny::tags$div(
    class = "alert alert-warning", role = "alert", paste0("Warning: ", msg)
  )
}

#' @title Message Alert
#' @description Returns a formatted message
#' @param msg The message to be displayed
#' @return a shiny::div syled as an alert using BS3 classes
#' @details The alert will be a full length box
#' with a background colour of yellow
#' @examples
#' \dontrun{
#' if(interactive()){
#'  message_alert("Using TRUE as default parameter")
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{reexports}}
#' @rdname message_alert
#' @importFrom shiny tags
message_alert <- function(msg) {
  shiny::tags$div(
    class = "alert alert-info", role = "alert", paste0("Message: ", msg)
  )
}