error_alert <- function(msg, title = "") {
  custom_alert(title, msg, "error")
}

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