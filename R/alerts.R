errorAlert <- function(msg, title = ""){
  customAlert(title, msg, "error")
}

customAlert <- function(title, msg, type){
  tryCatch({
    shinyalert(
      title = title, text = msg, type = type, confirmButtonText = "Close"
    )
  }, 
  error = function(e){
    # Do Nothing
  }, 
  finally = {
    print(paste(title, msg, sep = ": "))
  })
}

warningAlert <- function(msg){
  renderUI(
    tags$div(
      class= "alert alert-warning", role= "alert", paste0(msg)
    )
  )
}

messageAlert <- function(msg){
  renderUI(
    tags$div(
      class= "alert alert-info", role= "alert", paste0(msg)
    )
  )
}