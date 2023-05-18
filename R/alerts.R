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