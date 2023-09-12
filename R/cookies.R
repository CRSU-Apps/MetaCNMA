cookieAlert <- function(){
  shinyalert(
    text = get_cookie_message(), type = "info", html = T, confirmButtonText = "Accept", inputId = "cookieAccept"
  )
}

cookieUI <- function(id){
  ns <- NS("id")
}

cookieServer <- function(id,
                         cookies,
                         openPrivacyPolicy,
                         parentSession) {
  moduleServer(id, function(input,
                            output,
                            session) {
    observe({
      print("Cookie Message")
      print(cookies()$accept)
      if(is.null(cookies()$accept)){
        cookieAlert()
      }
      else if(as.logical(cookies()$accept)){
        print("accepted")
      }
      else{
        cookieAlert()
      }
    }) %>% bindEvent(cookies(), once = T)
    
    observe({
      print(input$cookieAccept)
      msg <- list(
        name = "accept",
        value = input$cookieAccept
      )
      session$sendCustomMessage("cookie-set", msg)
      print(cookies())
    }) %>% bindEvent(input$cookieAccept)
    
    observe({
      print("Open Privacy Policy")
      updateTabItems(parentSession(), "tabs", "privacyPolicy")
      closeAlert()
      shinyjs::delay(60000, cookieAlert())
    }) %>% bindEvent(openPrivacyPolicy())
  })
}