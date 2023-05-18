cookieAlert <- function(){
  shinyalert(
    text = getCookieMessage(), type = "info", html = T, confirmButtonText = "Accept", inputId = "cookieAccept"
  )
}

cookieUI <- function(id){
  ns <- NS("id")
}

cookieServer <- function(id, globalCookies,
                         globalOpenPrivacyPolicy,
                         globalSession) {
  moduleServer(id, function(input,
                            output,
                            session,
                            cookies = globalCookies,
                            openPrivacyPolicy = globalOpenPrivacyPolicy,
                            parent = globalSession) {
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
      updateTabItems(parent(), "tabs", "privacyPolicy")
      closeAlert()
      shinyjs::delay(60000, cookieAlert())
    }) %>% bindEvent(openPrivacyPolicy())
  })
}