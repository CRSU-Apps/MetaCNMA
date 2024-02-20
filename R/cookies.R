cookie_alert <- function() {
  shinyalert::shinyalert(
    text = get_cookie_message(),
    type = "info",
    html = TRUE,
    confirmButtonText = "Accept",
    inputId = "cookieAccept"
  )
}

cookie_server <- function(
  id,
  cookies,
  open_privacy_policy
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      `%>%` <- magrittr::`%>%`

      parent_session <- shiny::getDefaultReactiveDomain()$rootScope()

      shiny::observe({
        print("Cookie Message")
        print(cookies()$accept)
        if(is.null(cookies()$accept)) {
          cookie_alert()
        } else if (as.logical(cookies()$accept)) {
          print("accepted")
        } else {
          cookie_alert()
        }
      }) %>% shiny::bindEvent(cookies(), once = TRUE)

      shiny::observe({
        print(input$cookieAccept)
        msg <- list(
          name = "accept",
          value = input$cookieAccept
        )
        if (
          shiny::isolate(
            parent_session$clientData$url_protocol
          ) == "https:"
        ) {
          print("Running in HTTPS")
          session$sendCustomMessage("cookie-set-secure", msg)
        } else {
          print("Running in HTTP")
          session$sendCustomMessage("cookie-set", msg)
        }

        print(cookies())
      }) %>% shiny::bindEvent(input$cookieAccept)

      shiny::observe({
        print("Open Privacy Policy")
        shinydashboard::updateTabItems(
          parent_session,
          "tabs",
          "privacy_policy"
        )
        shinyalert::closeAlert()
        shinyjs::delay(60000, cookie_alert())
      }) %>% shiny::bindEvent(open_privacy_policy())
    }
  )
}