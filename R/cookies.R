cookie_alert <- function() {
  shinyalert::shinyalert(
    text = get_cookie_message(),
    type = "info",
    html = TRUE,
    confirmButtonText = "Accept",
    inputId = "cookieAccept"
  )
}

cookie_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$head(
    shiny::uiOutput(outputId = ns("analytics_script"))
  )
}

cookie_server <- function(
  id,
  cookies,
  google_analytics_id,
  tab
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      `%>%` <- magrittr::`%>%`

      parent_session <- shiny::getDefaultReactiveDomain()$rootScope()

      is_analytics <- shiny::reactiveVal(FALSE)

      add_analytics <- function() {
        shiny::renderUI({
          return(
            shiny::tags$head(
              shiny::singleton(
                tags$script(
                  stringr::str_replace_all(
                    readr::read_file("google_analytics.js"),
                    "<<GOOGLE_ANALYTICS_ID>>",
                    google_analytics_id
                  )
                )
              )
            )
          )
        })
      }

      shiny::observe({
        print("Cookie Message")
        print(cookies()$accept)
        if (is.null(cookies()$accept)) {
          cookie_alert()
        } else if (as.logical(cookies()$accept)) {
          print("accepted")
          output$analytics_script <- add_analytics()
          is_analytics(TRUE)
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
        output$analytics_script <- add_analytics()
        is_analytics(TRUE)
      }) %>% shiny::bindEvent(input$cookieAccept)

      shiny::observe({
        shiny::req(
          is_analytics(),
          cancelOutput = TRUE
        )
        session$sendCustomMessage(
          "add-event",
          list(
            value = tab()
          )
        )
      }) %>% shiny::bindEvent(
        tab()
      )

    }
  )
}