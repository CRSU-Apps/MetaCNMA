require(dplyr)
data_upload_tab_server <- function(id, reactive_data, reactive_freq) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {
      ns <- shiny::NS(id)

      output$static_content <- shiny::renderUI(shiny::tagList(
        shiny::p("Data should be uploaded as either a .csv or .xlsx file"),
        shiny::p(
          "For help on uploading data see the ",
          tab_link("dataHelp", "data help page") # nolint: object_usage
        )
      ))
      output$inputs <- shiny::renderUI(shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::uiOutput(ns("file_input")),
          offset = 2
        ),
        shiny::column(
          width = 6,
          shiny::uiOutput(ns("reload_button"))
        ),
        class = "vertical-align"
      ))
      output$file_input <- default_file_input(ns) # nolint: object_usage

      shiny::observe({
        # Reset any previous warnings / messages
        output$warning <- NULL
        output$info <- NULL
        tryCatch(
          {
            # Use withCallingHandlers to catch warnings after execution
            withCallingHandlers(
              warning = function(cond) {
                # If a warning occurs display the warning formatting it using
                # the warning_alert function in R/alerts.r
                output$warning <- warning_alert(cond) # nolint: object_usage
              },
              message = function(cond) {
                output$message <- message_alert(cond) # nolint: object_usage
              },
              {
                # Validate the input file
                if (validate_input(input$data, reactive_data()$data_type())) { # nolint: object_usage 
                  invalidate_reactive(reactive_data, reactive_freq)
                  # Render the reload button
                  output$reload_button <- default_reload_button(ns) # nolint: object_usage
                  # Temporarily load the date
                  .data <- rio::import(input$data$datapath)
                  .format <-
                    dplyr::if_else(suppressMessages(is_wide(.data)), "wide", "long") #nolint
                  reactive_data()$load_data(.format, .data)
                  print("Data valid and loaded")
                } else {
                  invalidate_reactive(reactive_data, reactive_freq) # nolint: object_usage
                }
              }
            )
          },
          error = function(e) {
            error_alert(e$message) # nolint: object_usage
            invalidate_reactive(reactive_data, reactive_freq) # nolint: object_usage
          }
        )
      }) %>% shiny::bindEvent(input$data)

      shiny::observe({
        print(reactive_data()$print())
        print(reactive_data()$valid())
        if (reactive_data()$valid() == FALSE) {
          print("Resetting File Input")
          output$file_input <- default_file_input(ns) # nolint: object_usage
          output$reload_button <- NULL
        }
      }) %>% shiny::bindEvent(reactive_data()$valid(),
        ignoreInit = TRUE, ignoreNULL = TRUE
      )

      shiny::observe({
        invalidate_reactive(reactive_data, reactive_freq) # nolint: object_usage
      }) %>% shiny::bindEvent(input$reload_button,
                              ignoreNULL = TRUE,
                              ignoreInit = TRUE)

    }
  )
}