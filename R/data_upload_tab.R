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
          shiny::uiOutput(ns("fileInput")),
          offset = 2
        ),
        shiny::column(
          width = 6,
          shiny::uiOutput(ns("reloadButton"))
        ),
        class = "vertical-align"
      ))
      output$fileInput <- default_file_input(ns) # nolint: object_usage

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
                reactive_data()$invalidate()
                reactive_freq()$invalidate()
                # Validate the input file
                if (validate_input(input$data, data$type)) { # nolint: object_usage 
                  # Render the reload button
                  output$reloadButton <- default_reload_button(ns) # nolint: object_usage
                  # Temporarily load the date
                  tmp_data <- rio::import(input$data$datapath)
                  data$format <-
                    dplyr::if_else(suppressMessages(is_wide(tmp_data)), "wide", "long") #nolint
                  data$default <- F
                  freq$valid <- F
                  data$data <- tmp_data
                  data$valid <- T
                  print("Data valid and loaded")
                } else {
                  invalidate_data(data, freq)
                }
              }
            )
          },
          error = function(e) {
            error_alert(e$message) # nolint: object_usage
            invalidate_data(data, freq)
          }
        )
      }) %>% shiny::bindEvent(input$data)
    }
  )
}