freq_exclude_tab_server <- function(id, reactive_data, reactive_freq) {
  shiny::moduleServer(id,
    function(input,
             output,
             session) {
      ns <- shiny::NS(id)

      require(dplyr)

      shiny::observe({
        output$warning <- NULL
        output$info <- NULL
        tryCatch({
          withCallingHandlers(
            warning = function(cond) {
              output$warning <- warning_alert(cond) # nolint: object_usage
            },
            message = function(cond) {
              output$message <- message_alert(cond) # nolint: object_usage
            },
            {
              if (! reactive_data()$valid()) {
                output$inputs <- default_no_data(ns) # nolint: object_usage
              } else {
                print("checking formatted data")
                if (is.null(reactive_freq()$formatted_data())) {
                  shiny::withProgress({
                    format_data(reactive_data, reactive_freq) # nolint: object_usage
                  },
                  message = "Formatting Data")
                }
                studies <- reactive_freq()$studies()
                if (is.null(studies)) {
                  stop(paste0("Error ST001: There is an issue with the 
                  Names of the studies, please check them and try again"))
                }
                output$inputs <- shiny::renderUI(
                  shiny::tagList(
                    shiny::fluidRow(
                      shiny::column(
                        3,
                        shiny::h3("Study Selection"),
                        shiny::checkboxGroupInput("exclude",
                          label = "Choose any Studies you wish to exclude",
                          choices = studies
                        )
                      ),
                      shiny::column(
                        9,
                        h3("Data"),
                        DT::renderDataTable(
                          reactive_freq()$formatted_data(),
                          filter = "top",
                          options = list(
                            scrollX = TRUE,
                            pageLength = 10,
                            info = FALSE,
                            lengthMenu = list(c(10, -1), c("10", "All"))
                          )
                        )
                      )
                    )
                  )
                )
              }
            }
          )
        },
        error = function(e) {
          print("this error occured trying to render the studies")
          error_alert(e$message) # nolint: object_name
          invalidate_reactive(reactive_data, reactive_freq) # nolint: object_name
        })
      }) %>% shiny::bindEvent(reactive_data()$valid())

      shiny::observe({
        load_default_data(reactive_data, reactive_freq) # nolint: object_name
      }) %>% shiny::bindEvent(input$default_data)

      shiny::observe({
        #data$exclude <- input$exclude
      }) %>% shiny::bindEvent(input$exclude)

    }
  )
}