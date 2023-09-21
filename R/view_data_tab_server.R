view_data_tab_server <- function(id, reactive_data, reactive_freq) {
  require(dplyr)
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {
      ns <- shiny::NS(id)

      output$outputs <- NULL

      shiny::observe({
        output$warning <- NULL
        output$info <- NULL
        tryCatch({
          withCallingHandlers(
            warning = function(cond) {
            output$warning <- warning_alert(conditionMessage(cond)) # nolint object_usage
          },
          message = function(cond) {
            output$message <- message_alert(conditionMessage(cond)) # nolint object_usage
          },
          {
            # Check if Data is loaded
            if (!is.null(reactive_data()$data())) {
              if (!is.null(reactive_data()$default()) &
                  as.logical(reactive_data()$default())) {
                if (!is.null(reactive_data()$data_type())
                  & reactive_data()$data_type() == "binary") {
                    output$static_content <-
                      shiny::renderUI(
                        shiny::includeMarkdown("md/binary_citation.md")
                      )
                  } else if (
                    !is.null(reactive_data()$data_type()) &
                      reactive_data()$data_type() == "continuous"
                  ) {
                    output$static_content <-
                      shiny::renderUI(
                        shiny::includeMarkdown("md/continuous_citation.md")
                      )
                  }
                } else {
                  output$static_content <- NULL
                }
                print("Rendering Data")
                print(reactive_data())
                output$outputs <- shiny::renderUI(shiny::tagList(
                  DT::renderDataTable(
                    reactive_data()$data(),
                    filter = "top",
                    options = list(
                      scrollX = TRUE,
                      pageLength = 10,
                      info = FALSE,
                      lengthMenu = list(c(10, -1), c("10", "All"))
                    )
                  )
                ))
              } else {
                print(reactive_data()$print())
                output$static_content <- NULL
                output$outputs <- default_no_data(ns) # nolint object_usage
              }
            }
          )
        },
        error = function(e) {
          output$static_content <- NULL
          output$outputs <- NULL
          error_alert(e$message) # nolint object_usage
        })
      }) %>% shiny::bindEvent(reactive_data())

      shiny::observe({
        load_default_data(reactive_data, reactive_freq) # nolint object_usage
      }) %>% shiny::bindEvent(input$default_data)

    }
  )
}
