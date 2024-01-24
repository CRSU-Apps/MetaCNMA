view_data_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("View Data"),
    message_tag_list(ns), # nolint: object_usage
    shiny::uiOutput(ns("citation")),
    DT::DTOutput(ns("data_table"))
  )
}

view_data_tab_server <- function(id, data_reactives) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      shiny::observe({
        shiny::req(data_reactives$data_type())
        if (is.null(data_reactives$data())) {
          output$citation <- NULL
          output$data_table <- NULL
        } else {
          tryCatch({
            withCallingHandlers(
              warning = function(cond) {
                output$warning <- shiny::renderUI(
                  warning_alert(conditionMessage(cond)) # nolint: object_name
                )
              },
              message = function(cond) {
                output$info <- shiny::renderUI(
                  message_alert(conditionMessage(cond)) # nolint: object_name
                )
              },
              {
                output$data_table <-
                  DT::renderDataTable(
                    data_reactives$data(),
                    filter = "top",
                    options = list(
                      scrollX = TRUE,
                      pageLength = 10,
                      info = FALSE,
                      lengthMenu = list(c(10, -1), c("10", "All"))
                    )
                  )
              }
            )
          }, error = function(e) {
            output$citation <- NULL
            output$data_table <- NULL
            data_reactives$invalidat_count(data_reactives$invalidate_count + 1)
            error_alert(e$message) # nolint object_usage
          })
        }
      }) %>% shiny::bindEvent(
        data_reactives$data(),
        ignoreInit = TRUE
      )
    }
  )
}
