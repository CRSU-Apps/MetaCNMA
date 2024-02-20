data_table_module_ui <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("data_table_output"))
}

data_table_module_server <- function(
  id,
  reactive_df
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      `%>%` <- magrittr::`%>%`
      ns <- session$ns
      shiny::observe({
        if (is.null(reactive_df)) {
          output$data_table_output <- NULL
        } else {
          output$data_table_output <- DT::renderDataTable(
            reactive_df(),
            filter = "top",
            options = list(
              scrollX = TRUE,
              pageLength = 10,
              info = FALSE,
              lengthMenu = list(c(10, -1), c("10", "All"))
            )
          )
        }
      }) %>% shiny::bindEvent(reactive_df())
    }
  )
}