file_input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    default_file_input(ns) # nolint: object_usage
  )
}

file_input_server <- function(
  id
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      return(shiny::reactive(input$data))
    }
  )
}