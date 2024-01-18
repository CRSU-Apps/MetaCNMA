data_summary_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("UploDataad "),
    message_tag_list(ns), # nolint: object_usage
  )
}

data_summary_tab_server <- function(id, reactive_data, reactive_freq, tab) {
  shiny::moduleServer(id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`
      
    }
  )
}