default_data_ui <- function(
  id
) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("no_data"))
}

default_data_server <- function(
  id,
  data_reactives
) {
  function(input,
           output,
           session) {

    ns <- session$ns

    `%>%` <- magrittr::`%>%`

    shiny::observe({
      #print("Data Change")
      shiny::req(
        !data_reactives$is_data_loaded(),
        cancelOutput = TRUE
      )
      output$no_data <- shiny::renderUI(
        default_no_data(ns)
      )
    }) %>% shiny::bindEvent(
      data_reactives$is_data_loaded()
    )

    shiny::observe({
      data_reactives$load_default_data(TRUE)
    }) %>% shiny::bindEvent(
      input$reload_data
    )

  }

}