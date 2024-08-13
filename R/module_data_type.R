data_type_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      shiny::uiOutput(ns("data_type")),
      id = "window-data-type",
      class = "no-form"
    )
  )
}

data_type_module_server <- function(
  id
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      `%>%` <- magrittr::`%>%`
      ns <- session$ns

      data_type <- shiny::reactiveVal("continuous")

      shiny::observe({
        output$data_type <- shiny::renderUI(
          shinyWidgets::prettyRadioButtons(
            ns("data_type"),
            "Data Type ",
            c(
              "Continuous" = "continuous",
              "Binary" = "binary"
            ),
            inline = TRUE,
            selected = data_type()
          )
        )
      }) %>% shiny::bindEvent(
        data_type()
      )

      shiny::observe({
        shiny::debounce(data_type(input$data_type), 1000)
        print(input$data_type)
      }) %>% shiny::bindEvent(input$data_type)

      return(data_type)

    }
  )
}
