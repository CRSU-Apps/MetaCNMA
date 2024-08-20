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
      # Scope the magrittr pipe
      `%>%` <- magrittr::`%>%`
      # Get the current namespace from the session
      ns <- session$ns

      # Forward declare and set reactive for data_type
      data_type <- shiny::reactiveVal("continuous")

      # Upadte the data_type output if the data_type reactive changes
      # Allows the data_type to be changed outside this module
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

      # Debouce the data_type and store in reactive
      shiny::observe({
        # Debounce to prevent excessive waterfall of reactives
        shiny::debounce(data_type(input$data_type), 1000)
        print(input$data_type)
      }) %>% shiny::bindEvent(input$data_type)

      # Return the data_type to allow access outside of module
      return(data_type)

    }
  )
}
