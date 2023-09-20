home_tab_server <- function(id, reactive_data, reactive_freq) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {
      ns <- shiny::NS(id)

      require(dplyr)

      output$outputs <- NULL

      output$static_content <- shiny::renderUI(shiny::tagList(
        shiny::img(
          src = "images/MetaCNMALogo.png",
          alt = "Logo for Meta CNMA App",
          class = "img-responsive center-block",
          id = "home-logo"
        ),
        shiny::fluidRow(shiny::column(
          width = 6,
          shiny::radioButtons(
            ns("dataType"),
            "Select the Type of Data",
            c(
              "Continuous" = "continuous",
              "Binary" = "binary"
            )
          ),
          offset = 3
        ))
      ))

      shiny::observe({
        reactive_data()$data_type(input$dataType)
        reactive_freq()$invalidate()
        load_default_data(reactive_data, reactive_freq) # nolint object_usage
        print(reactive_data()$data_type())
      }) %>%
        shiny::bindEvent(input$dataType, ignoreInit = FALSE, ignoreNULL = TRUE)
    }
  )
}
