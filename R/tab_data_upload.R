data_upload_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Upload Data"),
    message_tag_list(ns), # nolint: object_usage
    shiny::p("Data should be uploaded as either a .csv or .xlsx file"),
    shiny::p(
      "For help on uploading data see the ",
      shiny::actionLink(ns("data_help_link"), "data help tab.")
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::uiOutput(ns("file_input")),
        offset = 2
      ),
      shiny::column(
        width = 6,
        shiny::conditionalPanel(
          condition = "output.data_uploaded == true",
          ns = ns,
          p("test"),
          default_reload_button(ns) # nolint: object_usage
        )
      ),
      class = "vertical-align"
    )
  )
}

data_upload_tab_server <- function(
  id,
  data_type,
  parent_session
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      # Create a definable reactive value to allow reloading of data
      reload <- shiny::reactiveVal(FALSE)

      file_input <- shiny::renderUI(default_file_input(ns)) # nolint: object_usage

      # Logical to show reset button only when data uploaded
      data_uploaded <- shiny::reactiveVal(FALSE)
      output$data_uploaded <- shiny::reactive({
        data_uploaded()
      })
      shiny::outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)

      # Render the file input intially
      output$file_input <- file_input

      shiny::observe({
        reload(TRUE)
        output$file_input <- file_input
        data_uploaded(FALSE)
      }) %>% shiny::bindEvent(
        data_type(),
        input$reload_button,
        ignoreInit = TRUE
      )

      shiny::observe({
        reload(FALSE)
        data_uploaded(TRUE)
      }) %>% shiny::bindEvent(
        input$data,
        ignoreInit = TRUE
      )

    }
  )
}