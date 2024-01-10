data_upload_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Upload Data"),
    shiny::p("Data should be uploaded as either a .csv or .xlsx file"),
    shiny::p(
      "For help on uploading data see the ",
      shiny::actionLink(ns("data_help_link"), "data help tab.")
    ),
    shiny::column(
      width = 6,
      file_input_ui("data_upload_file_input"), # nolint: object_usage
      offset = 2
    ),

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

      uploaded_data <- file_input_server( # nolint: object_usage
        "data_upload_file_input"
      )

    }
  )
}