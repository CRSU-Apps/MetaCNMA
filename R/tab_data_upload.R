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

      # Use a counter in a reactiveVal to allow data to be invalidated
      invalidate_count <- shiny::reactiveVal(0)

      data <- shiny::reactiveVal(NULL)
      is_default_data <- shiny::reactiveVal(NULL)

      # Wrap the file_input from default_elements.R into a render function
      file_input <- shiny::renderUI(default_file_input(ns)) # nolint: object_usage

      # Logical to show reset button only when data uploaded
      data_uploaded <- shiny::reactiveVal(FALSE)
      output$data_uploaded <- shiny::reactive({
        data_uploaded()
      })
      shiny::outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)

      shiny::observe({
        shinydashboard::updateTabItems(parent_session(), "tabs", "data_help")
      }) %>% shiny::bindEvent(input$data_help_link,
        ignoreInit = TRUE, ignoreNULL = TRUE
      )

      # Render the file input intially
      output$file_input <- file_input

      shiny::observe({
        data(NULL)
      }) %>% shiny::bindEvent(
        invalidate_count(),
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

      # Trigger a data reload on reload button or data_type change
      shiny::observe({
        print("Reloading Data")
        reload(TRUE)
        output$file_input <- file_input
        data_uploaded(FALSE)
        data(NULL)
      }) %>% shiny::bindEvent(
        data_type(),
        input$reload_button,
        invalidate_count(),
        ignoreInit = TRUE
      )

      # Set data_uploaded when file_input changes
      shiny::observe({
        print("Setting Data Upload")
        if (validate_input(input$data, data_type())) {
          print("Data Validated Loading Data")
          reload(FALSE)
          data_uploaded(TRUE)
          is_default_data(FALSE)
          data(rio::import(input$data$datapath))
        } else {
          
        }
      }) %>% shiny::bindEvent(
        input$data,
        ignoreInit = TRUE
      )

      return(
        list(
          data = data,
          is_default_data = is_default_data,
          invalidate_count = invalidate_count
        )
      )

    }
  )
}