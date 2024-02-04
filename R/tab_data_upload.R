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

      data_reactives <- shiny::reactiveValues()

      data_reactives$data_type <- shiny::reactive({
        data_type()
      })

      # Create a definable reactive value to allow reloading of data
      data_reactives$reload <- shiny::reactiveVal(FALSE)

      # Use a counter in a reactiveVal to allow data to be invalidated
      data_reactives$invalidate_count <- shiny::reactiveVal(0)

      data_reactives$data <- shiny::reactiveVal(NULL)
      data_reactives$is_default_data <- shiny::reactiveVal(NULL)

      # Wrap the file_input from default_elements.R into a render function
      file_input <- shiny::renderUI(default_file_input(ns)) # nolint: object_usage

      # Logical to show reset button only when data uploaded
      data_reactives$data_uploaded <- shiny::reactiveVal(FALSE)
      output$data_uploaded <- shiny::reactive({
        data_reactives$data_uploaded()
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
        data_reactives$data(NULL)
      }) %>% shiny::bindEvent(
        data_reactives$invalidate_count(),
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

      # Trigger a data reload on reload button or data_type change
      shiny::observe({
        print("Reloading Data")
        data_reactives$reload(TRUE)
        output$file_input <- file_input
        data_reactives$data_uploaded(FALSE)
        data_reactives$data(NULL)
      }) %>% shiny::bindEvent(
        data_reactives$data_type(),
        input$reload_button,
        data_reactives$invalidate_count(),
        ignoreInit = TRUE
      )

      # Set data_uploaded when file_input changes
      shiny::observe({
        print("Setting Data Upload")
        if (validate_input(input$data, data_reactives$data_type())) {
          print("Data Validated Loading Data")
          data_reactives$reload(FALSE)
          data_reactives$data_uploaded(TRUE)
          data_reactives$is_default_data(FALSE)
          data_reactives$data(rio::import(input$data$datapath))
        } else {
          data_reactives$invalidate_count(data_reactives$invalidate_count() + 1)
        }
      }) %>% shiny::bindEvent(
        input$data,
        ignoreInit = TRUE
      )

      data_reactives$is_data_loaded <- shiny::reactive({
        return(!is.null(data_reactives$data()))
      })

      data_reactives$formatted_data <- shiny::reactive({
        if (!data_reactives$is_data_loaded()) {
          return(NULL)
        } else {
          return(
            format_data( # nolint: object_usage
              data_reactives$data(),
              data_reactives$data_type()
            )
          )
        }
      })

      data_reactives$is_data_formatted <- shiny::reactive({
        return(!is.null(data_reactives$formatted_data()))
      })

      data_reactives$studies <- shiny::reactive({
        if (is.null(data_reactives$is_data_formatted())) {
          return(NULL)
        } else {
          return(
            levels(as.factor(data_reactives$formatted_data()$study))
          )
        }
      })

      data_reactives$pairwise <- shiny::reactive({
        if (!data_reactives$is_data_formatted()) {
          return(NULL)
        } else {
          return(
            freq_pairwise( # nolint: object_usage
              data_reactives$formatted_data(),
              data_reactives$data_type()
            )
          )
        }
      })

      data_reactives$default_reference_component <- shiny::reactive({
        if (is.null(data_reactives$pairwise())) {
          return(NULL)
        } else {
          return(
            get_most_freq_component( # nolint: object_usage
              data_reactives$pairwise()
            )
          )
        }
      })

      return(
        data_reactives
      )

    }
  )
}