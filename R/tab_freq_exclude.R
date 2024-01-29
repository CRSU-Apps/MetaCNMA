freq_exclude_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Exclude Studies (Sensitivity Analysis)"),
    message_tag_list(ns), # nolint: object_usage
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel(
        "Exclude Studies",
        shiny::br(),
        shinydashboardPlus::box(
          title = "Original Data",
          id = ns("original_studies_box"),
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          data_table_module_ui(ns("original_table_output")) # nolint: object_usage
        ),
        shinycssloaders::withSpinner(
          shiny::uiOutput(ns("study_selection")),
          type = 6
        ),
      ),
      shiny::tabPanel(
        "Excluded Studies: Data Summary",
        shinycssloaders::withSpinner(
          data_table_module_ui(ns("excluded_studies_table_output")), # nolint: object_usage
          type = 6
        )
      )
    )
  )
}

freq_exclude_tab_server <- function(
  id,
  freq_options,
  freq_reactives,
  tab
) {
  shiny::moduleServer(id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      freq_exclude <- shiny::reactiveValues()

      .reactive_df <- shiny::reactiveVal(NULL)
      freq_exclude$formatted_data <- shiny::reactiveVal(NULL)

      data_table_module_server(
        "original_table_output",
        .reactive_df
      )

      data_table_module_server(
        "original_table_output",
        freq_exclude$formatted_data
      )

      shiny::observe({
        if (tab() == id) {
          output$warning <- NULL
          output$info <- NULL
          output$data_summary <- NULL
          .reactive_df <- NULL
          print(tab())
          shiny::req(
            !is.null(freq_reactives$studies()),
            cancelOutput = TRUE
          )
          tryCatch({
            withCallingHandlers(
              warning = function(cond) {
                output$warning <- shiny::renderUI(
                  warning_alert(conditionMessage(cond)) # nolint: object_name
                )
              },
              message = function(cond) {
                output$info <- shiny::renderUI(
                  message_alert(conditionMessage(cond)) # nolint: object_name
                )
              },
              {
                # Setting Studies
                output$study_selection <- shiny::renderUI(
                  shiny::checkboxGroupInput("exclude",
                    label = "Choose any Studies you wish to exclude",
                    choices = freq_reactives$studies(),
                    inline = TRUE
                  )
                )
                .reactive_df(freq_reactives$formatted_data())
              }
            )
          },
          error = function(e) {
            print("this error occured trying to render the studies")
            error_alert(e$message) # nolint: object_name
          })
        }
      }) %>% shiny::bindEvent(
        tab(),
        freq_options$update_reactive()
      )

      shiny::observe({
        # Do Something
      }) %>% shiny::bindEvent(input$exclude)

      return(freq_exclude)

    }
  )
}