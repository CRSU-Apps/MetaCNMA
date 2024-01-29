freq_exclude_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Exclude Studies (Sensitivity Analysis)"),
    message_tag_list(ns), # nolint: object_usage
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel(
        shiny::h2("Exclude Studies"),
        shinycssloaders::withSpinner(
          shiny::uiOutput(ns("study_selection")),
          type = 6
        )
      ),
      shiny::tabPanel(
        shiny::h2("Excluded Studies: Data Summary"),
        shinycssloaders::withSpinner(
          shiny::uiOutput(ns("data_summary")),
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

      freq_exclude <- shiny::reactiveValues

      shiny::observe({
        if (tab() == id) {
          output$warning <- NULL
          output$info <- NULL
          output$data_summary <- NULL
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
                    choices = freq_reactives$studies()
                  )
                )
                output$data_summary <- DT::renderDataTable(
                  freq_reactives$formatted_data(),
                  filter = "top",
                  options = list(
                    scrollX = TRUE,
                    pageLength = 10,
                    info = FALSE,
                    lengthMenu = list(c(10, -1), c("10", "All"))
                  )
                )
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
        load_default_data(reactive_data, reactive_freq) # nolint: object_name
      }) %>% shiny::bindEvent(input$default_data)

      shiny::observe({
        #data$exclude <- input$exclude
      }) %>% shiny::bindEvent(input$exclude)

    }
  )
}