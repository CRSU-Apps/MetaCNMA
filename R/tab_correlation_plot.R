correlation_plot_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Network Diagram"),
    message_tag_list(ns), # nolint: object_usage
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel(
        "Component Correlation Plot",
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns("correlation_plot")),
          type = 6
        )
      ),
      shiny::tabPanel(
        "Component Heatmap",
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns("component_heatmap")),
          type = 6
        )
      )
    )
  )
}

correlation_plot_tab_server <- function(id, freq_options, freq_reactives, tab) {
  shiny::moduleServer(id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      shiny::observe({
        if (tab() == id) {
          output$warning <- NULL
          output$info <- NULL
          output$correlation_plot <- NULL
          output$component_heatmap <- NULL
          print(tab())
          shiny::req(
            !is.null(freq_reactives$pairwise()),
            !is.null(freq_reactives$formatted_data()),
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
                output$correlation_plot <- shiny::renderPlot(
                  get_correlation_plot( # nolint: object_usage
                    freq_reactives$formatted_data(),
                    get_components_no_reference(freq_reactives$pairwise()) # nolint: object_usage
                  )
                )
                output$component_heatmap <- shiny::renderPlot(
                  get_heatmap( # nolint: object_usage
                    freq_reactives$formatted_data(),
                    get_components_no_reference(freq_reactives$pairwise()) # nolint: object_usage
                  )
                )
              }
            )
          })
        }
      }) %>% shiny::bindEvent(
        tab(),
        freq_options$update_reactive()
      )
    }
  )
}