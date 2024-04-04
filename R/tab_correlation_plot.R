correlation_plot_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Correlation Plot / Heatmap"),
    message_tag_list(ns), # nolint: object_name
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel(
        "Component Correlation Plot",
        save_plot_ui( # nolint: object_name
          ns("save_correlation_plot"),
          output_name = "Component_Correlation_Plot",
        ),
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns("correlation_plot")),
          type = 6
        )
      ),
      shiny::tabPanel(
        "Component Heatmap",
        save_plot_ui( # nolint: object_name
          ns("save_component_heatmap"),
          output_name = "Component_Heatmap",
        ),
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns("component_heatmap")),
          type = 6
        )
      )
    )
  )
}

correlation_plot_tab_server <- function(
  id,
  data_reactives,
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

      is_rendered_correlation_plot <- shiny::reactiveVal(FALSE)
      is_rendered_heatmap <- shiny::reactiveVal(FALSE)

      shiny::observe({
        if (tab() == id) {
          output$warning <- NULL
          output$info <- NULL
          output$correlation_plot <- NULL
          output$component_heatmap <- NULL
          is_rendered_correlation_plot(FALSE)
          is_rendered_heatmap(FALSE)
          print(tab())
          shiny::req(
            data_reactives$is_data_formatted(),
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
                correlation_plot <- function() {
                  get_correlation_plot( # nolint: object_name
                    data_reactives$formatted_data(),
                    data_reactives$components() # nolint: object_name
                  )
                }
                output$correlation_plot <- shiny::renderPlot(
                  correlation_plot()
                )
                is_rendered_correlation_plot(TRUE)
                save_plot_server("save_correlation_plot", # nolint: object_name
                  correlation_plot,
                  is_rendered_correlation_plot
                )
                .heatmap <- function() {
                  print(
                    get_heatmap( # nolint: object_name
                      data_reactives$formatted_data(),
                      data_reactives$components() # nolint: object_name
                    )
                  )
                }
                output$component_heatmap <- shiny::renderPlot(
                  .heatmap()
                )
                is_rendered_heatmap(TRUE)
                save_plot_server("save_component_heatmap", # nolint: object_name
                  .heatmap,
                  is_rendered_heatmap
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