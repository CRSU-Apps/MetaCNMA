forest_plot_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Forest Plot"),
    message_tag_list(ns), # nolint: object_usage
    save_plot_ui( # nolint: object_usage
      ns("save_forest_plot"),
      output_name = "Freq_Forest_Plot"
    ),
    shinydashboardPlus::box(
      title = shiny::textOutput(ns("plot_title")),
      id = ns("forest_plot_box"),
      width = 12,
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns("forest_plot")),
        type = 6
      )
    )
  )
}

forest_plot_tab_server <- function(
  id,
  data_reactives,
  freq_options,
  freq_reactives,
  tab
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      is_rendered <- shiny::reactiveVal(FALSE)

      shiny::observe({
        if (tab() == id) {
          output$warning <- NULL
          output$info <- NULL
          output$plot_title <- NULL
          output$forest_plot <- NULL
          is_rendered(FALSE)
          print(tab())
          shiny::req(
            freq_options$options_loaded(),
            !is.null(data_reactives$pairwise()),
            !is.null(freq_reactives$netmeta()),
            !is.null(freq_reactives$netcomb()),
            cancelOutput = TRUE
          )
          print("forest_plot")
          tryCatch({
            withCallingHandlers(
              warning = function(cond) {
                output$warning <- shiny::renderUI(
                  warning_alert(conditionMessage(cond)) # nolint: object_name
                )
              },
              message = function(cond) {
                output$info <- shiny::renderUI(
                  message_alert(conditionMessage(cond))  # nolint: object_name
                )
              },
              {
                output$plot_title <- shiny::renderText(
                  paste0("Forest plot showing the ",
                    freq_options$outcome_measure(),
                    " of ",
                    freq_options$outcome_name(),
                    " when compared against ",
                    get_most_freq_component( # nolint: object_name
                      data_reactives$pairwise()
                    )
                  )
                )
                forest_plot <- function() {
                  get_net_forest( # nolint: object_name
                    freq_reactives$netcomb(),
                    freq_options$data_type(),
                    freq_options$outcome_measure()
                  )
                }
                output$forest_plot <- shiny::renderPlot(
                  forest_plot()
                )
                is_rendered(TRUE)
                save_plot_server("save_forest_plot",
                  forest_plot,
                  is_rendered
                )
              }
            )
          },
          error = function(e) {
            error_alert(e$message) # nolint: object_name
          })
        }
      }) %>% shiny::bindEvent(
        tab(),
        freq_options$update_reactive(),
        freq_options$update_options()
      )

    }
  )
}