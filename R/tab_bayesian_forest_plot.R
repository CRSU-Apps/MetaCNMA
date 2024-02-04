bayes_forest_plot_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Forest Plot"),
    message_tag_list(ns), # nolint: object_usage
    run_bayesian_analysis_ui(ns("run_bayesian_analysis")),
    save_plot_ui( # nolint: object_usage
      ns("save_bayesian_forest_plot"),
      output_name = "Bayesian_Forest_Plot"
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

bayes_forest_plot_tab_server <- function(
  id,
  data_reactives,
  bayesian_options,
  bayesian_reactives,
  tab
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      run_bayesian_analysis_server(
        "run_bayesian_analysis",
        data_reactives,
        bayesian_options,
        bayesian_reactives
      )

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
            bayesian_options$options_loaded(),
            bayesian_reactives$is_model_run(),
            cancelOutput = TRUE
          )
          print("bayesian forest_plot")
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
                    bayesian_options$outcome_measure(),
                    " of ",
                    bayesian_options$outcome_name(),
                    " when compared against ",
                    get_most_freq_component( # nolint: object_name
                      data_reactives$pairwise()
                    )
                  )
                )
                forest_plot <- function() {
                  plot(bayesian_reactives$model())
                }
                output$forest_plot <- shiny::renderPlot(
                  forest_plot()
                )
                is_rendered(TRUE)
                save_plot_server("save_bayesian_forest_plot",
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
        bayesian_options$update_reactive(),
        bayesian_reactives$is_model_run()
      )

    }
  )
}