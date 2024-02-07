model_diagnostics_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Model Diagnostics"),
    message_tag_list(ns), # nolint: object_usage
    run_bayesian_analysis_ui(ns("run_bayesian_analysis")),
    shinydashboardPlus::box(
      title = "Sampler Diagnostics",
      id = ns("sampler_diagnostics_box"),
      collapsible = TRUE,
      width = 12,
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("sampler_diagnostics")),
        type = 6
      )
    ),
    shinydashboardPlus::box(
      title = "R-hat Statistics",
      id = ns("rhat_diagnostics_box"),
      collapsible = TRUE,
      width = 12,
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("rhat_diagnostics")),
        type = 6
      )
    ),
    shinydashboardPlus::box(
      title = "Density Plots",
      id = ns("density_plots_box"),
      collapsible = TRUE,
      width = 12,
      save_plot_ui( # nolint: object_usage
        ns("save_density_plots"),
        output_name = "Density_Plots"
      ),
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("density_plots")),
        type = 6
      )
    ),
    shinydashboardPlus::box(
      title = "Trace Plots",
      id = ns("trace_plots_box"),
      collapsible = TRUE,
      width = 12,
      save_plot_ui( # nolint: object_usage
        ns("save_trace_plots"),
        output_name = "Trace_Plots"
      ),
      shinycssloaders::withSpinner(
        shiny::uiOutput(ns("trace_plots")),
        type = 6
      )
    )
  )
}

model_diagnostics_tab_server <- function(
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

      is_density_rendered <- shiny::reactiveVal(FALSE)
      is_trace_rendered <- shiny::reactiveVal(FALSE)

      shiny::observe({
        if (tab() == id) {
          output$warning <- NULL
          output$info <- NULL
          output$sampler_diagnostics <- NULL
          output$rhat_diagnostics <- NULL
          output$density_plots <- NULL
          output$trace_plots <- NULL
          is_density_rendered(FALSE)
          is_trace_rendered(FALSE)
          print(tab())
          shiny::req(
            bayesian_options$options_loaded(),
            bayesian_reactives$is_model_run(),
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
                  message_alert(conditionMessage(cond))  # nolint: object_name
                )
              },
              {
                output$sampler_diagnostics <- shiny::renderUI(
                  DT::renderDataTable(
                    get_sampler_diagnostics(bayesian_reactives$model()$fit)
                  )
                )
                output$rhat_diagnostics <- shiny::renderUI(
                  DT::renderDataTable(
                    get_rhat_diagnostics(
                      bayesian_reactives$model()$fit,
                      bayesian_options$random_effects()
                    )
                  )
                )
                density_plot <- function() {
                  print(
                    get_denisty_plots(
                      bayesian_reactives$model()$fit,
                      bayesian_options$random_effects()
                    )
                  )
                }
                output$density_plots <- shiny::renderUI(
                  shiny::renderPlot(
                    density_plot()
                  )
                )
                is_density_rendered(TRUE)
                save_plot_server("save_density_plots",
                  density_plot,
                  is_density_rendered
                )
                trace_plot <- function() {
                  print(
                    get_trace_plots(
                      bayesian_reactives$model()$fit,
                      bayesian_options$random_effects()
                    )
                  )
                }
                output$trace_plots <- shiny::renderUI(
                  shiny::renderPlot(
                    trace_plot()
                  )
                )
                is_trace_rendered(TRUE)
                save_plot_server("save_trace_plots",
                  trace_plot,
                  is_trace_rendered
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