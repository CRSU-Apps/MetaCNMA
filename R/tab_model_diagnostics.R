model_diagnostics_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Model Diagnostics"),
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel(
        "Main Stan Model Diagnostics",
        message_tag_list(ns), # nolint: object_name
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
          save_plot_ui( # nolint: object_name
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
          save_plot_ui( # nolint: object_name
            ns("save_trace_plots"),
            output_name = "Trace_Plots"
          ),
          shinycssloaders::withSpinner(
            shiny::uiOutput(ns("trace_plots")),
            type = 6
          )
        )
      ),
      shiny::tabPanel(
        "Sensitivity Analysis Stan Model Diagnostics",
        shiny::uiOutput(ns("warning_sens")),
        shiny::uiOutput(ns("info_sens")),
        run_bayesian_analysis_ui(ns("run_bayesian_analysis_sens")),
        shinydashboardPlus::box(
          title = "Sampler Diagnostics",
          id = ns("sampler_diagnostics_box_sens"),
          collapsible = TRUE,
          width = 12,
          shinycssloaders::withSpinner(
            shiny::uiOutput(ns("sampler_diagnostics_sens")),
            type = 6
          )
        ),
        shinydashboardPlus::box(
          title = "R-hat Statistics",
          id = ns("rhat_diagnostics_box_sens"),
          collapsible = TRUE,
          width = 12,
          shinycssloaders::withSpinner(
            shiny::uiOutput(ns("rhat_diagnostics_sens")),
            type = 6
          )
        ),
        shinydashboardPlus::box(
          title = "Density Plots",
          id = ns("density_plots_box_sens"),
          collapsible = TRUE,
          width = 12,
          save_plot_ui( # nolint: object_name
            ns("save_density_plots_sens"),
            output_name = "Density_Plots_Sensitivity"
          ),
          shinycssloaders::withSpinner(
            shiny::uiOutput(ns("density_plots_sens")),
            type = 6
          )
        ),
        shinydashboardPlus::box(
          title = "Trace Plots",
          id = ns("trace_plots_box_sens"),
          collapsible = TRUE,
          width = 12,
          save_plot_ui( # nolint: object_name
            ns("save_trace_plots_sens"),
            output_name = "Trace_Plots_Sensitivity"
          ),
          shinycssloaders::withSpinner(
            shiny::uiOutput(ns("trace_plots_sens")),
            type = 6
          )
        )
      )
    )
  )
}

model_diagnostics_tab_server <- function(
  id,
  data_reactives,
  bayesian_options,
  bayesian_reactives,
  bayes_sens_data_reactives,
  bayesian_sens_reactives,
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

      run_bayesian_analysis_server(
        "run_bayesian_analysis_sens",
        bayes_sens_data_reactives,
        bayesian_options,
        bayesian_sens_reactives
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
                sampler_diagnostics <- get_sampler_diagnostics(bayesian_reactives$model()$fit)
                output$sampler_diagnostics <- shiny::renderUI(
                  DT::renderDataTable(
                    DT::datatable(
                      sampler_diagnostics
                    ) %>%
                      DT::formatStyle(
                        columns = c("Category"),
                        valueColumns = c("Category"),
                        target = 'row',
                        backgroundColor =
                          DT::styleEqual(
                            c("bad", "good"),
                            c("red", "green")
                          )
                      )
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

      is_density_rendered_sens <- shiny::reactiveVal(FALSE)
      is_trace_rendered_sens <- shiny::reactiveVal(FALSE)

      shiny::observe({
        if (tab() == id) {
          output$warning_sens <- NULL
          output$info_sens <- NULL
          output$sampler_diagnostics_sens <- NULL
          output$rhat_diagnostics_sens <- NULL
          output$density_plots_sens <- NULL
          output$trace_plots_sens <- NULL
          is_density_rendered_sens(FALSE)
          is_trace_rendered_sens(FALSE)
          print(tab())
          shiny::req(
            bayesian_options$options_loaded(),
            bayesian_sens_reactives$is_model_run(),
            cancelOutput = TRUE
          )
          tryCatch({
            withCallingHandlers(
              warning = function(cond) {
                output$warning_sens <- shiny::renderUI(
                  warning_alert(conditionMessage(cond)) # nolint: object_name
                )
              },
              message = function(cond) {
                output$info_sens <- shiny::renderUI(
                  message_alert(conditionMessage(cond))  # nolint: object_name
                )
              },
              {
                output$sampler_diagnostics_sens <- shiny::renderUI(
                  DT::renderDataTable(
                    get_sampler_diagnostics(bayesian_sens_reactives$model()$fit)
                  )
                )
                output$rhat_diagnostics_sens <- shiny::renderUI(
                  DT::renderDataTable(
                    get_rhat_diagnostics(
                      bayesian_sens_reactives$model()$fit,
                      bayesian_options$random_effects()
                    )
                  )
                )
                density_plot_sens <- function() {
                  print(
                    get_denisty_plots(
                      bayesian_sens_reactives$model()$fit,
                      bayesian_options$random_effects()
                    )
                  )
                }
                output$density_plots_sens <- shiny::renderUI(
                  shiny::renderPlot(
                    density_plot_sens()
                  )
                )
                is_density_rendered_sens(TRUE)
                save_plot_server("save_density_plots_sens",
                  density_plot_sens,
                  is_density_rendered_sens
                )
                trace_plot_sens <- function() {
                  print(
                    get_trace_plots(
                      bayesian_sens_reactives$model()$fit,
                      bayesian_options$random_effects()
                    )
                  )
                }
                output$trace_plots_sens <- shiny::renderUI(
                  shiny::renderPlot(
                    trace_plot_sens()
                  )
                )
                is_trace_rendered_sens(TRUE)
                save_plot_server("save_trace_plots_sens",
                  trace_plot_sens,
                  is_trace_rendered_sens
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
        bayesian_sens_reactives$is_model_run()
      )

    }
  )
}