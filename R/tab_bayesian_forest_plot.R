bayes_forest_plot_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Forest Plot"),
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel(
        "Main Forest Plot",
        shiny::uiOutput(ns("warning")),
        shiny::uiOutput(ns("info")),
        run_bayesian_analysis_ui(ns("run_bayesian_analysis")),
        save_plot_ui( # nolint: object_name
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
      ),
      shiny::tabPanel(
        "Sensitivity Analysis Forest Plot",
        shiny::uiOutput(ns("warning_sens")),
        shiny::uiOutput(ns("info_sens")),
        run_bayesian_analysis_ui(ns("run_bayesian_analysis_sens")),
        save_plot_ui( # nolint: object_name
          ns("save_bayesian_forest_plot_sens"),
          output_name = "Bayesian_Forest_Plot_Sensitivity"
        ),
        shinydashboardPlus::box(
          title = shiny::textOutput(ns("plot_title_sens")),
          id = ns("forest_plot_box_sens"),
          width = 12,
          shinycssloaders::withSpinner(
            shiny::plotOutput(ns("forest_plot_sens")),
            type = 6
          )
        )
      )
    )
  )
}

bayes_forest_plot_tab_server <- function(
  id,
  data_reactives,
  bayesian_options,
  bayesian_reactives,
  bayes_sens_data_reactives,
  bayesian_sens_reactives,
  shared_stan_settings,
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
        bayesian_reactives,
        shared_stan_settings
      )

      run_bayesian_analysis_server(
        "run_bayesian_analysis_sens",
        bayes_sens_data_reactives,
        bayesian_options,
        bayesian_sens_reactives,
        shared_stan_settings
      )

      is_rendered <- shiny::reactiveVal(FALSE)
      is_rendered_sens <- shiny::reactiveVal(FALSE)

      shiny::observe({
        if (tab() == id) {
          output$warning <- NULL
          output$info <- output$info <- shiny::renderUI(
            message_alert("Please run the model") # nolint: object_name
          )
          output$plot_title <- NULL
          output$forest_plot <- NULL
          is_rendered(FALSE)
          #print(tab())
          shiny::req(
            bayesian_options$options_loaded(),
            bayesian_reactives$is_model_run(),
            cancelOutput = TRUE
          )
          output$info <- NULL
          #print("bayesian forest_plot")
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
                    data_reactives$reference_component()
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

      shiny::observe({
        if (tab() == id) {
          output$warning_sens <- NULL
          output$info_sens <- shiny::renderUI(
            message_alert("Please select studies to exclude and run the model") # nolint: object_name
          )
          output$plot_title_sens <- NULL
          output$forest_plot_sens <- NULL
          is_rendered_sens(FALSE)
          #print(tab())
          shiny::req(
            bayesian_options$options_loaded(),
            bayesian_sens_reactives$is_model_run(),
            cancelOutput = TRUE
          )
          output$info_sens <- NULL
          #print("bayesian forest_plot sens")
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
                output$plot_title_sens <- shiny::renderText(
                  paste0("Forest plot showing the ",
                    bayesian_options$outcome_measure(),
                    " of ",
                    bayesian_options$outcome_name(),
                    " when compared against ",
                    data_reactives$reference_component()
                  )
                )
                forest_plot_sens <- function() {
                  plot(bayesian_sens_reactives$model())
                }
                output$forest_plot_sens <- shiny::renderPlot(
                  forest_plot_sens()
                )
                is_rendered_sens(TRUE)
                save_plot_server("save_bayesian_forest_plot_sens",
                  forest_plot_sens,
                  is_rendered_sens
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