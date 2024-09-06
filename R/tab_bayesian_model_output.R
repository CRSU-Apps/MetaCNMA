bayes_model_output_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Model Output"),
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel(
        "Main Model Output",
        shiny::uiOutput(ns("warning")),
        shiny::uiOutput(ns("info")),
        run_bayesian_analysis_ui(ns("run_bayesian_analysis")),
        #save_plot_ui( # nolint: object_name
        #  ns("save_bayesian_forest_plot"),
        #  output_name = "Bayesian_Forest_Plot"
        #),
        shinydashboardPlus::box(
          title = shiny::textOutput(ns("model_title")),
          id = ns("model_output_box"),
          width = 12,
          shinycssloaders::withSpinner(
            shiny::uiOutput(ns("model_output")),
            type = 6
          )
        )
      ),
      shiny::tabPanel(
        "Sensitivity Analysis Forest Plot",
        shiny::uiOutput(ns("warning_sens")),
        shiny::uiOutput(ns("info_sens")),
        run_bayesian_analysis_ui(ns("run_bayesian_analysis_sens")),
        #save_plot_ui( # nolint: object_name
        #  ns("save_bayesian_forest_plot_sens"),
        #  output_name = "Bayesian_Forest_Plot_Sensitivity"
        #),
        shinydashboardPlus::box(
          title = shiny::textOutput(ns("model_title_sens")),
          id = ns("model_output_box_sens"),
          width = 12,
          shinycssloaders::withSpinner(
            shiny::uiOutput(ns("model_output_sens")),
            type = 6
          )
        )
      )
    )
  )
}

bayes_model_output_tab_server <- function(
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
          output$model_title <- NULL
          output$model_output <- NULL
          is_rendered(FALSE)
          shiny::req(
            bayesian_options$options_loaded(),
            bayesian_reactives$is_model_run(),
            cancelOutput = TRUE
          )
          output$info <- NULL
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
                output$model_title <- shiny::renderText(
                  paste0("Model Output showing the ",
                    bayesian_options$outcome_measure(),
                    " of ",
                    bayesian_options$outcome_name(),
                    " when compared against ",
                    data_reactives$reference_component()
                  )
                )
                model_output <- get_bayes_model_output(
                  data_reactives$data_type(),
                  toupper(
                    bayesian_options$original_outcome_measure()
                  ),
                  bayesian_reactives$model()
                )
                output$model_output <- shiny::renderUI(
                  DT::renderDataTable(
                    DT::datatable(
                      model_output
                    ) %>%
                      DT::formatRound(columns = 2:5, digits = 2)
                  )
                )
                is_rendered(TRUE)
              }
            )
          },
          error = function(e) {
            error_alert(e$message) # nolint: object_name
          })
        }
      }) %>%
        shiny::bindEvent(
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
          output$model_title_sens <- NULL
          output$model_output_sens <- NULL
          is_rendered_sens(FALSE)
          shiny::req(
            bayesian_options$options_loaded(),
            bayesian_sens_reactives$is_model_run(),
            cancelOutput = TRUE
          )
          output$info_sens <- NULL
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
                output$model_title_sens <- shiny::renderText(
                  paste0("Model Output showing the ",
                    bayesian_options$outcome_measure(),
                    " of ",
                    bayesian_options$outcome_name(),
                    " when compared against ",
                    data_reactives$reference_component()
                  )
                )
                model_output_sens <- get_bayes_model_output(
                  data_reactives$data_type(),
                  toupper(
                    bayesian_options$original_outcome_measure()
                  ),
                  bayesian_sens_reactives$model()
                )
                output$model_output_sens <- shiny::renderUI(
                  DT::renderDataTable(
                    DT::datatable(
                      model_output_sens
                    ) %>%
                      DT::formatRound(columns = 2:5, digits = 2)
                  )
                )
                is_rendered_sens(TRUE)
              }
            )
          },
          error = function(e) {
            error_alert(e$message) # nolint: object_name
          })
        }
      }) %>%
        shiny::bindEvent(
          tab(),
          bayesian_options$update_reactive(),
          bayesian_sens_reactives$is_model_run()
        )

    }
  )
}