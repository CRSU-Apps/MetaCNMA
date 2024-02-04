run_bayesian_analysis_ui <- function(
  id
) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("warning")),
    shiny::uiOutput(ns("info")),
    shiny::uiOutput(ns("run_analysis")),
    shiny::uiOutput(ns("stan_settings")),
    shiny::br(),
    shinydashboardPlus::box(
      title = "Stan Output",
      id = ns("stan_output_box"),
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      shiny::uiOutput(ns("stan_output"))
    ),
    shiny::div(class = "clearfix"),
  )
}

run_bayesian_analysis_server <- function(
  id,
  data_reactives,
  bayesian_options,
  bayesian_reactives
) {
  shiny::moduleServer(id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      bayesian_ready <- shiny::reactiveVal(FALSE)

      shiny::observe({
        output$run_analysis <- NULL
        bayesian_ready(FALSE)
        print(bayesian_options$options_loaded())
        if (
          all(
            data_reactives$is_data_formatted(),
            bayesian_options$options_loaded(),
            !bayesian_reactives$is_model_run()
          )
        ) {
          output$run_analysis <- shiny::renderUI(
            shiny::actionButton(
              ns("run_model_button"),
              "Run Model",
              shiny::icon("play"),
              style =
                "color: #fff; background-color: #dc3545; border-color: #dc3545"
            )
          )
          bayesian_ready(TRUE)
        }
      }) %>% shiny::bindEvent(
        bayesian_options$update_reactive(),
        bayesian_options$options_loaded(),
        bayesian_reactives$is_model_run()
      )

      shiny::observe({
        print("Setting Model to NULL")
        bayesian_reactives$model(NULL)
      }) %>% shiny::bindEvent(
        bayesian_options$update_reactive(),
        bayesian_options$update_options()
      )

      console_out <- shiny::reactiveVal(NULL)

      shiny::observe({
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
              print("Running Bayesian Model")
              shinybusy::show_modal_spinner(
                spin = "atom",
                color = "#005398",
                text = "Running Model"
              )
              console_out(
                capture.output(
                  bayesian_reactives$model(
                    fit_model(
                      data_reactives$formatted_data(),
                      data_reactives$data_type(),
                      data_reactives$default_reference_component(),
                      bayesian_options$random_effects(),
                      toupper(
                        bayesian_options$original_outcome_measure()
                      )
                    )
                  )
                )
              )
            }
          )
        },
        error = function(e) {
            error_alert(e$message) # nolint: object_name
        },
        finally = {
          shinybusy::remove_modal_spinner()
        })
      }) %>% shiny::bindEvent(
        input$run_model_button
      )

      shiny::observe({
        print("Outputting to console")
        output$stan_output <- shiny::renderUI(
          shiny::renderPrint(
            console_out()
          )
        )
      }) %>% shiny::bindEvent(
        console_out
      )

    }
  )
}
