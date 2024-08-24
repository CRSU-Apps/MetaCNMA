run_bayesian_analysis_ui <- function(
  id
) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("warning")),
    shiny::uiOutput(ns("info")),
    shiny::div(
      shiny::uiOutput(ns("run_analysis")),
      stan_settings_ui(ns("bayesian_settings")), # nolint: object_name
    ),
    shiny::br(),
    shiny::div(class = "clearfix"),
  )
}

run_bayesian_analysis_server <- function(
  id,
  data_reactives,
  bayesian_options,
  bayesian_reactives,
  shared_stan_settings
) {
  shiny::moduleServer(id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      bayesian_ready <- shiny::reactiveVal(FALSE)
      stan_settings_server(
        "bayesian_settings",
        bayesian_ready,
        bayesian_reactives,
        shared_stan_settings
      )

      shiny::observe({
        output$run_analysis <- NULL
        bayesian_ready(FALSE)

        shiny::req(
          !is.null(data_reactives$data()),
          cancelOutput = TRUE
        )
        if (
          all(
            data_reactives$is_data_formatted(),
            bayesian_options$options_loaded(),
            !bayesian_reactives$is_model_run()
          )
        ) {
          output$warning <- NULL
          output$info <- NULL
          output$run_analysis <- shiny::renderUI(
            shiny::tagList(
              message_alert(
                "Running the Bayesian analysis may 
                take up to a couple of minutes"
              ),
              shiny::actionButton(
                ns("run_model_button"),
                "Run Model",
                shiny::icon("play"),
                style =
                  "
                    color: #fff;
                    background-color:#dc3545;
                    border-color:#dc3545;
                    float: left;
                  "
              )
            )
          )
          bayesian_ready(TRUE)
        }
      }) %>% shiny::bindEvent(
        bayesian_options$update_reactive(),
        bayesian_options$options_loaded(),
        bayesian_reactives$is_model_run(),
        data_reactives$data()
      )

      shiny::observe({
        bayesian_reactives$model(NULL)
      }) %>% shiny::bindEvent(
        bayesian_options$update_reactive(),
        bayesian_options$update_options()
      )

      shiny::observe({
        warnings <- ""
        tryCatch({
          withCallingHandlers(
            warning = function(cond) {
              warnings <<- paste(warnings, conditionMessage(cond), sep = "<br>")
            },
            message = function(cond) {
              output$info <- shiny::renderUI(
                message_alert(conditionMessage(cond))  # nolint: object_name
              )
            },
            {
              #print("Running Bayesian Model")
              shinybusy::show_modal_spinner(
                spin = "atom",
                color = "#005398",
                text = "Running Model"
              )
              bayesian_reactives$model(
                fit_model(
                  data_reactives$formatted_data(),
                  data_reactives$data_type(),
                  data_reactives$reference_component(),
                  bayesian_options$random_effects(),
                  toupper(
                    bayesian_options$original_outcome_measure()
                  ),
                  shared_stan_settings$chains(),
                  shared_stan_settings$warmup(),
                  shared_stan_settings$iter(),
                  shared_stan_settings$seed(),
                  shared_stan_settings$max_treedepth(),
                  shared_stan_settings$adapt_delta(),
                  shared_stan_settings$stepsize()
                )
              )
            }
          )
        },
        error = function(e) {
          print(e)
          error_alert(e$message) # nolint: object_name
        },
        finally = {
          if (warnings != "") {
            output$warning <- shiny::renderUI(
              warning_alert(warnings) # nolint: object_name
            )
          }
          shinybusy::remove_modal_spinner()
        })
      }) %>% shiny::bindEvent(
        input$run_model_button
      )
    }
  )
}
