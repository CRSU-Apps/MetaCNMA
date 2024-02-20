run_bayesian_analysis_ui <- function(
  id
) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("warning")),
    shiny::uiOutput(ns("info")),
    shiny::div(
      shiny::uiOutput(ns("run_analysis")),
      stan_settings_ui(ns("bayesian_settings")), # nolint: object_usage
    ),
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

      stan_settings_server(
        "bayesian_settings",
        bayesian_ready
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
        print("Setting Model to NULL")
        bayesian_reactives$model(NULL)
      }) %>% shiny::bindEvent(
        bayesian_options$update_reactive(),
        bayesian_options$update_options()
      )

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
              tmp_df_file <- tempfile("df", fileext = ".Rds")
              rio::export(data_reactives$formatted_data(), tmp_df_file)
              tmp_output_file <- tempfile("output", fileext = ".Rds")
              bayesian_reactives$console_out(system2(
                command = file.path(R.home("bin"), "Rscript"),
                args = c(
                  "run_bayes.R",
                  tmp_df_file,
                  tmp_output_file,
                  data_reactives$data_type(),
                  paste0(
                    "'",
                    data_reactives$default_reference_component(),
                    "'"
                  ),
                  bayesian_options$random_effects(),
                  toupper(
                    bayesian_options$original_outcome_measure()
                  )
                ),
                stdout = TRUE
              ))
              bayesian_reactives$model(rio::import(tmp_output_file))
              unlink(tmp_df_file)
              unlink(tmp_output_file)
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
            {
              cat(
                bayesian_reactives$console_out(),
                sep = "\n"
              )
            }
          )
        )
      }) %>% shiny::bindEvent(
        bayesian_reactives$console_out()
      )

    }
  )
}
