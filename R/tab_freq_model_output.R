freq_model_output_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Model Output"),
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel(
        "Main Model Output",
        # save_plot_ui( # nolint: object_name
        #   ns("save_forest_plot"),
        #   output_name = "Freq_Forest_Plot"
        # ),
        shinydashboardPlus::box(
          title = shiny::textOutput(ns("model_output_title")),
          id = ns("model_output_box"),
          width = 12,
          shiny::uiOutput(ns("warning")),
          shiny::uiOutput(ns("info")),
          shinycssloaders::withSpinner(
            shiny::uiOutput(ns("model_output")),
            type = 6
          )
        )
      ),
      shiny::tabPanel(
        "Sensitivity Analysis Model Output",
        # save_plot_ui( # nolint: object_name
        #   ns("save_forest_plot_sens"),
        #   output_name = "Freq_Forest_Plot_Sensitivity"
        # ),
        shinydashboardPlus::box(
          title = shiny::textOutput(ns("model_output_title_sens")),
          id = ns("model_output_box_sens"),
          width = 12,
          shiny::uiOutput(ns("warning_sens")),
          shiny::uiOutput(ns("info_sens")),
          shinycssloaders::withSpinner(
            shiny::uiOutput(ns("model_output_sens")),
            type = 6
          )
        )
      )
    )
  )
}

freq_model_output_tab_server <- function(
  id,
  data_reactives,
  freq_options,
  freq_reactives,
  freq_sens_reactives,
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
      is_rendered_sens <- shiny::reactiveVal(FALSE)

      shiny::observe({
        if (tab() == id) {
          output$warning <- NULL
          output$info <- NULL
          output$model_output_title <- NULL
          output$model_output <- NULL
          is_rendered(FALSE)
          #print(tab())
          shiny::req(
            freq_options$options_loaded(),
            !is.null(freq_reactives$model()),
            cancelOutput = TRUE
          )
          #print("forest_plot")
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
                output$model_output_title <- shiny::renderText(
                  paste0("Model Output showing the ",
                    freq_options$outcome_measure(),
                    " of ",
                    freq_options$outcome_name(),
                    " when compared against ",
                    data_reactives$reference_component()
                  )
                )
                model_output <- get_freq_model_output(
                  freq_reactives$model(),
                  data_reactives$data_type(),
                  as.logical(
                    as.numeric(
                        freq_options$random_effects()
                    )
                  ),
                  freq_options$summary_measure()
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
          freq_options$update_reactive(),
          freq_options$update_options()
        )

      shiny::observe({
        if (tab() == id) {
          output$warning_sens <- NULL
          output$info_sens <- NULL
          output$model_output_title_sens <- NULL
          output$model_output_sens <- NULL
          is_rendered_sens(FALSE)
          #print(tab())
          shiny::req(
            freq_options$options_loaded(),
            !is.null(freq_sens_reactives$model()),
            cancelOutput = TRUE
          )
          #print("forest_plot_sens")
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
                output$model_output_title_sens <- shiny::renderText(
                  paste0("Model Output showing the ",
                    freq_options$outcome_measure(),
                    " of ",
                    freq_options$outcome_name(),
                    " when compared against ",
                    data_reactives$reference_component()
                  )
                )
                model_output_sens <- get_freq_model_output(
                  freq_sens_reactives$model(),
                  data_reactives$data_type(),
                  as.logical(
                    as.numeric(
                      freq_options$random_effects()
                    )
                  ),
                  freq_options$summary_measure()
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
          freq_options$update_reactive(),
          freq_options$update_options()
        )

    }
  )
}