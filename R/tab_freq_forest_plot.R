forest_plot_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Forest Plot"),
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel(
        "Main Forest Plot",
        save_plot_ui( # nolint: object_name
          ns("save_forest_plot"),
          output_name = "Freq_Forest_Plot"
        ),
        shinydashboardPlus::box(
          title = shiny::textOutput(ns("plot_title")),
          id = ns("forest_plot_box"),
          width = 12,
          shiny::uiOutput(ns("warning")),
          shiny::uiOutput(ns("info")),
          shinycssloaders::withSpinner(
            shiny::plotOutput(ns("forest_plot")),
            type = 6
          )
        )
      ),
      shiny::tabPanel(
        "Sensitivity Analysis Forest Plot",
        save_plot_ui( # nolint: object_name
          ns("save_forest_plot_sens"),
          output_name = "Freq_Forest_Plot_Sensitivity"
        ),
        shinydashboardPlus::box(
          title = shiny::textOutput(ns("plot_title_sens")),
          id = ns("forest_plot_box_sens"),
          width = 12,
          shiny::uiOutput(ns("warning_sens")),
          shiny::uiOutput(ns("info_sens")),
          shinycssloaders::withSpinner(
            shiny::plotOutput(ns("forest_plot_sens")),
            type = 6
          )
        )
      )
    )
  )
}

forest_plot_tab_server <- function(
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
          output$plot_title <- NULL
          output$forest_plot <- NULL
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
                output$plot_title <- shiny::renderText(
                  paste0("Forest plot showing the ",
                    freq_options$outcome_measure(),
                    " of ",
                    freq_options$outcome_name(),
                    " when compared against ",
                    data_reactives$reference_component()
                  )
                )
                forest_plot <- function() {
                  get_net_forest( # nolint: object_name
                    freq_reactives$model(),
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

      shiny::observe({
        if (tab() == id) {
          output$warning_sens <- NULL
          output$info_sens <- NULL
          output$plot_title_sens <- NULL
          output$forest_plot_sens <- NULL
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
                output$plot_title_sens <- shiny::renderText(
                  paste0("Forest plot showing the ",
                    freq_options$outcome_measure(),
                    " of ",
                    freq_options$outcome_name(),
                    " when compared against ",
                    data_reactives$reference_component()
                  )
                )
                forest_plot_sens <- function() {
                  get_net_forest( # nolint: object_name
                    freq_sens_reactives$model(),
                    freq_options$data_type(),
                    freq_options$outcome_measure()
                  )
                }
                output$forest_plot_sens <- shiny::renderPlot(
                  forest_plot_sens()
                )
                is_rendered_sens(TRUE)
                save_plot_server( # nolint: object_name
                  "save_forest_plot_sens",
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
        freq_options$update_reactive(),
        freq_options$update_options()
      )

    }
  )
}