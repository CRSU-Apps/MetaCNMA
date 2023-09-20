forest_plot_tab_server <- function(id, reactive_data, reactive_freq, tab) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {
      require(dplyr)
      output$outputs <- NULL
      ns <- shiny::NS(id)

      shiny::observe({
        if (tab() == id) {
          output$warning <- NULL
          output$info <- NULL
          print("forest_plot")
          tryCatch({
            withCallingHandlers(
              warning = function(cond) {
                output$warning <- warning_alert(cond) # nolint: object_name
              },
              message = function(cond) {
                output$message <- message_alert(cond) # nolint: object_name
              },
              {
                if (!reactive_data()$valid()) {
                  output$inputs <- default_no_data(ns) # nolint: object_name
                  output$outputs <- NULL
                  return(NULL)
                }
                if (is.null(reactive_freq()$pairwise())) {
                  shiny::withProgress({
                    reactive_freq()$pairwise(
                      freq_pairwise(reactive_data, reactive_freq) # nolint: object_name
                    )
                    reactive_freq()$n_connection(
                      run_net_connection(reactive_freq()$pairwise()) # nolint: object_name
                    )
                  },
                  message = "Formatting Data")
                }
                if (is.null(reactive_freq()$netmeta())) {
                  shiny::withProgress({
                    reactive_freq()$netmeta(
                      run_netmeta(
                        reactive_freq()$pairwise(),
                        ref = get_most_freq_component(
                          reactive_freq()$pairwise()
                        ),
                        random_eff = as.logical(
                          reactive_freq()$random_effects()
                        )
                      )
                    )
                  },
                  message = "Running Network Meta Analysis")
                }
                if (is.null(reactive_freq()$netcomb())) {
                  shiny::withProgress({
                    reactive_freq()$netcomb(
                      run_netcomb( # nolint: object_name
                        reactive_freq()$netmeta(),
                        inactive = get_most_freq_component( # nolint: object_name
                          reactive_freq()$pairwise()
                        )
                      )
                    )
                  },
                  message = "Running Network Meta Analysis")
                }
                if (reactive_freq()$outcome_measure() == "Outcome Measure") {
                  warning("No Outcome Measure Selected")
                }
                output$static_content <- shiny::renderUI(
                  shiny::h4(
                    paste0("Forest plot showing the ",
                      reactive_freq()$outcome_measure(),
                      " of ",
                      reactive_data()$outcome_name(),
                      " when compared against ",
                      get_most_freq_component( # nolint: object_name
                        reactive_freq()$pairwise()
                      )
                    )
                  )
                )
                output$outputs <- shiny::renderUI(
                  render_net_forest( # nolint: object_name
                    reactive_freq()$netcomb(),
                    reactive_data()$data_type(),
                    reactive_freq()$outcome_measure()
                  )
                )
              }
            )
          },
          error = function(e) {
            error_alert(e$message) # nolint: object_name
            invalidate_reactive(reactive_data, reactive_freq) # nolint: object_name
          })
        }
      }) %>% shiny::bindEvent(tab(), input$default_data)

      shiny::observe({
        load_default_data(reactive_data, reactive_freq()) # nolint: object_name
      }) %>% shiny::bindEvent(input$default_data)
    }
  )
}