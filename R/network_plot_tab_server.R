network_plot_tab_server <- function(id, reactive_data, reactive_freq, tab) {
  shiny::moduleServer(id,
    function(input,
             output,
             session) {
      ns <- shiny::NS(id)

      require(dplyr)

      shiny::observe({
        output$warning <- NULL
        output$info <- NULL
        output$inputs <- NULL
        output$static_content <- NULL
        output$outputs <- NULL
        if (tab() == id) {
          tryCatch({
            withCallingHandlers(
              warning = function(cond) {
                output$warning <- warning_alert(conditionMessage(cond)) #nolint: object_usage
              },
              message = function(cond) {
                output$message <- message_alert(conditionMessage(cond)) #nolint: object_usage
              },
              {
                if (!reactive_data()$valid()) {
                  output$outputs <- default_no_data(ns) # nolint: object_usage
                } else {
                  if (is.null(reactive_freq()$pairwise())) {
                    shiny::withProgress({
                      reactive_freq()$pairwise(
                        freq_pairwise(reactive_data, reactive_freq) # nolint: object_usage
                      )
                      reactive_freq()$n_connection(
                        run_net_connection(reactive_freq()$pairwise()) # nolint: object_usage
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
                          random_eff = reactive_freq()$random_effects_logical()
                        )
                      )
                    },
                    message = "Running Network Meta Analysis")
                  }
                  output$outputs <- shiny::renderUI(
                    render_vis_graph( # nolint: object_usage
                      reactive_freq()$netmeta(),
                      names(get_combination_components(reactive_freq()$pairwise())) # nolint: object_usage
                    )
                  )
                }
              }
            )
          })
        }
      }) %>% shiny::bindEvent(
        tab(),
        reactive_freq()$valid()
      )

      shiny::observe({
        load_default_data(reactive_data, reactive_freq) # nolint: object_usage
      }) %>% shiny::bindEvent(input$default_data)
    }
  )
}