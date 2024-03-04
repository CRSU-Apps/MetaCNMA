network_plot_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Network Diagram"),
    message_tag_list(ns), # nolint: object_name
    shinycssloaders::withSpinner(shiny::uiOutput(ns("vis_network")), type = 6)
  )
}

network_plot_tab_server <- function(
  id,
  data_reactives,
  freq_options,
  freq_reactives,
  tab
) {
  shiny::moduleServer(id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      shiny::observe({
        if (tab() == id) {
          output$warning <- NULL
          output$info <- NULL
          output$vis_network <- NULL
          print(tab())
          shiny::req(
            freq_options$options_loaded(),
            !is.null(data_reactives$pairwise()),
            !is.null(freq_reactives$model()),
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
                  message_alert(conditionMessage(cond)) # nolint: object_name
                )
              },
              {
                output$vis_network <- shiny::renderUI(
                  vis_network_ui(ns("vis_network_1"))
                )
                vis_network_server(
                  "vis_network_1",
                  freq_reactives$model(),
                  names(
                    get_combination_components(data_reactives$pairwise())
                  )
                )
              }
            )
          })
        }
      }) %>% shiny::bindEvent(
        tab(),
        freq_options$update_reactive()
      )
    }
  )
}