forest_plot_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Forest Plot"),
    message_tag_list(ns), # nolint: object_usage
    shiny::uiOutput(ns("plot_title")),
    shinycssloaders::withSpinner(
      shiny::plotOutput(ns("forest_plot")),
      type = 6
    )
  )
}

forest_plot_tab_server <- function(id, freq_options, freq_reactives, tab) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      shiny::observe({
        if (tab() == id) {
          output$warning <- NULL
          output$info <- NULL
          output$plot_title <- NULL
          output$forest_plot <- NULL
          print(tab())
          shiny::req(
            freq_options$options_loaded(),
            !is.null(freq_reactives$pairwise()),
            !is.null(freq_reactives$netmeta()),
            !is.null(freq_reactives$netcomb()),
            cancelOutput = TRUE
          )
          print("forest_plot")
          tryCatch({
            withCallingHandlers(
              warning = function(cond) {
                output$warning <- shiny::renderUI(
                  warning_alert(conditionMessage(cond))
                ) # nolint: object_name
              },
              message = function(cond) {
                output$info <- shiny::renderUI(
                  message_alert(conditionMessage(cond))
                ) # nolint: object_name
              },
              {
                print(freq_options$outcome_measure())
                print(freq_options$outcome_name())
                print(get_most_freq_component( # nolint: object_name
                        freq_reactives$pairwise()
                      ))
                output$plot_title <- shiny::renderUI(
                  shiny::h4(
                    paste0("Forest plot showing the ",
                      freq_options$outcome_measure(),
                      " of ",
                      freq_options$outcome_name(),
                      " when compared against ",
                      get_most_freq_component( # nolint: object_name
                        freq_reactives$pairwise()
                      )
                    )
                  )
                )
                output$forest_plot <- shiny::renderPlot(
                  get_net_forest( # nolint: object_name
                    freq_reactives$netcomb(),
                    freq_options$data_type(),
                    freq_options$outcome_measure()
                  )
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
        freq_options$update_reactive()
      )
    }
  )
}