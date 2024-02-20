upset_plot_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Upset Plot"),
    message_tag_list(ns), # nolint: object_usage
    save_plot_ui( # nolint: object_usage
      ns("save_upset_plot"),
      output_name = "Upset_Plot",
      height = "675"
    ),
    shinydashboardPlus::box(
      title = shiny::textOutput(ns("plot_title")),
      id = ns("upset_plot_box"),
      width = 12,
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns("upset_plot")),
        type = 6
      )
    )
  )
}
upset_plot_tab_server <- function(
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

      is_rendered <- shiny::reactiveVal(FALSE)

      shiny::observe({
        if (tab() == id) {
          output$warning <- NULL
          output$info <- NULL
          output$plot_title <- NULL
          output$upset_plot <- NULL
          is_rendered(FALSE)
          print(tab())
          shiny::req(
            !is.null(data_reactives$pairwise()),
            data_reactives$is_data_formatted(),
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
                upset_plot <- function() {
                  print(
                    get_upset_plot( # nolint: object_usage
                      data_reactives$formatted_data(),
                      get_components_no_reference(data_reactives$pairwise()) # nolint: object_usage
                    )
                  )
                }
                output$plot_title <- shiny::renderText(
                  paste0(
                    "Upset Plot of Component: ",
                    paste(
                      get_components_no_reference( # nolint: object_name# nolint: object_name
                        data_reactives$pairwise()
                      ),
                      collapse = ", "
                    )
                  )
                )
                output$upset_plot <- shiny::renderPlot(
                  upset_plot()
                )
                is_rendered(TRUE)
                save_plot_server("save_upset_plot", # nolint: object_usage
                  upset_plot,
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
        freq_options$update_reactive()
      )
    }
  )
}