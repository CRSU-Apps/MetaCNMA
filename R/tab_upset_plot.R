upset_plot_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("UpSet Plot"),
    message_tag_list(ns), # nolint: object_name
    save_plot_ui( # nolint: object_name
      ns("save_upset_plot"),
      output_name = "UpSet_Plot",
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
          #print(tab())
          shiny::req(
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
                    get_upset_plot( # nolint: object_name
                      data_reactives$formatted_data(),
                      data_reactives$components() # nolint: object_name
                    )
                  )
                }
                output$plot_title <- shiny::renderText(
                  paste0(
                    "UpSet Plot of Component: ",
                    paste(
                      data_reactives$components(),
                      collapse = ", "
                    )
                  )
                )
                output$upset_plot <- shiny::renderPlot(
                  upset_plot(),
                  width = 1200,
                  height = 675
                )
                is_rendered(TRUE)
                save_plot_server("save_upset_plot", # nolint: object_name
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