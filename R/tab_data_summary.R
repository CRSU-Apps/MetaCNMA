data_summary_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Data Summary"),
    default_data_ui("summary_default_data"),
    message_tag_list(ns), # nolint: object_usage
    shinycssloaders::withSpinner(
      shiny::uiOutput(ns("data_summary")),
      type = 6
    )
  )
}

data_summary_tab_server <- function(
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

      default_data_server(
        ns("summary_default_data"),
        data_reactives
      )

      shiny::observe({
        if (tab() == id) {
          print(tab())
          output$warning <- NULL
          output$info <- NULL
          output$data_summary <- shiny::renderText("Waiting for data")
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
                if (!is.null(freq_reactives$netconnection())) {
                  pw_summary <- get_summary(data_reactives$pairwise()) # nolint: object_name
                  output$data_summary <- shiny::renderUI(
                    shiny::tagList(
                      shiny::tags$ul(
                        shiny::tags$li(paste0("Number of Studies: ",
                          pw_summary$n_studies
                        )),
                        shiny::tags$li(paste0("Number of Components: ",
                          length(pw_summary$components)
                        )),
                        shiny::tags$li(paste0("Components: ",
                          paste0(pw_summary$components, collapse = ", ")
                        )),
                        shiny::tags$li(paste0("Is the Network Connected: ",
                          !freq_reactives$netconnection()$details.disconnected
                        ))
                      ),
                      DT::renderDataTable(
                        component_summary_as_df(
                          pw_summary$combination_components
                        ),
                        filter = "top",
                        options = list(scrollX = TRUE,
                          pageLength = 10,
                          info = FALSE,
                          lengthMenu = list(c(10, -1), c("10", "All"))
                        )
                      )
                    )
                  )
                }
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