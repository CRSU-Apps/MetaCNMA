freq_outcome_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Model Settings"),
    message_tag_list(ns), # nolint: object_usage
    shiny::uiOutput(ns("outcome_measure")),
    shiny::uiOutput(ns("desirable")),
    shiny::uiOutput(ns("random_effects")),
    shiny::uiOutput(ns("outcome_name")),
  )
}

freq_outcome_tab_server <- function(id, data, data_type, is_default_data, tab) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      freq_options <- shiny::reactiveValues()

      update_reactive <- shiny::reactive({
        data()
        data_type()
        is_default_data()
      })

      shiny::observe({
        print("Waiting on data_type and is_default_data")

        output$outcome_measure <- NULL

        shiny::req(
          !is.null(data()),
          !is.null(data_type()),
          !is.null(is_default_data()),
          cancelOutput = TRUE
        )

        print("Rendering outcome measure radio")

        selected_outcome_measure <-
          default_outcome_measure(data_type(), is_default_data())

        print(data_type())

        output$outcome_measure <- shiny::renderUI(shiny::radioButtons(
          ns("outcome_measure"),
          "Select an outcome measure:",
          # {
          #   if (data_type == "binary") {
          #     return(
          #       c("Odds Ratio (OR)" = "or",
          #         "Risk Ratio (RR)" = "rr",
          #         "Risk Difference (RD)" = "rd")
          #     )
          #   } else {
          #     return(
          #       c("Mean Difference (MD)" = "md",
          #         "Standardised Mean Difference (SMD)" = "smd")
          #     )
          #   }
          # }
          ifelse(data_type() == "binary",
            (c("Odds Ratio (OR)" = "or",
               "Risk Ratio (RR)" = "rr",
               "Risk Difference (RD)" = "rd")),
            (c("Mean Difference (MD)" = "md",
               "Standardised Mean Difference (SMD)" = "smd")),
          ),
          selected = selected_outcome_measure
        ))
      }) %>% shiny::bindEvent(
        update_reactive()
      )


    }
  )
}