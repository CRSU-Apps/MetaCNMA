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

        output$outcome_measure <- shiny::renderText("No Data Loaded")
        output$desirable <- NULL
        output$random_effects <- NULL
        output$outcome_name <- NULL

        shiny::req(
          !is.null(data()),
          !is.null(data_type()),
          !is.null(is_default_data()),
          cancelOutput = TRUE
        )

        print("Rendering outcome measure radio")

        print(data_type())

        output$outcome_measure <- shiny::renderUI(
          shiny::radioButtons(
            ns("outcome_measure"),
            "Select an outcome measure:",
            choices = if (data_type() == "binary") {
              c("Odds Ratio (OR)" = "or",
                "Risk Ratio (RR)" = "rr",
                "Risk Difference (RD)" = "rd")
            } else {
              c("Mean Difference (MD)" = "md",
                "Standardised Mean Difference (SMD)" = "smd")
            } ,
            selected = default_outcome_measure(data_type(), is_default_data())
          )
        )

        output$desirable <- shiny::renderUI(
          shiny::radioButtons(
            ns("desirable"),
            default_desirable_text(data_type()),
            c("Desirable" = 1,
              "Undesirable" = 0),
            selected = default_outcome_desirable(data_type(), is_default_data())
          )
        )

        output$random_effects <- shiny::renderUI(
          shiny::radioButtons(
            ns("random_effects"),
            "The model should use:",
            c("Fixed Effects" = 0,
              "Random Effects" = 1),
            selected = 0
          )
        )

        output$outcome_name <- shiny::renderUI(
          shiny::textInput(
            ns("outcome_name"),
            "Outcome Name",
            value = default_outcome_name(data_type(), is_default_data())
          )
        )

      }) %>% shiny::bindEvent(
        update_reactive()
      )


    }
  )
}