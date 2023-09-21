freq_outcome_tab_server <- function(id, reactive_data, reactive_freq, tab) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {
      require(dplyr)
      ns <- shiny::NS(id)

      output$inputs <- NULL
      output$outputs <- NULL
      shiny::outputOptions(output, "inputs", suspendWhenHidden = FALSE)

      shiny::observe({
        print("data change")
        if (! reactive_data()$valid()) {
          print("Data not valid")
          output$inputs <- default_no_data(ns) # nolint object_usage
        } else {
          input_options <- shiny::tagList()

          if (reactive_data()$data_type() == "continuous") {
            input_options <- shiny::tagAppendChild(
              input_options,
              shiny::radioButtons(
                ns("outcome"),
                "Select an outcome measure:",
                c("Mean Difference (MD)" = "md",
                  "Standardised Mean Difference (SMD)" = "smd"),
                selected = ifelse(
                  !is.null(reactive_freq()$measure()),
                  reactive_freq()$measure(), "md"
                )
              )
            )
            input_options <- shiny::tagAppendChild(
              input_options,
              shiny::radioButtons(
                ns("desirable"),
                "For treatment rankings a smaller 
                outcome value (MD / SMD) is:",
                c("Desirable" = 1,
                  "Undesirable" = 0),
                selected = ifelse(
                  !is.null(reactive_data()$desirable()),
                  reactive_data()$desirable(), 1
                )
              )
            )
          } else if (reactive_data()$data_type() == "binary") {
            input_options <- shiny::tagAppendChild(
              input_options,
              shiny::radioButtons(
                ns("outcome"),
                "Select an outcome measure:",
                c("Odds Ratio (OR)" = "or",
                  "Risk Ratio (RR)" = "rr",
                  "Risk Difference (RD)" = "rd"),
                selected = ifelse(
                  !is.null(reactive_freq()$measure()),
                  reactive_freq()$measure(), "or"
                )
              )
            )
            input_options <- shiny::tagAppendChild(
              input_options,
              shiny::radioButtons(
                ns("desirable"),
                "For treatment rankings an outcome value (OR / RR / RR) 
                less than 1 is:",
                c("Desirable" = 1,
                  "Undesirable" = 0),
                selected = ifelse(
                  !is.null(reactive_data()$desirable()),
                  as.numeric(reactive_data()$desirable()), 0
                )
              )
            )
          }
          input_options <- shiny::tagAppendChild(
            input_options,
            shiny::radioButtons(
              ns("random_effects"),
              "For treatment rankings an outcome value (OR / RR / RR) 
              less than 1 is:",
              c("Fixed Effects" = 0,
                "Random Effects" = 1),
              selected = ifelse(
                !is.null(reactive_freq()$random_effects()),
                as.numeric(reactive_freq()$random_effects()), 0
              )
            )
          )
          input_options <- shiny::tagAppendChild(
            input_options,
            shiny::textInput(
              ns("outcome_name"),
              "Outcome Name",
              value = ifelse(
                !is.null(reactive_data()$outcome_name()),
                reactive_data()$outcome_name(), ""
              )
            )
          )
          output$inputs <- shiny::renderUI(input_options)
        }
      }) %>% shiny::bindEvent(
        reactive_data()$valid()
      )

      outcome_name <- shiny::reactiveValues()
      shiny::observe({
        if (tab() == id) {
          shiny::invalidateLater(3000, session)
          outcome_name$text <- shiny::isolate(input$outcome_name)
        }
      }) %>% shiny::bindEvent(input$outcome_name, ignoreInit = TRUE)

      shiny::observe({
        # Set defaults for default data
        print("setting outcome measure")
        reactive_freq()$measure(input$outcome)
        print(paste0("Outcome Measure Set to ", reactive_freq()$measure()))
        print("setting desirable")
        reactive_data()$desirable(input$desirable)
        print("setting random effects")
        reactive_freq()$random_effects(input$random_effects)
        print(
          paste0(
            "Random Effects Set to ",
            reactive_freq()$random_effects()
          )
        )
        reactive_data()$outcome_name(input$outcome_name)
        print("setting outcome name")
        print(paste0("Outcome Name Set to ", reactive_data()$outcome_name()))
        reactive_freq()$valid(FALSE)
        print(reactive_data())
      }) %>% shiny::bindEvent(
        input$outcome,
        input$desirable,
        input$random_effects,
        reactive_data()$valid(),
        outcome_name$text,
        ignoreNULL = TRUE
      )

      shiny::observe({
        load_default_data(reactive_data(), reactive_freq()) # nolint object_usage
      }) %>% shiny::bindEvent(input$default_data)
    }
  )
}