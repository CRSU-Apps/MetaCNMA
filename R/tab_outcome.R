model_outcome_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Model Settings"),
    message_tag_list(ns), # nolint: object_usage
    shiny::uiOutput(ns("outcome_measure")),
    shiny::uiOutput(ns("reference_component")),
    shiny::uiOutput(ns("desirable")),
    shiny::uiOutput(ns("random_effects")),
    shiny::uiOutput(ns("outcome_name")),
  )
}

model_outcome_tab_server <- function(
  id,
  data_reactives,
  tab,
  freq = TRUE
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      model_options <- shiny::reactiveValues()

      model_options$update_reactive <- shiny::reactive({
        data_reactives$data()
        data_reactives$data_type()
        data_reactives$is_default_data()
      })

      shiny::observe({
        print("Waiting on data_type and is_default_data")

        output$outcome_measure <- shiny::renderText("No Data Loaded")
        output$desirable <- NULL
        output$random_effects <- NULL
        output$outcome_name <- NULL

        shiny::req(
          !is.null(data_reactives$data()),
          !is.null(data_reactives$data_type()),
          !is.null(data_reactives$is_default_data()),
          cancelOutput = TRUE
        )

        print("Rendering outcome measure radio")

        print(data_reactives$data_type())

        output$outcome_measure <- shiny::renderUI(
          shiny::radioButtons(
            ns("outcome_measure"),
            "Select an outcome measure:",
            choices = if (data_reactives$data_type() == "binary") {
              c("Odds Ratio (OR)" = "or",
                "Risk Ratio (RR)" = "rr",
                "Risk Difference (RD)" = "rd")
            } else {
              c("Mean Difference (MD)" = "md",
                "Standardised Mean Difference (SMD)" = "smd")
            } ,
            selected = default_outcome_measure(
              data_reactives$data_type(),
              data_reactives$is_default_data()
            )
          )
        )

        shiny::outputOptions(
          output,
          "outcome_measure",
          suspendWhenHidden = FALSE
        )

        output$desirable <- shiny::renderUI(
          shiny::radioButtons(
            ns("desirable"),
            default_desirable_text(data_reactives$data_type()),
            c("Desirable" = 1,
              "Undesirable" = 0),
            selected = default_outcome_desirable(
              data_reactives$data_type(),
              data_reactives$is_default_data()
            )
          )
        )

        shiny::outputOptions(
          output,
          "desirable",
          suspendWhenHidden = FALSE
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

        shiny::outputOptions(
          output,
          "random_effects",
          suspendWhenHidden = FALSE
        )

        output$outcome_name <- shiny::renderUI(
          shiny::textInput(
            ns("outcome_name"),
            "Outcome Name",
            value = default_outcome_name(
              data_reactives$data_type(),
              data_reactives$is_default_data()
            )
          )
        )

        shiny::outputOptions(output, "outcome_name", suspendWhenHidden = FALSE)

      }) %>% shiny::bindEvent(
        model_options$update_reactive()
      )

      model_options$data_type <- shiny::reactive({
        return(data_reactives$data_type())
      })

      model_options$original_outcome_measure <- shiny::reactive({
        return(input$outcome_measure)
      })

      model_options$desirable <- shiny::reactive({
        return(input$desirable)
      })

      model_options$outcome_measure <- shiny::reactive({
        return(
          get_outcome_measure(
            input$outcome_measure
          )
        )
      })

      model_options$random_effects <- shiny::reactive({
        return(as.logical(as.numeric(input$random_effects)))
      })

      .outcome_name <- shiny::reactiveVal(NULL)

      shiny::observe({
        if (tab() == id) {
          shiny::invalidateLater(3000, session)
          .outcome_name(shiny::isolate(input$outcome_name))
        } else {
          .outcome_name(shiny::isolate(input$outcome_name))
        }
      }) %>% shiny::bindEvent(input$outcome_name, ignoreInit = TRUE)

      model_options$outcome_name <- shiny::reactive({
        .outcome_name()
      })

      model_options$options_loaded <- shiny::reactive({
        if (
          any(
            is.null(input$outcome_measure),
            is.null(input$desirable),
            is.null(input$random_effects),
            is.null(model_options$outcome_name())
          )
        ) {
          print("Options Not Loaded")
          return(FALSE)
        } else {
          print("Options Loaded")
          return(TRUE)
        }
      })

      shiny::observe({
        if (all(
          tab() == id,
          !freq
        )) {
          shinyjs::delay(
            5,
            shinyjs::disable(
              selector =
                paste0(
                  "#",
                  ns("outcome_measure"),
                  " [type='radio'][value='smd']"
                )
            )
          )
        }
      }) %>% shiny::bindEvent(tab(), model_options$update_reactive())

      model_options$update_options <- shiny::reactive({
        input$outcome_measure
        input$desirable
        input$random_effects
      })

      return(model_options)


    }
  )
}