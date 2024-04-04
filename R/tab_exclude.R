exclude_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("Exclude Studies (Sensitivity Analysis)"),
    message_tag_list(ns), # nolint: object_name
    shiny::tabsetPanel(
      type = "tabs",
      shiny::tabPanel(
        "Exclude Studies",
        shiny::br(),
        shinydashboardPlus::box(
          title = "Original Data",
          id = ns("original_studies_box"),
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          data_table_module_ui(ns("original_table_output")) # nolint: object_name
        ),
        shiny::div(class = "clearfix"),
        shinycssloaders::withSpinner(
          shiny::uiOutput(ns("study_selection")),
          type = 6
        )
      ),
      shiny::tabPanel(
        "Excluded Studies: Data Summary",
        shinycssloaders::withSpinner(
          data_table_module_ui(ns("excluded_studies_table_output")), # nolint: object_name
          type = 6
        )
      )
    )
  )
}

exclude_tab_server <- function(
  id,
  data_reactives,
  model_options,
  tab
) {
  shiny::moduleServer(id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      exclude_reactives <- data_reactives_server(
        ns("exclude_reactives"),
        shiny::reactive(data_reactives$data_type())
      )

      exclude_reactives$reference_component <- shiny::reactive({
        data_reactives$reference_component()
      })

      .reactive_df <- shiny::reactiveVal(NULL)
      .reactive_exclude_df <- shiny::reactiveVal(NULL)

      data_table_module_server(
        "original_table_output",
        .reactive_df
      )

      data_table_module_server(
        "excluded_studies_table_output",
        .reactive_exclude_df
      )

      .excluded <- shiny::reactiveVal(NULL)

      shiny::observe({
        if (tab() == id) {
          output$warning <- NULL
          output$info <- NULL
          output$data_summary <- NULL
          .reactive_df(NULL)
          print(tab())
          shiny::req(
            !is.null(data_reactives$studies()),
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
                # Setting Studies
                output$study_selection <- shiny::renderUI(
                  shiny::checkboxGroupInput(ns("excluded"),
                    label = "Choose any Studies you wish to exclude",
                    choices = data_reactives$studies(),
                    selected = .excluded()
                  )
                )
                .reactive_df(data_reactives$formatted_data())
              }
            )
          },
          error = function(e) {
            print("this error occured trying to render the studies")
            error_alert(e$message) # nolint: object_name
          })
        }
      }) %>% shiny::bindEvent(
        tab(),
        model_options$update_reactive()
      )

      .excluded_studies <- shiny::reactiveVal(NULL)

      shiny::observe({
        if (tab() == id) {
          shiny::invalidateLater(3000, session)
          .excluded_studies(shiny::isolate(input$exclude))
        } else {
          .excluded_studies(shiny::isolate(input$exclude))
        }
      }) %>% shiny::bindEvent(input$exclude, ignoreInit = TRUE)

      shiny::observe({
        exclude_reactives$data(NULL)
        .excluded(NULL)
      }) %>% shiny::bindEvent(
        model_options$update_reactive()
      )

      shiny::observe({
        if (tab() == id) {
          shiny::invalidateLater(3000, session)
          .excluded(shiny::isolate(input$excluded))
        } else {
          .excluded(shiny::isolate(input$excluded))
        }
      }) %>% shiny::bindEvent(input$excluded, ignoreInit = TRUE)

      exclude_reactives$excluded <- shiny::reactive({
        .excluded()
      })

      shiny::observe({
        exclude_reactives$data(NULL)
        print(exclude_reactives$excluded())
        df <- shiny::isolate(data_reactives$formatted_data())
        exclude_reactives$data(
          df[!df$study %in% exclude_reactives$excluded(), ]
        )
        .reactive_exclude_df(exclude_reactives$formatted_data())
      }) %>% shiny::bindEvent(
        exclude_reactives$excluded(),
        ignoreInit = TRUE
      )

      return(exclude_reactives)

    }
  )
}