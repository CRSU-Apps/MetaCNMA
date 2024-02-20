bayesian_analysis_server <- function( # nolint: cyclocomp_linter.
  id,
  data_reactives,
  bayesian_options
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      `%>%` <- magrittr::`%>%`

      bayesian_reactives <- shiny::reactiveValues()

      bayesian_reactives$update_reactive <- shiny::reactive(
        bayesian_options$update_reactive()
      )

      bayesian_reactives$model <- shiny::reactiveVal(NULL)

      bayesian_reactives$console_out <- shiny::reactiveVal(NULL)

      bayesian_reactives$exclude_df <- shiny::reactiveVal(NULL)

      bayesian_reactives$is_model_run <- shiny::reactive({
        return(!is.null(bayesian_reactives$model()))
      })

      shiny::observe({
        bayesian_reactives$model(NULL)
      }) %>% shiny::bindEvent(
        bayesian_reactives$update_reactive()
      )

      return(bayesian_reactives)

    }
  )
}