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

      bayesian_reactives <- shiny::reactiveValues()

      bayesian_reactives$model <- shiny::reactiveVal(NULL)

      bayesian_reactives$console_out <- shiny::reactiveVal(NULL)

      bayesian_reactives$exclude_df <- shiny::reactiveVal(NULL)

      bayesian_reactives$is_model_run <- shiny::reactive({
        return(!is.null(bayesian_reactives$model()))
      })

      return(bayesian_reactives)

    }
  )
}