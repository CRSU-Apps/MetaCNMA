freq_analysis_server <- function( # nolint: cyclocomp_linter.
  id,
  data_reactives,
  freq_options
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      freq_reactives <- shiny::reactiveValues()

      freq_reactives$exclude_df <- shiny::reactiveVal(NULL)

      freq_reactives$netconnection <- shiny::reactive({
        if (is.null(data_reactives$pairwise())) {
          return(NULL)
        } else {
          return(
            run_net_connection( # nolint: object_usage
              data_reactives$pairwise()
            )
          )
        }
      })

      freq_reactives$netmeta <- shiny::reactive({
        if (
          any(
            is.null(data_reactives$default_reference_component()),
            !freq_options$options_loaded()
          )
        ) {
          return(NULL)
        } else {
          return(
            run_netmeta( # nolint: object_usage
              data_reactives$pairwise(),
              ref = data_reactives$default_reference_component(),
              random_eff = as.logical(
                as.numeric(
                  freq_options$random_effects()
                )
              )
            )
          )
        }
      })

      freq_reactives$netcomb <- shiny::reactive({
        if (
          is.null(freq_reactives$netmeta())
        ) {
          return(NULL)
        } else {
          return(
            run_netcomb( # nolint: object_usage
              freq_reactives$netmeta(),
              inactive = data_reactives$default_reference_component()
            )
          )
        }
      })

      return(freq_reactives)

    }
  )
}