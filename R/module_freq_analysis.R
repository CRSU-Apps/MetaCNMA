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

      freq_reactives$pairwise <- shiny::reactive(
        if (
          !freq_options$options_loaded() ||
            !data_reactives$is_data_formatted() ||
            !check_outcome_measure(
              data_reactives$data_type(),
              freq_options$summary_measure()
            ) ||
            !check_reference_component(
              data_reactives$reference_component(),
              data_reactives$components()
            )
        ) {
          return(NULL)
        } else {
          return(
            freq_pairwise( # nolint: object_name
              data_reactives$formatted_data(),
              data_reactives$data_type(),
              freq_options$summary_measure()
            )
          )
        }
      )

      freq_reactives$netconnection <- shiny::reactive({
        if (is.null(freq_reactives$pairwise())) {
          return(NULL)
        } else {
          return(
            run_net_connection( # nolint: object_name
              freq_reactives$pairwise()
            )
          )
        }
      })

      freq_reactives$is_network_connected <- shiny::reactive({
        if (is.null(freq_reactives$netconnection())) {
          return(NULL)
        } else {
          return(is_connected(freq_reactives$netconnection()))
        }
      })

      freq_reactives$model <- shiny::reactive(
        if (
          is.null(freq_reactives$is_network_connected())
          || is.null(data_reactives$reference_component())
        ) {
          return(NULL)
        } else {
          #print(data_reactives$reference_component())
          return(
            run_freq( # nolint: object_name
              freq_reactives$pairwise(),
              freq_reactives$is_network_connected(),
              ref = data_reactives$reference_component(),
              random_eff = as.logical(
                as.numeric(
                  freq_options$random_effects()
                )
              ),
              freq_options$summary_measure()
            )
          )
        }
      )

      return(freq_reactives)

    }
  )
}