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

      freq_reactives$formatted_data <- shiny::reactive({
        if (!data_reactives$is_data_loaded()) {
          return(NULL)
        } else {
          return(
            format_data( # nolint: object_usage
              data_reactives$data(),
              data_reactives$data_type()
            )
          )
        }
      })

      freq_reactives$is_data_formatted <- shiny::reactive({
        return(!is.null(freq_reactives$formatted_data()))
      })

      freq_reactives$studies <- shiny::reactive({
        if (is.null(freq_reactives$is_data_formatted())) {
          return(NULL)
        } else {
          return(
            levels(as.factor(freq_reactives$formatted_data()$study))
          )
        }
      })

      freq_reactives$pairwise <- shiny::reactive({
        if (!freq_reactives$is_data_formatted()) {
          return(NULL)
        } else {
          return(
            freq_pairwise( # nolint: object_usage
              freq_reactives$formatted_data(),
              data_reactives$data_type()
            )
          )
        }
      })

      freq_reactives$netconnection <- shiny::reactive({
        if (is.null(freq_reactives$pairwise())) {
          return(NULL)
        } else {
          return(
            run_net_connection( # nolint: object_usage
              freq_reactives$pairwise()
            )
          )
        }
      })

      freq_reactives$reference_component <- shiny::reactive({
        if (is.null(freq_reactives$pairwise())) {
          return(NULL)
        } else {
          return(
            get_most_freq_component( # nolint: object_usage
              freq_reactives$pairwise()
            )
          )
        }
      })

      freq_reactives$netmeta <- shiny::reactive({
        if (
          any(
            is.null(freq_reactives$reference_component()),
            !freq_options$options_loaded()
          )
        ) {
          return(NULL)
        } else {
          return(
            run_netmeta( # nolint: object_usage
              freq_reactives$pairwise(),
              ref = freq_reactives$reference_component(),
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
              inactive = freq_reactives$reference_component()
            )
          )
        }
      })

      return(freq_reactives)

    }
  )
}