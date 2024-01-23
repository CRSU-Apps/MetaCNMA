freq_analysis_server <- function(id, data_reactives, freq_options) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      `%>%` <- magrittr::`%>%`

      freq_reactives <- shiny::reactiveValues()

      freq_reactives$formatted_data <- shiny::reactive({
        if (!data_reactives$is_data_loaded()) {
          return(NULL)
        } else {
          return(format_data(data_reactives$data(), data_reactives$data_type()))
        }
      })

      freq_reactives$is_data_formatted <- shiny::reactive({
        return(!is.null(freq_reactives$formatted_data()))
      })

      freq_reactives$pairwise <- shiny::reactive({
        if (!freq_reactives$is_data_formatted()) {
          return(NULL)
        } else {
          return(
            freq_pairwise(
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
            run_net_connection(
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
            get_most_freq_component(
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
            run_netmeta(
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

      return(freq_reactives)

    }
  )
}