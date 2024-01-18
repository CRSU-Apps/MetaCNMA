freq_analysis_server <- function(id, freq_options) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      `%>%` <- magrittr::`%>%`

      freq_data <- shiny::reactiveValues()

      freq_data$pairwise <- shiny::reactiveVal(NULL)

    }
  )
}