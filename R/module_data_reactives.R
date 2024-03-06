data_reactives_server <- function(
  id,
  data_type
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      ns <- session$ns

      `%>%` <- magrittr::`%>%`

      parent_session <- shiny::getDefaultReactiveDomain()$rootScope()

      data_reactives <- shiny::reactiveValues()

      # Create a definable reactive value to allow reloading of data
      data_reactives$reload <- shiny::reactiveVal(FALSE)

      # Use a counter in a reactiveVal to allow data to be invalidated
      data_reactives$invalidate_count <- shiny::reactiveVal(0)

      data_reactives$data <- shiny::reactiveVal(NULL)
      data_reactives$is_default_data <- shiny::reactiveVal(NULL)

      data_reactives$data_type <- data_type

      data_reactives$is_data_loaded <- shiny::reactive({
        return(
          !is.null(data_reactives$data())
        )
      })

      data_reactives$formatted_data <- shiny::reactive({
        if (!data_reactives$is_data_loaded()) {
          return(NULL)
        } else {
          return(
            format_data( # nolint: object_name
              data_reactives$data(),
              data_reactives$data_type()
            )
          )
        }
      })

      data_reactives$is_data_formatted <- shiny::reactive({
        return(!is.null(data_reactives$formatted_data()))
      })

      data_reactives$studies <- shiny::reactive({
        if (is.null(data_reactives$is_data_formatted())) {
          return(NULL)
        } else {
          return(
            levels(as.factor(data_reactives$formatted_data()$study))
          )
        }
      })

      data_reactives$components <- shiny::reactive({
        if (is.null(data_reactives$is_data_formatted())) {
          return(NULL)
        } else {
          return(
            get_components(data_reactives$formatted_data())
          )
        }
      })

      data_reactives$default_reference_component <- shiny::reactive({
        if (is.null(data_reactives$is_data_formatted())) {
          return(NULL)
        } else {
          return(
            get_most_freq_component(data_reactives$formatted_data())
          )
        }
      })

      data_reactives$reference_component <- shiny::reactive({
        return(data_reactives$default_reference_component())
      })

      data_reactives$components_no_reference <- shiny::reactive({
        if (is.null(data_reactives$is_data_formatted())) {
          return(NULL)
        } else {
          return(
            get_components_no_reference(
              data_reactives$formatted_data(),
              data_reactives$default_reference_component()
            )
          )
        }
      })

      data_reactives$pairwise <- shiny::reactive({
        if (!data_reactives$is_data_formatted()) {
          return(NULL)
        } else {
          return(
            freq_pairwise( # nolint: object_name
              data_reactives$formatted_data(),
              data_reactives$data_type()
            )
          )
        }
      })

      return(data_reactives)

    }
  )
}