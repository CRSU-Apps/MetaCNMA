#' @title Data Reactives Server
#' @description Shiny Module for Data Reactives
#' @param id Module ID
#' @param data_type reactive of data_type see module data_type
#' @return reactiveVals for data
#' @details Various Reacive functionality for data
#' @examples
#' \dontrun{
#' if(interactive()){
#'    data_reactives_server(
#'      "data_reactives",
#'      data_type
#'    )
#'  }
#' }
#' @seealso 
#'  \code{\link[shiny]{moduleServer}},
#' \code{\link[shiny]{reactiveValues}},
#' \code{\link[shiny]{reactiveVal}},
#' \code{\link[shiny]{reactive}}
#' @rdname data_reactives_server
#' @importFrom shiny moduleServer reactiveValues reactiveVal reactive
data_reactives_server <- function( # nolint: cyclocomp_linter.
  id,
  data_type
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      # Setup a reactiveValues for the data reactives
      data_reactives <- shiny::reactiveValues()

      # Create a definable reactive value to allow reloading of data
      data_reactives$reload <- shiny::reactiveVal(FALSE)

      # Use a counter in a reactiveVal to allow data to be invalidated
      data_reactives$invalidate_count <- shiny::reactiveVal(0)

      # Forward declare the data reactive (where the data.frame will be stored)
      data_reactives$data <- shiny::reactiveVal(NULL)
      # Forward declare the default data indicator
      data_reactives$is_default_data <- shiny::reactiveVal(NULL)

      # set the data_type as a reactive from function parameters
      # see module_data_type
      data_reactives$data_type <- data_type

      # Forward declare the reference component
      data_reactives$reference_component <- shiny::reactiveVal(NULL)

      # Reactive to check if the data is loaded (not NULL)
      data_reactives$is_data_loaded <- shiny::reactive({
        return(
          !is.null(data_reactives$data())
        )
      })

      # Rective tor return formatted data
      # Returns NULL if the data is not loaded
      data_reactives$formatted_data <- shiny::reactive({
        # Return NULL if data is not loaded
        if (!data_reactives$is_data_loaded()) {
          return(NULL)
        } else {
          # Call data to update reactive
          data_reactives$data()
          # Return the formatted data
          return(
            format_data( # nolint: object_name
              data_reactives$data(),
              data_reactives$data_type()
            )
          )
        }
      })

      # Reactive to check if the data is formatted (not NULL)
      data_reactives$is_data_formatted <- shiny::reactive({
        return(!is.null(data_reactives$formatted_data()))
      })

      # Reactive to return the study names
      # Returns NULL if data is not formatted / loaded
      data_reactives$studies <- shiny::reactive({
        # Return NULL if the data is not formatted
        # Waterfalls to data loaded
        if (is.null(data_reactives$is_data_formatted())) {
          return(NULL)
        } else {
          # Return the study names
          return(
            levels(as.factor(data_reactives$formatted_data()$study))
          )
        }
      })

      # Reactive to return the components
      # Returns NULL if data is not formatted / loaded
      data_reactives$components <- shiny::reactive({
        # Return NULL if the data is not formatted
        # Waterfalls to data loaded
        if (is.null(data_reactives$is_data_formatted())) {
          return(NULL)
        } else {
          # Return the components
          return(
            get_components(data_reactives$formatted_data()) # nolint: object_name
          )
        }
      })

      # Reactive to return the default reference component
      # If using a default data set this is preset
      # Else will return the most frequent component
      # Returns NULL if data is not formatted / loaded
      data_reactives$default_reference_component <- shiny::reactive({
        # Return NULL if the data is not formatted
        # Waterfalls to data loaded
        if (is.null(data_reactives$is_data_formatted())) {
          return(NULL)
        }
        # If a default dataset
        if (data_reactives$is_default_data()) {
          # Return preset reference component see default_elements.R
          return(
            default_reference_component(data_reactives$data_type()) # nolint: object_name
          )
        } else {
          # Otherwise return the most frequent component see utils.R
          return(
            get_most_freq_component(data_reactives$formatted_data()) # nolint: object_name
          )
        }
      })

      # Reacive to return a list of compents without the reference component
      # Returns NULL if the reference component has not been set
      # See tab_outcome
      data_reactives$components_no_reference <- shiny::reactive({
        # Check to see if the reference component has been set
        # Once tab_outcome loads this will be either preset for default datasets
        # Or the most frequent component
        if (is.null(data_reactives$reference_component())) {
          return(NULL)
        } else {
          # Return the list of components without the reference
          # see utils.R
          return(
            get_components_no_reference( # nolint: object_name
              data_reactives$formatted_data(),
              data_reactives$reference_component()
            )
          )
        }
      })

      # Return the reactive
      return(data_reactives)

    }
  )
}