#' @title Bayesian Analysis Sever
#' @description Shiny Module Server for Bayesian Analysis
#' @param id Module ID
#' @param data_reactives reactiveVals for data see module data_reactives
#' @param bayesian_options reactiveVals for bayesian options see tab_outcome
#' @return reactiveVales for the bayesian analysis
#' @details Various Reactive functionality to aid bayesian analysis
#' @examples
#' \dontrun{
#' if(interactive()){
#'    bayesian_analysis_server(
#'      "bayesian_analysis",
#'      data_reactives,
#'      bayesian_options
#'    )
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{moduleServer}},
#'  \code{\link[shiny]{reactiveValues}},
#'  \code{\link[shiny]{reactive}},
#'  \code{\link[shiny]{reactiveVal}},
#'  \code{\link[shiny]{observe}},
#'  \code{\link[shiny]{bindEvent}}
#'  \code{\link[magrittr]{character(0)}}
#' @rdname bayesian_analysis_server
#' @importFrom shiny moduleServer reactiveValues
#' reactive reactiveVal observe bindEvent
#' @importFrom magrittr `%>%`
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

      # Scope the magrittr pipe
      `%>%` <- magrittr::`%>%`

      # Setup a reactiveValues for the bayesian reactives
      bayesian_reactives <- shiny::reactiveValues()

      # Wrap the update_reative to be accesible from the bayesian reactives
      bayesian_reactives$update_reactive <- shiny::reactive(
        bayesian_options$update_reactive()
      )

      # Forward declare the model reactive
      bayesian_reactives$model <- shiny::reactiveVal(NULL)

      # Forward declare the console out
      # @todo remove?
      bayesian_reactives$console_out <- shiny::reactiveVal(NULL)

      # Forward declare the data.frame for studies with exclusions
      bayesian_reactives$exclude_df <- shiny::reactiveVal(NULL)

      # Reactive to check if the model has been run (the model is not NULL)
      bayesian_reactives$is_model_run <- shiny::reactive({
        return(!is.null(bayesian_reactives$model()))
      })

      # Set the model to NULL if the update_reactive changes
      # (any change required to trigger and update see bayesian_options)
      shiny::observe({
        bayesian_reactives$model(NULL)
      }) %>% shiny::bindEvent(
        bayesian_reactives$update_reactive()
      )

      # Return the reactive
      return(bayesian_reactives)

    }
  )
}