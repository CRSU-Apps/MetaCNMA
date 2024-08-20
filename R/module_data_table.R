#' @title Data Table Module UI
#' @description UI for Data Table Module
#' @param id Module ID
#' @return no explicit return
#' @details UI for the data_table_module
#' @examples
#' \dontrun{
#' if(interactive()){
#'    data_table_module_ui("table_1")
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{NS}}
#'  \code{\link[DT]{dataTableOutput}}
#' @rdname data_table_module_ui
#' @importFrom shiny NS
#' @importFrom DT dataTableOutput
data_table_module_ui <- function(id) {
  # Declare namespace function from ID
  ns <- shiny::NS(id)
  # Shiny output for the DT::datatable
  DT::dataTableOutput(ns("data_table_output"))
}

#' @title Data Table Module Server
#' @description A shiny Module Server for Rendering a DT::datatable()
#' @param id Module ID
#' @param reactive_df a data.frame as a shiny::reactive
#' @return No explicit return
#' @details reusable shiny module server for rendering DT::datatable
#' @examples
#' \dontrun{
#' if(interactive()){
#'    data_table_module_server(
#'      "table_1",
#'      reactive_df
#'    )
#'  }
#' }
#' @seealso
#'  \code{\link[shiny]{moduleServer}},
#'  \code{\link[shiny]{observe}},
#'  \code{\link[shiny]{bindEvent}}
#'  \code{\link[magrittr]{character(0)}}
#'  \code{\link[DT]{dataTableOutput}}
#' @rdname data_table_module_server
#' @importFrom shiny moduleServer observe bindEvent
#' @importFrom magrittr `%>%`
#' @importFrom DT renderDataTable
data_table_module_server <- function(
  id,
  reactive_df
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      # Scope the magrittr pipe
      `%>%` <- magrittr::`%>%`

      # On the data.frame from function parameters change
      # Render the data frame using DT::renderDataTable()
      shiny::observe({
        # If the data frame is NULL set the output to NULL
        if (is.null(reactive_df)) {
          output$data_table_output <- NULL
        } else {
          # Else render the data frame
          output$data_table_output <- DT::renderDataTable(
            reactive_df(),
            filter = "top",
            options = list(
              scrollX = TRUE,
              pageLength = 10,
              info = FALSE,
              lengthMenu = list(c(10, -1), c("10", "All"))
            )
          )
        }
      }) %>% shiny::bindEvent(reactive_df())
    }
  )
}