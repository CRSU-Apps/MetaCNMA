reactive_page <- function(id, title = "") {

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1(title, class = "text-center"),
    shiny::uiOutput(ns("warning")),
    shiny::uiOutput(ns("info")),
    shiny::uiOutput(ns("static_content")),
    shiny::uiOutput(ns("inputs")),
    withSpinner(shiny::uiOutput(ns("outputs")), type = 6)
  )
}