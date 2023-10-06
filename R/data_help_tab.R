data_help_binary_tab <- function() {
  shiny::tagList(
    shiny::h1("Binary Data"),
    shinydashboardPlus::box(
      title = "Long Format",
      closable = FALSE,
      width = 12,
      height = "500px",
      solidHeader = FALSE,
      collapsible = TRUE,
      shiny::div(class = "cnma-table",
        shiny::includeMarkdown("md/binary_long.md")
      )
    ),
    shinydashboardPlus::box(
      title = "Wide Format",
      closable = FALSE,
      width = 12,
      height = "500px",
      solidHeader = FALSE,
      collapsible = TRUE,
      shiny::div(class = "cnma-table",
        shiny::includeMarkdown("md/binary_wide.md")
      )
    )
  )
}

data_help_continuous_tab <- function() {
  shiny::tagList(
    shiny::h1("Coninous Data"),
    shinydashboardPlus::box(
      title = "Long Format",
      closable = FALSE,
      width = 12,
      height = "500px",
      solidHeader = FALSE,
      collapsible = TRUE,
      shiny::div(class = "cnma-table",
        shiny::includeMarkdown("md/continuous_long.md")
      )
    ),
    shinydashboardPlus::box(
      title = "Wide Format",
      closable = FALSE,
      width = 12,
      height = "500px",
      solidHeader = FALSE,
      collapsible = TRUE,
      shiny::div(class = "cnma-table",
        shiny::includeMarkdown("md/continuous_wide.md")
      )
    )
  )
}

data_help_tab <- function() {
  shiny::tagList(
    shiny::h1("Instructions for data upload", class = "text-center"),
    shiny::p("Data needs to be in arm-based format and 
    can be uploaded as either a csv or excel (xlsx)"),
    shiny::tabsetPanel(type = "tabs",
      shiny::tabPanel("Continuous Data", data_help_continuous_tab()),
      shiny::tabPanel("Binary Data", data_help_binary_tab())
    )
  )
}