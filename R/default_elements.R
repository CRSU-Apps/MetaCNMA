tab_link <- function(tabName = "tab name", linkText = "tab link") {
  return(
    shiny::actionLink("updateSidebar",
               linkText,
               `data-toggle`= "tab",
               `data-value` = tabName,
               onclick = paste0("Shiny.onInputChange('targetTab', '", tabName, "')"))
  )
}

default_file_input <- function(ns) {
  shiny::renderUI({
    shiny::fileInput(
      inputId = ns("data"),
      label = "",
      buttonLabel = list(shiny::icon("file"), "Select File"),
      placeholder = "No file selected",
      accept = c(".csv", ".xlsx")
    )
  })
}

default_no_data <- function(ns) {
  shiny::renderUI(shiny::tagList(p("No data loaded"),
                   shiny::div(
                     shiny::actionButton(
                       ns("defaultData"),
                       "Reload Default Data",
                       shiny::icon("arrows-rotate"),
                       style =
                         "color: #fff; background-color: #dc3545; border-color: #dc3545"
                     )
                   )))
}

default_data_continuous <- function() {
  return(list(
    dataFrame = rio::import("data/continuous.Rds"),
    type = "continuous",
    format = "long",
    measure = "md",
    desirable = 1,
    randomEffects = 0,
    outcomeName = "Total Cholesterol Level (mmol/L)"
  ))
}

default_data_binary <- function() {
  return(list(
    dataFrame = rio::import("data/binary.Rds"),
    type = "binary",
    format = "long",
    measure = "or",
    desirable = 0,
    randomEffects = 0,
    outcomeName = "Incidence of Delirium"
  ))
}


default_reload_button <- function(ns, buttonText = "Delete Data") {
  shiny::renderUI({
    shiny::div(
      shiny::actionButton(ns("reloadButton"), buttonText, shiny::icon("trash"),
                   style =
                     "color: #fff; background-color: #dc3545; border-color: #dc3545")
    )
  })
}