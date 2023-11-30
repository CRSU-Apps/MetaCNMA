tab_link <- function(tab_name = "tab name", link_text = "tab link") {
  return(
    shiny::actionLink("updateSidebar",
                      link_text,
                      `data-toggle` = "tab",
                      `data-value` = tab_name,
                      onclick = paste0("Shiny.onInputChange('targetTab', '",
                                       tab_name, "')"))
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
  shiny::renderUI(shiny::tagList(shiny::p("No data loaded"),
                                 shiny::div(
                                   shiny::actionButton(
                                     ns("default_data"),
                                     "Reload Default Data",
                                     shiny::icon("arrows-rotate"),
                                     style =
                                       "color: #fff; background-color: #dc3545; border-color: #dc3545" # nolint line_length
                                   )
                                 )))
}

default_data_continuous <- function() {
  return(list(
    data_frame = rio::import("data/continuous.Rds"),
    type = "continuous",
    format = "long",
    measure = "md",
    desirable = 1,
    outcome_name = "Length of Stay"
  ))
}

default_data_binary <- function() {
  return(list(
    data_frame = rio::import("data/binary.Rds"),
    type = "binary",
    format = "long",
    measure = "or",
    desirable = 0,
    outcome_name = "Incidence of Delirium"
  ))
}


default_reload_button <- function(ns, button_text = "Delete Data") {
  shiny::renderUI({
    shiny::div(
      shiny::actionButton(
                          ns("reload_button"),
                          button_text,
                          shiny::icon("trash"),
                          style =
                          "color: #fff; background-color: #dc3545; border-color: #dc3545") # nolint line_length
    )
  })
}