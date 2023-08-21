tabLink <- function(tabName = "tab name", linkText = "tab link"){
  return(
    actionLink("updateSidebar",
               linkText,
               `data-toggle`= "tab",
               `data-value` = tabName,
               onclick = paste0("Shiny.onInputChange('targetTab', '", tabName, "')"))
  )
}

defaultFileInput <- function(ns){
  renderUI({
    fileInput(
      inputId = ns("data"),
      label = "",
      buttonLabel = list(icon("file"), "Select File"),
      placeholder = "No file selected",
      accept = c(".csv", ".xlsx")
    )
  })
}

defaultNoData <- function(ns){
  renderUI(tagList(p("No data loaded"),
                   div(
                     actionButton(
                       ns("defaultData"),
                       "Reload Default Data",
                       icon("arrows-rotate"),
                       style =
                         "color: #fff; background-color: #dc3545; border-color: #dc3545"
                     )
                   )))
}
  

defaultDataContinuous<- function(){
  return(list(
    dataFrame = import("data/continuous.Rds"),
    type = "continuous",
    format = "long",
    measure = "md",
    desirable = 1,
    randomEffects = 0,
    outcomeName = "Total Cholesterol Level (mmol/L)"
  ))
}

defaultDataBinary<- function(){
  return(list(
    dataFrame = import("data/binary.Rds"),
    type = "binary",
    format = "long",
    measure = "or",
    desirable = 0,
    randomEffects = 0,
    outcomeName = "Incidence of Delirium"
  ))
}


defaultReloadButton <- function(ns, buttonText = "Delete Data"){
  renderUI({
    div(
      actionButton(ns("reloadButton"), buttonText, icon("trash"),
                   style =
                     "color: #fff; background-color: #dc3545; border-color: #dc3545")
    )
  })
}