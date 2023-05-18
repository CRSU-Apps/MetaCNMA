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
  

defaultDataContinous<- function(){
  return(list(
    dataFrame = import("data/continous.Rds"),
    type = "continous",
    format = "long",
    measure = "md",
    desirable = T
  ))
}

defaultDataBinary<- function(){
  return(list(
    dataFrame = import("data/binary.Rds"),
    type = "binary",
    format = "long",
    measure = "or",
    desirable = F
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