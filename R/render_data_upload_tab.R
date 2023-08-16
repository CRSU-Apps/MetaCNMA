renderDataUploadTabUI <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(column(
    width = 6,
    h1("Upload Data"),
    uiOutput(ns("warning")),
    uiOutput(ns("info")),
    p("Data should be uploaded as either a .csv or .xlsx file"),
    p(
      "For help on uploading data see the ",
      tabLink("dataHelp", "data help page")
    ),
    offset = 3
  )),
  fluidRow(
    column(width = 4,
           uiOutput(ns("fileInput")),
           offset = 2),
    column(width = 4,
           uiOutput(ns("reloadButton"))),
    class = "vertical-align"
  ))
}

renderDataUploadTabServer <- function(id, data, freq) {
  moduleServer(id,
               function(input,
                        output,
                        session) 
                        {
                 ns <- NS(id)
                 output$fileInput <- defaultFileInput(ns)
                 
                 observe({
                   print("Invalidating Data")
                   output$reloadButton <- NULL
                   output$fileInput <- defaultFileInput(ns)
                   invalidateData(data, freq)
                 }) %>% bindEvent(input$reloadButton)
                 
                 observe({
                   output$warning <- NULL
                   output$info <- NULL
                   tryCatch({
                     withCallingHandlers(
                       warning = function(cond) {
                         output$warning <- warningAlert(cond)
                       },
                       message = function(cond) {
                         output$message <- messageAlert(cond)
                       },
                       {
                         invalidateData(data, freq)
                         if (validateInput(input$data, data$type)) {
                           output$reloadButton <- defaultReloadButton(ns)
                           tmpData <- rio::import(input$data$datapath)
                           data$format <-
                             if_else(suppressMessages(isWide(tmpData)), "wide", "long")
                           data$default = F
                           freq$valid = F
                           data$data <- tmpData
                           data$valid = T
                           print("Data valid and loaded")
                         }
                         else{
                           invalidateData(data, freq)
                         }
                       }
                     )
                     
                   },
                   error = function(e) {
                     errorAlert(e$message)
                     invalidateData(data, freq)
                   })
                   
                 }) %>% bindEvent(input$data)
                 
                 observe({
                   if (!is.null(data$valid) & !as.logical(data$valid)) {
                     print("Resetting File Input")
                     output$fileInput <- defaultFileInput(ns)
                     output$reloadData <- NULL
                   }
                 }) %>% bindEvent(data$valid, ignoreInit = T)
                 
                 observe({
                   output$warning <- NULL
                   output$info <- NULL
                   tryCatch({
                     withCallingHandlers(
                       warning = function(cond) {
                         output$warning <- warningAlert(cond)
                       },
                       message = function(cond) {
                         output$message <- messageAlert(cond)
                       },
                       {
                         print("resetting data")
                         invalidateData(data, freq)
                         print("loading default data")
                         loadDefaultData(data, freq)
                       }
                     )
                   },
                   error = function(e) {
                     errorAlert(e$message)
                     invalidateData(data, freq)
                   })
                 }) %>% bindEvent(data$type)
                 
                 
               })
}