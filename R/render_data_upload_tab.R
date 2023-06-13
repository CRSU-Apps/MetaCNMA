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
                        session,
                        thisId = id,
                        globalData = data,
                        globalFreq = freq) {
                 ns <- NS(id)
                 output$fileInput <- defaultFileInput(ns)
                 
                 observe({
                   print("Invalidating Data")
                   output$reloadButton <- NULL
                   output$fileInput <- defaultFileInput(ns)
                   invalidateData(globalData, globalFreq)
                   #globalData$data <- NULL
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
                         invalidateData(globalData, globalFreq)
                         if (validateInput(input$data, globalData$type)) {
                           output$reloadButton <- defaultReloadButton(ns)
                           tmpData <- rio::import(input$data$datapath)
                           globalData$format <-
                             if_else(suppressMessages(isWide(tmpData)), "wide", "long")
                           globalData$default = F
                           globalFreq$valid = F
                           #globalData$data <- NULL
                           globalData$data <- tmpData
                           globalData$valid = T
                           # print(globalData$data)
                           # print(globalData$format)
                           # print(globalData$default)
                           # print(globalData$valid)
                           # print(globalFreq$valid)
                           print("Data valid and loaded")
                         }
                         else{
                           invalidateData(globalData, globalFreq)
                         }
                       }
                     )
                     
                   },
                   error = function(e) {
                     errorAlert(e$message)
                     invalidateData(globalData, globalFreq)
                   })
                   
                 }) %>% bindEvent(input$data)
                 
                 observe({
                   if (!is.null(globalData$valid) & !as.logical(globalData$valid)) {
                     print("Resetting File Input")
                     output$fileInput <- defaultFileInput(ns)
                     output$reloadData <- NULL
                   }
                 }) %>% bindEvent(globalData$valid, ignoreInit = T)
                 
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
                         invalidateData(globalData, globalFreq)
                         print("loading default data")
                         loadDefaultData(globalData, globalFreq)
                       }
                     )
                   },
                   error = function(e) {
                     errorAlert(e$message)
                     invalidateData(globalData, globalFreq)
                   })
                 }) %>% bindEvent(globalData$type)
                 
                 
               })
}