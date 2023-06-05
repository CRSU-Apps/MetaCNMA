renderViewDataTabUI <- function(id) {
  ns <- NS(id)
  tagList(h1("View Data"),
          uiOutput(ns("citation")),
          uiOutput(ns("data")))
}

renderViewDataTabServer <-
  function(id, data, freq) {
    moduleServer(id,
                 function(input,
                          output,
                          session,
                          thisId = id,
                          globalData = data,
                          globalFreq = freq) {
                   ns <- NS(id)
                   
                   observe({
                     #print(globalData)
                     tryCatch({
                       if (!is.null(globalData$data) &
                           as.logical(globalData$valid)) {
                         if (!is.null(globalData$default) & as.logical(globalData$default)) {
                           if (!is.null(globalData$type) & globalData$type == "binary")
                           {
                             output$citation <-
                               renderUI(includeMarkdown("md/binary_citation.md"))
                           } else if (!is.null(globalData$type) &
                                      globalData$type == "continous") {
                             output$citation <-
                               renderUI(includeMarkdown("md/continous_citation.md"))
                           }
                         }
                         else {
                           output$citation <- NULL
                         }
                         print("Rendering Data")
                         output$data <- renderUI(
                           tagList(
                             DT::renderDataTable(globalData$data,
                                                 filter='top',
                                             options = list(scrollX = T, pageLength = 10, info = FALSE,
                                                            lengthMenu = list(c(10, -1), c("10", "All")) ))
                           )
                         )
                         
                       }
                       else {
                         output$citation <- NULL
                         output$data <- defaultNoData(ns)
                           
                       }
                     },
                     error = function(e) {
                       output$citation <- NULL
                       output$data <- NULL
                       errorAlert(e$message)
                     })
                   }) %>% bindEvent(globalData$valid)
                   
                   observe({
                     loadDefaultData(globalData, globalFreq)
                   }) %>% bindEvent(input$defaultData)
                   
                   
                 })
  }
