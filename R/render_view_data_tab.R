renderViewDataTabUI <- function(id) {
  ns <- NS(id)
  tagList(h1("View Data"),
          uiOutput(ns("warning")),
          uiOutput(ns("info")),
          uiOutput(ns("citation")),
          uiOutput(ns("data")))
}

renderViewDataTabServer <-
  function(id, data, freq) {
    moduleServer(id,
                 function(input,
                          output,
                          session) {
                   ns <- NS(id)
                   
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
                           if (!is.null(data$data) &
                               as.logical(data$valid)) {
                             if (!is.null(data$default) & as.logical(data$default)) {
                               if (!is.null(data$type) & data$type == "binary")
                               {
                                 output$citation <-
                                   renderUI(includeMarkdown("md/binary_citation.md"))
                               } else if (!is.null(data$type) &
                                          data$type == "continuous") {
                                 output$citation <-
                                   renderUI(includeMarkdown("md/continuous_citation.md"))
                               }
                             }
                             else {
                               output$citation <- NULL
                             }
                             print("Rendering Data")
                             output$data <- renderUI(tagList(
                               DT::renderDataTable(
                                 data$data,
                                 filter = 'top',
                                 options = list(
                                   scrollX = T,
                                   pageLength = 10,
                                   info = FALSE,
                                   lengthMenu = list(c(10,-1), c("10", "All"))
                                 )
                               )
                             ))
                             
                           }
                           else {
                             output$citation <- NULL
                             output$data <- defaultNoData(ns)
                           }
                         }
                       )
                     },
                     error = function(e) {
                       output$citation <- NULL
                       output$data <- NULL
                       errorAlert(e$message)
                     })
                   }) %>% bindEvent(data$valid)
                   
                   observe({
                     loadDefaultData(data, freq)
                   }) %>% bindEvent(input$defaultData)
                   
                   
                 })
  }
