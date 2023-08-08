renderFreqExcludeTabUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("warning")),
          uiOutput(ns("info")),
          uiOutput(ns("exclude")))
}

renderFreqExcludeTabServer <- function(id, data, freq) {
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
                         if (!isDataValid(data)) {
                           output$exclude <- defaultNoData(ns)
                         }
                         else{
                           if (is.null(freq$data)) {
                             withProgress({
                               formatData(data, freq)
                             },
                             message = "Formatting Data")
                           }
                           studies <- getStudies(data, freq)
                           output$exclude <- renderUI(tagList(fluidRow(
                             column(
                               3,
                               h3("Study Selection"),
                               checkboxGroupInput("exclude",
                                                  label = "Choose any Studies you wish to exclude",
                                                  choices = studies)
                             ),
                             column(
                               9,
                               h3("Data"),
                               DT::renderDataTable(
                                 freq$data,
                                 filter = 'top',
                                 options = list(
                                   scrollX = T,
                                   pageLength = 10,
                                   info = FALSE,
                                   lengthMenu = list(c(10,-1), c("10", "All"))
                                 )
                               )
                             )
                           )))
                         }
                       }
                     )
                   },
                   error = function(e) {
                     print("this error occured trying to render the studies")
                     errorAlert(e$message)
                     invalidateData(data, freq)
                   })
                 }) %>% bindEvent(data$valid)
                 
                 observe({
                   loadDefaultData(data, freq)
                 }) %>% bindEvent(input$defaultData)
                 
               })
}