renderFreqExcludeTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("exclude"))
    )
}

renderFreqExcludeTabServer <- function(id, data, freq, showReloadButton){
  moduleServer(
    id,
    function(input,
             output,
             session,
             thisId = id,
             globalData = data,
             globalFreq = freq,
             globalShowReloadButton) {
      
      ns <- NS(id)
      
      observe({
        tryCatch({
          if (!isDataValid(globalData)) {
            output$exclude <- defaultNoData(ns)
          }
          else{
            if (is.null(globalFreq$data)) {
              withProgress({
                formatData(globalData, globalFreq)
              },
              message = "Formatting Data")
            }
            studies <- getStudies(globalData, globalFreq)
            output$exclude <- renderUI(tagList(
              fluidRow(
                column(
                  3,
                  h3("Study Selection"),
                  checkboxGroupInput("exclude",
                                     label = "Choose any Studies you wish to exclude",
                                     choices = studies)
                ),
                column(9,
                       h3("Data"),
                       DT::renderDataTable(globalFreq$data,
                                           filter='top',
                                   options = list(scrollX = T, pageLength = 10, info = FALSE,
                                                  lengthMenu = list(c(10, -1), c("10", "All")) )))
              )))
          }
          
        },
        error = function(e) {
          print("this error occured trying to render the studies")
          errorAlert(e$message)
          invalidateData(globalData, globalFreq)
        })
      }) %>% bindEvent(globalData$valid)
      
      observe({
        loadDefaultData(globalData, globalFreq)
      }) %>% bindEvent(input$defaultData)
      
    })
}