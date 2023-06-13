renderDataSummaryTabUI <- function(id){
  ns <- NS(id)
  tagList(
    h1("Data Summary", class = "text-center"),
    withSpinner(uiOutput(ns("dataSummary")), type = "6")
  )
}

renderDataSummaryTabServer <- function(id, data, freq, tab){
  moduleServer(
    id,
    function(input,
             output,
             session,
             thisId = id,
             globalData = data,
             globalFreq = freq,
             currentTab = tab) {
      
      ns <- NS(id)
      # To do change tab names to namespace (to allow reuse of module)
      observe({
        print(currentTab())
        if(currentTab() == "dataSummary"){
          print("dataSummary")
          tryCatch({
            if (!isDataValid(globalData)) {
              output$dataSummary <- defaultNoData(ns)
            }
            else if (is.null(globalFreq$pairwise)){
              withProgress({
                globalFreq$pairwise <- freqPairwise(globalData, globalFreq)
              },
              message = "Formatting Data")
              output$dataSummary <- renderFreqSummary(globalFreq$pairwise)
            }
            else if (!is.null(globalFreq$pairwise)){
              output$dataSummary <- renderFreqSummary(globalFreq$pairwise)
            }
          })
        }
      }) %>% bindEvent(currentTab())
      
      observe({
        loadDefaultData(globalData, globalFreq)
      }) %>% bindEvent(input$defaultData)
      
      
      
    }
  )
}