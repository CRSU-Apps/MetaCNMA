renderDataSummaryTabUI <- function(id){
  ns <- NS(id)
  tagList(
    h1("Data Summary", class = "text-center"),
    uiOutput("dataSummary")
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
                freqPairwise(globalData, globalFreq)
              },
              message = "Formatting Data")
            }
            else if (!is.null(globalFreq$pairwise)){
              
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