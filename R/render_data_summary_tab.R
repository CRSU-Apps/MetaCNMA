renderDataSummaryTabUI <- function(id){
  ns <- NS(id)
  tagList(
    h1("Data Summary", class = "text-center"),
    uiOutput("dataSummary")
  )
}

renderDataSummaryTabServer <- function(id, data, freq, showReloadButton){
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
            output$dataSummary <- defaultNoData(ns)
          }
          else{
            
          }
        })
      })
      
      observe({
        loadDefaultData(globalData, globalFreq)
      }) %>% bindEvent(input$defaultData)
      
      
      
    }
  )
}