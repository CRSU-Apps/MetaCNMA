renderForestPlotTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Forest Plot"),
    uiOutput(ns("warning")),
    uiOutput(ns("info")),
    uiOutput(ns("comparitor")),
    withSpinner(uiOutput(ns("plot")), type = 6)
  )
}

renderForestPlotTabServer <- function(id, data, freq, tab){
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
        if(currentTab() == "forestPlot"){
          output$warning <- NULL
          output$info <- NULL
          print("forestPlot")
          tryCatch({
            withCallingHandlers(
              warning = function(cond){
                output$warning <- warningAlert(cond)
              },
              message = function(cond){
                output$message <- messageAlert(cond)
              },
              {
                if (!isDataValid(globalData)) {
                  output$compator <- defaultNoData(ns)
                  output$plot <- NULL
                  return(NULL)
                }
                if (is.null(globalFreq$pairwise)){
                  withProgress({
                    globalFreq$pairwise <- freqPairwise(globalData, globalFreq)
                  },
                  message = "Formatting Data")
                }
                if(is.null(globalFreq$nm)){
                  withProgress({
                    globalFreq$nm <- runNetmeta(globalFreq$pairwise, ref = getMostFreqComponent(globalFreq$pairwise))
                  },
                  message = "Running Network Meta Analysis")
                }
                if(is.null(globalFreq$nc)){
                  withProgress({
                    globalFreq$nc <- runNetcomb(globalFreq$nm, inactive = getMostFreqComponent(globalFreq$pairwise))
                  },
                  message = "Running Network Meta Analysis")
                }
                output$plot <- renderUI(renderNetForest(globalFreq$nc))
              }
            )
          },
          error = function(e) {
            errorAlert(e$message)
            invalidateData(globalData, globalFreq)
          })
        }
      }) %>% bindEvent(currentTab())
      
      observe({
        loadDefaultData(globalData, globalFreq)
      }) %>% bindEvent(input$defaultData)
      
    })
}