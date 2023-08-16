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
             session) {
      
      ns <- NS(id)
      
      # To do change tab names to namespace (to allow reuse of module)
      observe({
        print(tab())
        if(tab() == id){
          output$warning <- NULL
          output$info <- NULL
          output$comparitor <- NULL
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
                if (!isDataValid(data)) {
                  output$comparitor <- defaultNoData(ns)
                  output$plot <- NULL
                  return(NULL)
                }
                if (is.null(freq$pairwise)){
                  withProgress({
                    freq$pairwise <- freqPairwise(data, freq)
                    freq$nConnection <- runNetconnection(freq$pairwise)
                  },
                  message = "Formatting Data")
                }
                if(is.null(freq$nm)){
                  withProgress({
                    freq$nm <- runNetmeta(freq$pairwise, ref = getMostFreqComponent(freq$pairwise))
                  },
                  message = "Running Network Meta Analysis")
                }
                if(is.null(freq$nc)){
                  withProgress({
                    freq$nc <- runNetcomb(freq$nm, inactive = getMostFreqComponent(freq$pairwise))
                  },
                  message = "Running Network Meta Analysis")
                }
                if(getOutcomeMeasure(freq$outcome) == "Outcome Measure"){
                  warning("No Outcome Measure Selected")
                }
                output$plot <- renderUI(renderNetForest(freq$nc, getOutcomeMeasure(freq$outcome)))
              }
            )
          },
          error = function(e) {
            errorAlert(e$message)
            invalidateData(data, freq)
          })
        }
      }) %>% bindEvent(tab(), input$defaultData)
      
      observe({
        loadDefaultData(data, freq)
      }) %>% bindEvent(input$defaultData)
      
      observe({
        freq$pairwise <- NULL
        freq$nm <- NULL
        freq$nc <- NULL
      }) %>% bindEvent(freq$valid)
      
    })
}