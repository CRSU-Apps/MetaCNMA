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
                output$warning <- warning_alert(cond)
              },
              message = function(cond){
                output$message <- message_alert(cond)
              },
              {
                if (!is_data_valid(data)) {
                  output$comparitor <- default_no_data(ns)
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
                    freq$nm <- runNetmeta(freq$pairwise, ref = getMostFreqComponent(freq$pairwise), random_eff = as.logical(freq$randomEffects))
                  },
                  message = "Running Network Meta Analysis")
                }
                if(is.null(freq$nc)){
                  withProgress({
                    freq$nc <- runNetcomb(freq$nm, inactive = getMostFreqComponent(freq$pairwise))
                  },
                  message = "Running Network Meta Analysis")
                }
                if(get_outcome_measure(freq$outcome) == "Outcome Measure"){
                  warning("No Outcome Measure Selected")
                }
                output$comparitor <- renderUI(h4(paste0("Forest plot showing the ", get_outcome_measure(freq$outcome), " of ", freq$outcomeName, " when compared against ", getMostFreqComponent(freq$pairwise))))
                output$plot <- renderUI(renderNetForest(freq$nc, data$type, get_outcome_measure(freq$outcome)))
              }
            )
          },
          error = function(e) {
            error_alert(e$message)
            invalidate_data(data, freq)
          })
        }
      }) %>% bindEvent(tab(), input$defaultData)
      
      observe({
        load_default_data(data, freq)
      }) %>% bindEvent(input$defaultData)
      
      observe({
        freq$pairwise <- NULL
        freq$nm <- NULL
        freq$nc <- NULL
      }) %>% bindEvent(freq$valid)
      
    })
}