renderFreqOutcomeTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("outcome")),
    uiOutput(ns("desirable"))
  )
}

renderFreqOutcomeTabServer <- function(id, data, freq){
  moduleServer(
    id,
    function(input,
             output,
             session,
             thisId = id,
             globalData = data,
             globalFreq = freq) {
      
      ns <- NS(id)
      observe({
        if(!isDataValid(globalData)){
          output$outcome <- defaultNoData(ns)
        }
        else{
          if(globalData$type == "continous"){
            output$outcome <- renderUI(
              tagList(
                radioButtons(
                  ns("outcome"),
                  "Select an outcome measure:",
                  c("Mean Difference (MD)" = "md",
                    "Standardised Mean Difference (SMD)" = "smd"),
                  selected = if_else(!is.null(globalData$measure), globalData$measure, "md")
                ),
                radioButtons(
                  ns("desirable"),
                  "For treatment rankings a smaller outcome value (MD / SMD) is:",
                  c("Desirable" = 1,
                    "Undesirable" = 0),
                  selected = if_else(!is.null(globalData$desirable), as.numeric(globalData$desirable), 1)
                )
              )
            )
          }
          else if(globalData$type == "binary"){
            output$outcome <- renderUI(
              tagList(
                radioButtons(
                  ns("outcome"),
                  "Select an outcome measure:",
                  c("Odds Ratio (OR)" = "or",
                    "Risk Ratio (RR)" = "rr",
                    "Risk Difference (RD)" = "rd"),
                  selected = if_else(!is.null(globalData$measure), globalData$measure, "or")
                ),
                radioButtons(
                  ns("desirable"),
                  "For treatment rankings an outcome value (OR / RR / RR) less than 1 is:",
                  c("Desirable" = 1,
                    "Undesirable" = 0),
                  selected = if_else(!is.null(globalData$desirable), as.numeric(globalData$desirable), 0)
                )
              )
            )
          }
        }
      }) %>% bindEvent(globalData$valid)
      
      observe({
        print("setting outcome measure")
        globalFreq$outcome <- input$outcome
        print("setting desirable")
        globalFreq$desirable <- as.logical(input$desirable)
        globalFreq$valid <- F
      }) %>% bindEvent(input$outcome, input$desirable)
      
      
      observe({
        loadDefaultData(globalData, globalFreq)
      }) %>% bindEvent(input$defaultData)
      
    })
}