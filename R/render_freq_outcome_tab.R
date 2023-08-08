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
             session) {
      
      ns <- NS(id)
      observe({
        if(!isDataValid(data)){
          output$outcome <- defaultNoData(ns)
        }
        else{
          if(data$type == "continous"){
            output$outcome <- renderUI(
              tagList(
                radioButtons(
                  ns("outcome"),
                  "Select an outcome measure:",
                  c("Mean Difference (MD)" = "md",
                    "Standardised Mean Difference (SMD)" = "smd"),
                  selected = if_else(!is.null(data$measure), data$measure, "md")
                ),
                radioButtons(
                  ns("desirable"),
                  "For treatment rankings a smaller outcome value (MD / SMD) is:",
                  c("Desirable" = 1,
                    "Undesirable" = 0),
                  selected = if_else(!is.null(data$desirable), as.numeric(data$desirable), 1)
                )
              )
            )
          }
          else if(data$type == "binary"){
            output$outcome <- renderUI(
              tagList(
                radioButtons(
                  ns("outcome"),
                  "Select an outcome measure:",
                  c("Odds Ratio (OR)" = "or",
                    "Risk Ratio (RR)" = "rr",
                    "Risk Difference (RD)" = "rd"),
                  selected = if_else(!is.null(data$measure), data$measure, "or")
                ),
                radioButtons(
                  ns("desirable"),
                  "For treatment rankings an outcome value (OR / RR / RR) less than 1 is:",
                  c("Desirable" = 1,
                    "Undesirable" = 0),
                  selected = if_else(!is.null(data$desirable), as.numeric(data$desirable), 0)
                )
              )
            )
          }
        }
      }) %>% bindEvent(data$valid)
      
      observe({
        print("setting outcome measure")
        freq$outcome <- input$outcome
        print("setting desirable")
        freq$desirable <- as.logical(input$desirable)
        freq$valid <- F
      }) %>% bindEvent(input$outcome, input$desirable)
      
      
      observe({
        loadDefaultData(data, freq)
      }) %>% bindEvent(input$defaultData)
      
    })
}