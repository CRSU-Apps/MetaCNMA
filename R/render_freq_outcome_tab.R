renderFreqOutcomeTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("outcomeName")),
    uiOutput(ns("outcome")),
    uiOutput(ns("desirable")),
    uiOutput(ns("randomEffects"))
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
          print("Data not valid")
          output$outcome <- defaultNoData(ns)
        }
        else{
          print(paste0("Data Measure: ", data$measure))
          print(paste0("Data type: ", data$type))
          if(data$type == "continuous"){
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
                  selected = if_else(!is.null(freq$measure), freq$measure, "or")
                ),
                radioButtons(
                  ns("desirable"),
                  "For treatment rankings an outcome value (OR / RR / RR) less than 1 is:",
                  c("Desirable" = 1,
                    "Undesirable" = 0),
                  selected = if_else(!is.null(freq$desirable), as.numeric(freq$desirable), 0)
                )
              )
            )
          }
          output$randomEffects <- renderUI(
            tagList(
              radioButtons(
                ns("randomEffects"),
                "For treatment rankings an outcome value (OR / RR / RR) less than 1 is:",
                c("Fixed Effects" = 0,
                  "Random Effects" = 1),
                selected = if_else(!is.null(freq$randomEffects), as.numeric(freq$randomEffects), 0)
              )
            )
          )
        output$outcomeName <- renderUI(
          tagList(
            textInput(
              ns("outcomeName"),
                 "Outcome Name",
                 value = if_else(!is.null(freq$outcomeName), freq$outcomeName, "")
            )
          )
        )
        }
      }) %>% bindEvent(data$valid)
      
      observe({
        # Set defaults for default data
        if(all(is.null(input$outcome), is.null(input$desirable), data$valid, data$default)){
          freq$outcome <- data$measure
          freq$desirable <- data$desirable
          freq$randomEffects <- F
          freq$outcomeName <- data$outcomeName
        }
        else {
          print("setting outcome measure")
          freq$outcome <- input$outcome
          print(paste0("Outcome Measure Set to ", freq$outcome))
          print("setting desirable")
          freq$desirable <- as.logical(input$desirable)
          print("setting random effects")
          print(paste0("Random Effects Set to ", freq$randomEffects))
          freq$outcomeName <- input$outcomeName
          print("setting outcome name")
          print(paste0("Outcome Name Set to ", freq$outcomeName))
        }
        freq$valid <- F
      }) %>% bindEvent(input$outcome, input$desirable, data$valid)
      
      observe({
        loadDefaultData(data, freq)
      }) %>% bindEvent(input$defaultData)
      
    })
}