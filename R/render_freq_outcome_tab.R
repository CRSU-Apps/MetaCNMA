renderFreqOutcomeTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("outcomeName")),
    uiOutput(ns("outcome")),
    uiOutput(ns("desirable")),
    uiOutput(ns("randomEffects"))
  )
}

renderFreqOutcomeTabServer <- function(id, data, freq, tab){
  moduleServer(
    id,
    function(input,
             output,
             session) {
      
      ns <- NS(id)
      
      observe({
        print("data change")
        if(!is_data_valid(data)){
          print("Data not valid")
          output$outcome <- default_no_data(ns)
        }
        else{
          if(data$type == "continuous"){
            output$outcome <- renderUI(
              tagList(
                radioButtons(
                  ns("outcome"),
                  "Select an outcome measure:",
                  c("Mean Difference (MD)" = "md",
                    "Standardised Mean Difference (SMD)" = "smd"),
                  selected = ifelse(!is.null(freq$measure), freq$measure, "md")
                )
              )
            )
            output$desirable <- renderUI(
              tagList(
                radioButtons(
                  ns("desirable"),
                  "For treatment rankings a smaller outcome value (MD / SMD) is:",
                  c("Desirable" = 1,
                    "Undesirable" = 0),
                  selected = ifelse(!is.null(freq$desirable), freq$desirable, 1)
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
                  selected = ifelse(!is.null(freq$measure), freq$measure, "or")
                )
              )
            )
            output$desirable <- renderUI(
              tagList(
                radioButtons(
                  ns("desirable"),
                  "For treatment rankings an outcome value (OR / RR / RR) less than 1 is:",
                  c("Desirable" = 1,
                    "Undesirable" = 0),
                  selected = ifelse(!is.null(freq$desirable), as.numeric(freq$desirable), 0)
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
                selected = ifelse(!is.null(freq$randomEffects), as.numeric(freq$randomEffects), 0)
              )
            )
          )
        output$outcomeName <- renderUI(
          tagList(
            textInput(
              ns("outcomeName"),
                 "Outcome Name",
                 value = ifelse(!is.null(freq$outcomeName), freq$outcomeName, "")
            )
          )
        )
        }
      }) %>% bindEvent(data$valid)
      
      outcomeName <- reactiveValues()
      observe({
        if(tab() == id){
          invalidateLater(3000, session)
          outcomeName$text <- isolate(input$outcomeName)
        }
      }) %>% bindEvent(input$outcomeName, ignoreInit = T)
      
      observe({
        if(tab() != id & data$default){
          print("Setting default measures")
          freq$outcome <- data$measure
          freq$desirable <- data$desirable
          freq$randomEffects <- data$randomEffects
          freq$outcomeName <- data$outcomeName
        }
        else{
          # Set defaults for default data
          print("setting outcome measure")
          freq$outcome <- input$outcome
          print(paste0("Outcome Measure Set to ", freq$outcome))
          print("setting desirable")
          freq$desirable <- input$desirable
          print("setting random effects")
          freq$randomEffects <- input$randomEffects
          print(paste0("Random Effects Set to ", freq$randomEffects))
          freq$outcomeName <- input$outcomeName
          print("setting outcome name")
          print(paste0("Outcome Name Set to ", freq$outcomeName))
        }
        freq$valid <- F
      }) %>% bindEvent(input$outcome, input$desirable, input$randomEffects, data$type, outcomeName$text)
      
      observe({
        load_default_data(data, freq)
      }) %>% bindEvent(input$defaultData)
      
    })
}