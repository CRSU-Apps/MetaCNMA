renderDataSummaryTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Data Summary", class = "text-center"),
    uiOutput(ns("warning")),
    uiOutput(ns("info")),
    withSpinner(uiOutput(ns("dataSummary")), type = "6")
  )
}

renderDataSummaryTabServer <- function(id, data, freq, tab) {
  moduleServer(id,
               function(input,
                        output,
                        session) {
                 ns <- NS(id)
                 observe({
                   print(tab())
                   output$warning <- NULL
                   output$info <- NULL
                   if (tab() == id) {
                     print(paste0("tab: ", id))
                     print("dataSummary")
                     tryCatch({
                       withCallingHandlers(
                         warning = function(cond) {
                           output$warning <- warningAlert(cond)
                         },
                         message = function(cond) {
                           output$message <- messageAlert(cond)
                         },
                         {
                           if (!isDataValid(data)) {
                             output$dataSummary <- defaultNoData(ns)
                           }
                           else if (is.null(freq$pairwise)) {
                             withProgress({
                               freq$pairwise <- freqPairwise(data, freq)
                               freq$nConnection <- runNetconnection(freq$pairwise)
                             },
                             message = "Formatting Data")
                             output$dataSummary <-
                               renderFreqSummary(freq$pairwise, freq$nConnection)
                           }
                           else if (!is.null(freq$pairwise)) {
                             output$dataSummary <- renderFreqSummary(freq$pairwise, freq$nConnection)
                           }
                         }
                       )
                       
                     })
                   }
                 }) %>% bindEvent(tab())
                 
                 observe({
                   loadDefaultData(data, freq)
                 }) %>% bindEvent(input$defaultData)
                 
                 
               })
}