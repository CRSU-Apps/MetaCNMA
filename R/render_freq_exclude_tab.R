renderFreqExcludeTabUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("warning")),
          uiOutput(ns("info")),
          uiOutput(ns("exclude")))
}

renderFreqExcludeTabServer <- function(id, data, freq) {
  moduleServer(id,
               function(input,
                        output,
                        session) {
                 ns <- NS(id)
                 
                 observe({
                   output$warning <- NULL
                   output$info <- NULL
                   tryCatch({
                     withCallingHandlers(
                       warning = function(cond) {
                         output$warning <- warning_alert(cond)
                       },
                       message = function(cond) {
                         output$message <- message_alert(cond)
                       },
                       {
                         if (!is_data_valid(data)) {
                           output$exclude <- default_no_data(ns)
                         }
                         else{
                           if (is.null(freq$data)) {
                             withProgress({
                               format_data(data, freq)
                             },
                             message = "Formatting Data")
                           }
                           studies <- get_studies(data, freq)
                           output$exclude <- renderUI(tagList(fluidRow(
                             column(
                               3,
                               h3("Study Selection"),
                               checkboxGroupInput("exclude",
                                                  label = "Choose any Studies you wish to exclude",
                                                  choices = studies)
                             ),
                             column(
                               9,
                               h3("Data"),
                               DT::renderDataTable(
                                 freq$data,
                                 filter = 'top',
                                 options = list(
                                   scrollX = T,
                                   pageLength = 10,
                                   info = FALSE,
                                   lengthMenu = list(c(10,-1), c("10", "All"))
                                 )
                               )
                             )
                           )))
                         }
                       }
                     )
                   },
                   error = function(e) {
                     print("this error occured trying to render the studies")
                     error_alert(e$message)
                     invalidate_data(data, freq)
                   })
                 }) %>% bindEvent(data$valid)
                 
                 observe({
                   load_default_data(data, freq)
                 }) %>% bindEvent(input$defaultData)
                 
                 observe({
                   data$exclude <- input$exclude
                 }) %>% bindEvent(input$exclude)
                 
               })
}