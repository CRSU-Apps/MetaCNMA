##################################################################
##                         Shiny Server                         ##
##                          CRSU CNMA                           ##
##                          Ryan Field                          ##
##################################################################
shinyServer(function(input, output, session){
  # Reactive Values
  reactive_data <- Data$new()$reactive()
  reactive_freq <- Freq$new()$reactive()
  # Tabs as a reactive
  tabs <- reactive(input$tabs)
  log <- reactiveValues()
  # Load the cookie module from R/cookies.R
  cookieServer(
    id = "cookies_1",
    cookies = reactive(input$cookies),
    open_privacy_policy = reactive(input$open_privacy_policy),
    parent_session = reactive(session)
  )
  home_tab_server(
    "home_1",
    reactive_data,
    reactive_freq
  )
  data_upload_tab_server(
    "data_upload_1",
    reactive_data,
    reactive_freq
  )
  view_data_tab_server(
    "viewData_1",
    reactive_data,
    reactive_freq
  )
  # renderFreqOutcomeTabServer(
  #   "freqOutcome_1",
  #   data,
  #   freq,
  #   tabs
  # )
  # renderFreqExcludeTabServer(
  #   "freqExclude_1",
  #   data,
  #   freq
  # )
  # renderDataSummaryTabServer(
  #   "dataSummary_1",
  #   data,
  #   freq,
  #   tabs
  # )
  # renderForestPlotTabServer(
  #   "forestPlot_1",
  #   data,
  #   freq,
  #   tabs
  # )
})