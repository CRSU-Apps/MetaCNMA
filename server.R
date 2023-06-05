##################################################################
##                         Shiny Server                         ##
##                          CRSU CNMA                           ##
##                          Ryan Field                          ##
##################################################################
shinyServer(function(input, output, session){
  cookieServer(
    id = "cookies_1", 
    globalCookies = reactive(input$cookies),
    globalOpenPrivacyPolicy = reactive(input$openPrivacyPolicy),
    globalSession = reactive(session))
  renderDataUploadTabServer(
    "dataUpload_1",
    data,
    freq
  )
  renderHomeTabServer(
    "home_1",
    data,
    freq
  )
  renderViewDataTabServer(
    "viewData_1",
    data,
    freq
  )
  renderFreqOutcomeTabServer(
    "freqOutcome_1",
    data,
    freq
  )
  renderFreqExcludeTabServer(
    "freqExclude_1",
    data,
    freq
  )
  renderDataSummaryTabServer(
    "dataSummary_1",
    data,
    freq,
    reactive(input$tabs)
  )
})