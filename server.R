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
    freq,
    showReloadButton
  )
  renderHomeTabServer(
    "home_1",
    data,
    freq,
    showReloadButton
  )
  renderViewDataTabServer(
    "viewData_1",
    data,
    freq,
    showReloadButton
  )
  renderFreqOutcomeTabServer(
    "freqOutcome_1",
    data,
    freq,
    showReloadButton
  )
  renderFreqExcludeTabServer(
    "freqExclude_1",
    data,
    freq,
    showReloadButton
  )
})