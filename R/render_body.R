renderBody <- function() {
  tabItems(
    tabItem(tabName = "home_1",
            renderHomeTabUI("home_1")),
    tabItem(tabName = "dataUpload_1",
            renderDataUploadTabUI("dataUpload_1")),
    tabItem(tabName = "viewData_1",
            renderViewDataTabUI("viewData_1")),
    tabItem(tabName = "dataHelp_1",
            renderDataHelpTab()),
    tabItem(tabName = "dataSummary_1",
            renderDataSummaryTabUI("dataSummary_1")),
    tabItem(tabName = "freqOutcome_1",
            renderFreqOutcomeTabUI("freqOutcome_1")),
    tabItem(tabName = "freqExclude_1",
            renderFreqExcludeTabUI("freqExclude_1")),
    tabItem(tabName = "forestPlot_1",
            renderForestPlotTabUI("forestPlot_1")),
    tabItem(tabName = "privacyPolicy",
            renderStaticPageUI("privacyPolicy", "md/privacy_policy.md")),
    tabItem(tabName = "readme",
            renderStaticPageUI("readme", "readme.md"))
  )
  
}