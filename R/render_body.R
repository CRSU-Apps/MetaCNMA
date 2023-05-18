renderBody <- function() {
  tabItems(
    tabItem(tabName = "home",
            renderHomeTabUI("home_1")),
    tabItem(tabName = "dataUpload",
            renderDataUploadTabUI("dataUpload_1")),
    tabItem(tabName = "viewData",
            renderViewDataTabUI("viewData_1")),
    tabItem(tabName = "dataHelp",
            renderDataHelpTab()),
    tabItem(tabName = "dataSummary",
            renderDataSummaryTabUI("dataSummary_1")),
    tabItem(tabName = "freqOutcome",
            renderFreqOutcomeTabUI("freqOutcome_1")),
    tabItem(tabName = "freqExclude",
            renderFreqExcludeTabUI("freqExclude_1")),
    tabItem(tabName = "forestPlot",
            renderForestPlotTab()),
    tabItem(tabName = "privacyPolicy",
            renderStaticPageUI("privacyPolicy", "md/privacy_policy.md")),
    tabItem(tabName = "readme",
            renderStaticPageUI("readme", "readme.md"))
  )
  
}