render_body <- function() {

  shinydashboard::tabItems(
    shinydashboard::tabItem(tabName = "home_1",
      reactive_page("home_1", # nolint onject_usage
        paste(site_info$title, site_info$version, sep = " ") # nolint object_usage
      )
    ),
    shinydashboard::tabItem(tabName = "data_upload_1",
      reactive_page("data_upload_1", "Data Upload") # nolint object_usage
    ),
    shinydashboard::tabItem(tabName = "viewData_1",
      reactive_page("viewData_1", "View Data") # nolint object_usage
    ),
    shinydashboard::tabItem(tabName = "dataHelp_1",
      data_help_tab() # nolint object_usage
    ),
#     tabItem(tabName = "dataSummary_1",
#             renderDataSummaryTabUI("dataSummary_1")),
#     tabItem(tabName = "freqOutcome_1",
#             renderFreqOutcomeTabUI("freqOutcome_1")),
#     tabItem(tabName = "freqExclude_1",
#             renderFreqExcludeTabUI("freqExclude_1")),
#     tabItem(tabName = "forestPlot_1",
#             renderForestPlotTabUI("forestPlot_1")),
    tabItem(tabName = "privacy_policy",
      static_page_ui("privacy_policy", "md/privacy_policy.md") # nolint object_usage
    ),
    tabItem(tabName = "readme",
      static_page_ui("readme", "readme.md") # nolint object_usage
    )
  )
}