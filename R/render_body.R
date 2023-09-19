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
    shinydashboard::tabItem(tabName = "view_data_1",
      reactive_page("view_data_1", "View Data") # nolint object_usage
    ),
    shinydashboard::tabItem(tabName = "data_help_1",
      data_help_tab() # nolint object_usage
    ),
#     tabItem(tabName = "dataSummary_1",
#             renderDataSummaryTabUI("dataSummary_1")),
    shinydashboard::tabItem(tabName = "freq_outcome_1",
      reactive_page("freq_outcome_1",
      "Outcome Measure and Model Settings") # nolint object_usage
    ),
    tabItem(tabName = "freq_exclude_1",
      reactive_page("freq_exclude_1",
      "Outcome Measure and Model Settings") # nolint object_usage
    ),
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