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
    shinydashboard::tabItem(tabName = "data_summary_1",
      reactive_page("data_summary_1", "Data Summary") # nolint object_usage
    ),
    shinydashboard::tabItem(tabName = "net_graph_1",
      reactive_page("net_graph_1", "Network Diagram") # nolint object_usage
    ),
    shinydashboard::tabItem(tabName = "correlation_plot_1",
      reactive_page("correlation_plot_1", "Correlation Plot") # nolint object_usage
    ),
    shinydashboard::tabItem(tabName = "upset_plot_1",
      reactive_page("upset_plot_1", "Upset Plot") # nolint object_usage
    ),
    shinydashboard::tabItem(tabName = "freq_outcome_1",
      reactive_page("freq_outcome_1",
      "Outcome Measure and Model Settings") # nolint object_usage
    ),
    shinydashboard::tabItem(tabName = "freq_exclude_1",
      reactive_page("freq_exclude_1",
      "Exclude Studies") # nolint object_usage
    ),
    shinydashboard::tabItem(tabName = "forest_plot_1",
      reactive_page("forest_plot_1",
      "Forest Plot") # nolint object_usage
    ),
    tabItem(tabName = "privacy_policy",
      static_page_ui("privacy_policy", "md/privacy_policy.md") # nolint object_usage
    ),
    tabItem(tabName = "readme",
      static_page_ui("readme", "readme.md") # nolint object_usage
    )
  )
}