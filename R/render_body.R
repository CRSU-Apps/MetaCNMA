render_body <- function() {

  shinydashboard::tabItems(
    shinydashboard::tabItem(tabName = "home_tab",
      home_tab_ui("home_tab") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "data_upload_tab",
      data_upload_tab_ui("data_upload_tab") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "view_data_tab",
      view_data_tab_ui("view_data_tab") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "data_help",
      data_help_tab() # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "data_summary",
      data_summary_tab_ui("data_summary") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "net_graph",
      network_plot_tab_ui("net_graph") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "correlation_plot",
      correlation_plot_tab_ui("correlation_plot") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "upset_plot",
      upset_plot_tab_ui("upset_plot") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "freq_outcome_tab",
      model_outcome_tab_ui("freq_outcome_tab") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "freq_exclude",
      exclude_tab_ui("freq_exclude") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "freq_model_output",
      freq_model_output_tab_ui("freq_model_output") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "freq_forest_plot",
      forest_plot_tab_ui("freq_forest_plot") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "bayesian_outcome_tab",
      model_outcome_tab_ui("bayesian_outcome_tab") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "bayesian_exclude",
      exclude_tab_ui("bayesian_exclude") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "bayesian_forest_plot",
      bayes_forest_plot_tab_ui("bayesian_forest_plot") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "bayesian_model_output",
      bayes_model_output_tab_ui("bayesian_model_output") # nolint object_name
    ),
    shinydashboard::tabItem(tabName = "model_diagnostics",
      model_diagnostics_tab_ui("model_diagnostics") # nolint object_name
    ),
    tabItem(tabName = "readme",
      static_page_ui("readme", "README.md") # nolint object_name
    )
  )
}