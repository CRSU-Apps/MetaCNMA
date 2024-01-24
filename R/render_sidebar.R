render_sidebar <- function() {
  shinydashboard::sidebarMenu(
    id = "tabs",
    shinydashboard::menuItem("Home",
      tabName = "home_tab",
      icon = shiny::icon("home")
    ),
    shinydashboard::menuItem(
      "Data",
      icon = icon("database"),
      startExpanded = TRUE,
      shinydashboard::menuSubItem("Upload Data",
        tabName = "data_upload_tab",
        icon = shiny::icon("file")
      ),
      shinydashboard::menuSubItem(
        "View Data",
        tabName = "view_data_tab",
        icon = shiny::icon("table")
      ),
      shinydashboard::menuSubItem(
        "Help",
        tabName = "data_help",
        icon = shiny::icon("question")
      )
    ),
    shinydashboard::menuItem(
      "Component Summary",
      icon = shiny::icon("puzzle-piece"),
      startExpanded = TRUE,
      menuSubItem(
        "Data Summary",
        tabName = "data_summary",
        icon = icon("clipboard-list")
      ),
      menuSubItem(
        "Network Diagram",
        tabName = "net_graph",
        icon = icon("circle-nodes")
      ),
      menuSubItem(
        "Correlation Plot and Heatmap",
        tabName = "correlation_plot",
        icon = icon("chart-simple")
      ),
      menuSubItem(
        "Upset Plot",
        tabName = "upset_plot",
        icon = icon("chart-bar")
      )
    ),
    menuItem(
      "Frequentist Analysis",
      icon = icon("microscope"),
      startExpanded = TRUE,
      menuSubItem(
        "Outcome & Model Settings",
        tabName = "freq_outcome_tab",
        icon = icon("equals")
      ),
    #   menuSubItem(
    #     "Exclude Studies",
    #     tabName = "freq_exclude_1",
    #     icon = icon("text-slash")
    #   ),
      menuSubItem(
        "Forest Plot",
        tabName = "freq_forest_plot",
        icon = icon("chart-gantt")
      )
    #),
    # shinydashboard::menuItem(
    #   "Privacy Policy",
    #   tabName = "privacy_policy",
    #   icon = icon("lock")
    # ),
    # shinydashboard::menuItem(
    #   "Readme",
    #   tabName = "readme",
    #   icon = shiny::icon("readme")
    )
  )
}