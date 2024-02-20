render_sidebar <- function() {
  shinydashboard::sidebarMenu(
    id = "tabs",
    shinydashboard::menuItem("Home",
      tabName = "home_tab",
      icon = shiny::icon("home")
    ),
    shinydashboard::menuItem(
      "Data",
      icon = shiny::icon("database"),
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
      shinydashboard::menuSubItem(
        "Data Summary",
        tabName = "data_summary",
        icon = shiny::icon("clipboard-list")
      ),
      shinydashboard::menuSubItem(
        "Network Diagram",
        tabName = "net_graph",
        icon = shiny::icon("circle-nodes")
      ),
      shinydashboard::menuSubItem(
        "Correlation Plot and Heatmap",
        tabName = "correlation_plot",
        icon = shiny::icon("chart-simple")
      ),
      shinydashboard::menuSubItem(
        "Upset Plot",
        tabName = "upset_plot",
        icon = shiny::icon("chart-bar")
      )
    ),
    shinydashboard::menuItem(
      "Frequentist Analysis",
      icon = icon("microscope"),
      startExpanded = TRUE,
      shinydashboard::menuSubItem(
        "Outcome & Model Settings",
        tabName = "freq_outcome_tab",
        icon = shiny::icon("equals")
      ),
      shinydashboard::menuSubItem(
        "Sensitivity Analysis: Exclude Studies",
        tabName = "freq_exclude",
        icon = shiny::icon("text-slash")
      ),
      shinydashboard::menuSubItem(
        "Forest Plot",
        tabName = "freq_forest_plot",
        icon = shiny::icon("chart-gantt")
      )
    ),
    shinydashboard::menuItem(
      "Bayesian Analysis",
      icon = shiny::icon("magnifying-glass-chart"),
      startExpanded = TRUE,
      shinydashboard::menuSubItem(
        "Outcome & Model Settings",
        tabName = "bayesian_outcome_tab",
        icon = shiny::icon("equals")
      ),
      shinydashboard::menuSubItem(
        "Sensitivity Analysis: Exclude Studies",
        tabName = "bayesian_exclude",
        icon = shiny::icon("text-slash")
      ),
      shinydashboard::menuSubItem(
        "Forest Plot",
        tabName = "bayesian_forest_plot",
        icon = shiny::icon("chart-gantt")
      ),
      shinydashboard::menuSubItem(
        "Model Diagnostics",
        tabName = "model_diagnostics",
        icon = shiny::icon("stethoscope")
      )
    ),
    shinydashboard::menuItem(
      "Readme",
      tabName = "readme",
      icon = shiny::icon("readme")
    )
  )
}