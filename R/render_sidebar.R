render_sidebar <- function() {
  shinydashboard::sidebarMenu(
    id = "tabs",
    shinydashboard::menuItem("Home",
      tabName = "home_1",
      icon = shiny::icon("home")
    ),
    shinydashboard::menuItem(
      "Data",
      icon = icon("database"),
      startExpanded = TRUE,
      shinydashboard::menuSubItem("Upload Data",
        tabName = "data_upload_1",
        icon = shiny::icon("file")
      ),
      shinydashboard::menuSubItem(
        "View Data",
        tabName = "view_data_1",
        icon = shiny::icon("table")
      ),
      shinydashboard::menuSubItem(
        "Help",
        tabName = "data_help_1",
        icon = shiny::icon("question")
      )
    ),
    menuItem(
      "Frequentist Analysis",
      icon = icon("microscope"),
      startExpanded = TRUE,
      menuSubItem(
        "Outcome & Model Settings",
        tabName = "freq_outcome_1",
        icon = icon("equals")
      ),
      menuSubItem(
        "Exclude Studies",
        tabName = "freq_exclude_1",
        icon = icon("text-slash")
      )#,
    #   menuSubItem(
    #     "Data Summary",
    #     tabName = "dataSummary_1",
    #     icon = icon("clipboard-list")
    #   ),
    #   menuSubItem(
    #     "Forest Plot",
    #     tabName = "forestPlot_1",
    #     icon = icon("chart-bar")
    #   )
    ),
    shinydashboard::menuItem(
      "Privacy Policy",
      tabName = "privacy_policy",
      icon = icon("lock")
    ),
    shinydashboard::menuItem(
      "Readme",
      tabName = "readme",
      icon = shiny::icon("readme")
    )
  )
}