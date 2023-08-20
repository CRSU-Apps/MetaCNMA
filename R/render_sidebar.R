renderSideBar <- function() {
  sidebarMenu(
    id = "tabs",
    menuItem("Home", tabName = "home_1", icon = icon("home")),
    menuItem(
      "Data",
      icon = icon("database"),
      startExpanded = T,
      menuSubItem("Upload Data", tabName = "dataUpload_1", icon = icon("file")),
      menuSubItem("View Data", tabName = "viewData_1", icon = icon("table")),
      menuSubItem(
        "Help",
        tabName = "dataHelp_1",
        icon = icon("question")
      )
    ),
    menuItem(
      "Frequentist Analysis",
      icon = icon("microscope"),
      startExpanded = T,
      menuSubItem(
        "Outcome & Model Settings",
        tabName = "freqOutcome_1",
        icon = icon("equals")
      ),
      menuSubItem(
        "Exclude Studies",
        tabName = "freqExclude_1",
        icon = icon("text-slash")
      ),
      menuSubItem(
        "Data Summary",
        tabName = "dataSummary_1",
        icon = icon("clipboard-list")
      ),
      menuSubItem(
        "Forest Plot",
        tabName = "forestPlot_1",
        icon = icon("chart-bar")
      )
    ),
    menuItem(
      "Privacy Policy",
      tabName = "privacyPolicy",
      icon = icon("lock")
    ),
    menuItem("Readme", tabName = "readme", icon = icon("readme"))
  )
}