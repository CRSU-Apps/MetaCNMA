renderSideBar <- function() {
  sidebarMenu(
    id = "tabs",
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem(
      "Data",
      icon = icon("database"),
      startExpanded = T,
      menuSubItem("Upload Data", tabName = "dataUpload", icon = icon("file")),
      menuSubItem("View Data", tabName = "viewData", icon = icon("table")),
      menuSubItem(
        "Help",
        tabName = "dataHelp",
        icon = icon("question")
      )
    ),
    menuItem(
      "Frequentist Analysis",
      icon = icon("microscope"),
      startExpanded = T,
      menuSubItem(
        "Outcome",
        tabName = "freqOutcome",
        icon = icon("equals")
      ),
      menuSubItem(
        "Exclude Studies",
        tabName = "freqExclude",
        icon = icon("text-slash")
      ),
      menuSubItem(
        "Data Summary",
        tabName = "dataSummary",
        icon = icon("clipboard-list")
      ),
      menuSubItem(
        "Forest Plot",
        tabName = "forestPlot",
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