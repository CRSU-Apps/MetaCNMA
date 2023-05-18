##################################################################
##                          Shiny Page                          ##
##                         Fluid Layout                         ##
##################################################################
shinyUI(fluidPage(
  tags$head(
    # load custom stylesheet
    includeCSS("www/app.css"),
    # load custom JS
    tags$script(src = "js.cookie.min.js"),
    tags$script(src = "app.js"),
    includeHTML("www/favicon/favicon.html")
  ),
  
  useShinyjs(),
  
  dashboardPage(
    header = dashboardHeader(title = getTitle(), titleWidth = 300),
    sidebar = dashboardSidebar(width = 300,
                               renderSideBar(),
                               id = "sidebar"
    ),
    body = dashboardBody(
      renderBody()
    ),
    controlbar = dashboardControlbar(
      skin = "dark"),
    footer = dashboardFooter(renderFooter()),
    title = getTitle()
  )
  
))