##################################################################
##                          Shiny Page                          ##
##                         Fluid Layout                         ##
##################################################################
shinyUI(
  # Use a fluid page (bootstap 3)
  fluidPage(
      # HTML head tags
    tags$head(
      # load custom stylesheet
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
      # load custom JS
      tags$script(src="js.cookie.min.js"),
      tags$script(src="app.js"),
      # load favicon from html
      includeHTML("www/favicon/favicon.html"),
      # SEO meta tags (partially redundant)
      tags$meta(name="description", content=getDescription()),
      tags$meta(name="keywords", content=getKeywords()),
      # Open Graph Tags
      tags$meta(property="og:title", content=getTitle()),
      tags$meta(property="og:description", content=getDescription()),
      tags$meta(property="og:image", content="images/MetaCNMALogo.png")
    ),
    
    # Set up Shinyjs
    useShinyjs(),
    
    # Use dashboardPage from shinydashboardPlus
    shinydashboardPlus::dashboardPage(
      header = shinydashboardPlus::dashboardHeader(title = getTitle(), titleWidth = 300), # Header with title
      sidebar = shinydashboardPlus::dashboardSidebar(width = 300, # Sidebar (Navigation)
                                 renderSideBar(), # Render sidebar from R/render_sidebar.R
                                 id = "sidebar" # ID for access from modules
      ),
      body = shinydashboard::dashboardBody(
        renderBody() # Render body from R/render_body.R
      ),
      controlbar = shinydashboardPlus::dashboardControlbar(
        skin = "dark"),
      footer = shinydashboardPlus::dashboardFooter(renderFooter()), # Render footer from R/render_footer.R
      title = getTitle() # Get title from R/utils.R
    )
  )
)