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
      tags$script(src="js.cookie.min.js"), # JS for cookie (GDPR)
      tags$script(src="app.js"), # Application JS
      # load favicon from html
      includeHTML("www/favicon/favicon.html"),
      # SEO meta tags (partially redundant)
      tags$meta(name="description", content=getDescription()), # getDescription from R/utils.R
      tags$meta(name="keywords", content=getKeywords()), # getKeywords() from R/utils.R
      # Open Graph Tags
      tags$meta(property="og:title", content=getTitle()), # getTitle from R/utils.R
      tags$meta(property="og:description", content=getDescription()), # getDescription from R/utils.R
      tags$meta(property="og:image", content="https://raw.githubusercontent.com/CRSU-Apps/MetaDTA/main/www/images/MetaCNMALogo.png")
    ),
    
    # Set up Shinyjs
    useShinyjs(),
    
    # Use dashboardPage from shinydashboardPlus
    shinydashboardPlus::dashboardPage(
      header = shinydashboardPlus::dashboardHeader(title = getTitle(), titleWidth = 300), # Header with title from R/utils.R
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