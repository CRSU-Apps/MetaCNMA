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
      tags$script(src = "js.cookie.min.js"), # JS for cookie (GDPR)
      tags$script(src = "app.js"), # Application JS
      # load favicon from html
      includeHTML("www/favicon/favicon.html"),
      # SEO meta tags (partially redundant)
      # get_description from R/utils.R
      tags$meta(name = "description", content=get_description()),
      # get_keywords() from R/utils.R
      tags$meta(name = "keywords", content=get_keywords()),
      # Open Graph Tags
      # get_title from R/utils.R
      tags$meta(property = "og:title", content = get_title()),
      # get_description from R/utils.R
      tags$meta(property = "og:description", content = get_description()),
      tags$meta(property = "og:image",
        content =
          "https://raw.githubusercontent.com/CRSU-Apps/MetaDTA/main/www/images/MetaCNMALogo.png" # nolint
      ),
    ),

    # Set up Shinyjs
    useShinyjs(),

    # Use dashboardPage from shinydashboardPlus
    shinydashboardPlus::dashboardPage(
      header = shinydashboardPlus::dashboardHeader(
        title = get_title(),
        titleWidth = 300,
        leftUi = data_type_module_ui("data_type")
      ), # Header with title from R/utils.R
      sidebar = shinydashboardPlus::dashboardSidebar(
        width = 300, # Sidebar (Navigation)
        render_sidebar(), # Render sidebar from R/render_sidebar.R
        id = "sidebar" # ID for access from modules
      ),
      body = shinydashboard::dashboardBody(
        render_body() # Render body from R/render_body.R
      ),
      controlbar = shinydashboardPlus::dashboardControlbar(
        skin = "dark"
      ),
      footer = shinydashboardPlus::dashboardFooter(
        renderFooter()
      ), # Render footer from R/render_footer.R
      title = get_title() # Get title from R/utils.R
    )
  )
)