home_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1(paste(site_info$title, site_info$version, sep = " ")), # nolint object_usage
    shiny::img(
      src = "images/MetaCNMALogo.png",
      alt = "Logo for Meta CNMA App",
      class = "img-responsive center-block",
      id = "home-logo"
    ),
    shiny::fluidRow(shiny::column(
      width = 6,
      offset = 3
    ))
  )
}

home_tab_server <- function(
  id
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

    }
  )
}
