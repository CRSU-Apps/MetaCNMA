home_tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1(paste(site_info$title, site_info$version, sep = " ")), # nolint object_name
    shiny::img(
      src = "images/MetaCNMALogo.png",
      alt = "Logo for Meta CNMA App",
      class = "img-responsive center-block",
      id = "home-logo"
    ),
    shiny::p(
      "Welcome to MetaCNMA, Currently this software is in BETA and is
      offered AS IS without warranty of any kind."
    ),
    shiny::includeMarkdown("md/freq_binary.md"),
    shiny::p(
      "If you use this software please cite it as: 
      Field, R., Freeman, S., Smith, E., & Morris, T. (2024). 
      MetaBayesDTA (v0.2.0-beta). Zenodo. 
      https://doi.org/10.5281/zenodo.13718869"
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
