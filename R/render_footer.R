renderFooter <- function() {
  tagList(
    fluidRow(
      column(
        width = 6,
        tags$a(href = "https://www.nihr.ac.uk/", target = "_blank",
           img(src = "images/funded-by-nihr-logo.png",
               alt = "Funded by NIHR",
               class = "footer-img nihr-img img-responsive center-block"))
      ),
    column(
      width = 6,
      tags$a(href = "https://www.crsu.org.uk/", target = "_blank",
           img(src = "images/CRSU_logo.png",
               alt = "CRSU Logo",
               class = "footer-img crsu-img img-responsive center-block")),
    ),
    column(
      width = 12,
      includeMarkdown("md/funding_statement.md"),
    )
  ),
  shiny::div(
          class = "clearfix"
        ),
  fluidRow(column(
    width = 6,
    tags$a(href = "https://www.gla.ac.uk/", target = "_blank",
           img(src = "images/university-of-glasgow-logo.png", 
               alt = "University of Glasgow", 
               class = "footer-img img-responsive center-block")
    )),
    column(
      width = 6,
      tags$a(href = "https://le.ac.uk/", target = "_blank",
             img(src = "images/university-of-leicester-logo.jpg",
                 alt = "University of Leicester",
                 class = "footer-img img-responsive center-block"))
    )))
}