renderHomeTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    img(
      src = "images/MetaCNMALogo.png",
      alt = "Logo for Meta CNMA App",
      class = "img-responsive center-block",
      id = "home-logo"
    ),
    heading = h1(paste(siteInfo$title, siteInfo$version, sep = " "), class = "text-center"),
    fluidRow(column(
      width = 6,
      radioButtons(
        ns("dataType"),
        "Select the Type of Data",
        c("Continous" = "continous",
          "Binary" = "binary")
      ),
      offset = 3
      
    ))
  )
}

renderHomeTabServer <- function(id, data, freq){
  moduleServer(
    id,
    function(input,
             output,
             session,
             thisId = id,
             globalData = data,
             globalFreq = freq) {
      
      ns <- NS(id)
      
      observe({
        globalData$type = input$dataType
        print(globalData$type)
      }) %>% bindEvent(input$dataType)
      
    }
  )
}