renderDataHelpBinaryTab <- function() {
  tagList(
    h1("Binary Data"),
    box(
      title = "Long Format", 
      closable = FALSE, 
      width = 12,
      height = "500px",
      solidHeader = FALSE, 
      collapsible = TRUE,
      div(class = "cnma-table",
      includeMarkdown("md/binary_long.md"))
    ),
    box(
      title = "Wide Format", 
      closable = FALSE, 
      width = 12,
      height = "500px",
      solidHeader = FALSE, 
      collapsible = TRUE,
      div(class = "cnma-table",
      includeMarkdown("md/binary_wide.md"))
    )
  )}

renderDataHelpContinousTab <- function() {
  tagList(
    h1("Coninous Data"),
    box(
      title = "Long Format", 
      closable = FALSE, 
      width = 12,
      height = "500px",
      solidHeader = FALSE, 
      collapsible = TRUE,
      div(class = "cnma-table",
      includeMarkdown("md/continous_long.md"))
    ),
    box(
      title = "Wide Format", 
      closable = FALSE, 
      width = 12,
      height = "500px",
      solidHeader = FALSE, 
      collapsible = TRUE,
      div(class = "cnma-table",
      includeMarkdown("md/continous_wide.md"))
    )
  )}

renderDataHelpTab <- function(){
  tagList(
    h1("Instructions for data upload", class = "text-center"),
    p("Data needs to be in arm-based format and can be uploaded as either a csv or excel (xlsx)"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Continous data", renderDataHelpContinousTab()),
                tabPanel("Binary Data", renderDataHelpBinaryTab())
    )
    
  )
}