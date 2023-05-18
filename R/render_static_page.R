renderStaticPageUI <- function(id, markDownFile) {
  ns <- NS(id)
  tagList(
    if(!is.null(markDownFile) & file.exists(markDownFile)){
      includeMarkdown(markDownFile)
    }
  )
}

renderStaticPageServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # Do nothing
    }
  )
}