static_page_ui <- function(id, mark_down_file) {
  ns <- shiny::NS(id) # nolint object_name
  shiny::tagList(
    if (!is.null(mark_down_file) & file.exists(mark_down_file)) {
      shiny::includeMarkdown(mark_down_file)
    }
  )
}

static_page_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # Do nothing
    }
  )
}