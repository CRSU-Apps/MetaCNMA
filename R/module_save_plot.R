save_plot_ui <- function(
  id,
  output_name = "plot",
  width = "1200",
  height = "1200"
) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      shinyWidgets::dropdown(
        shiny::textInput(
          ns("width"),
          label = "Width (px)",
          value = width
        ),
        shiny::textInput(
          ns("height"),
          label = "Height (px)",
          value = height
        ),
        shiny::textInput(
          ns("filename"),
          label = "File Name",
          value = output_name
        ),
        shinyWidgets::pickerInput(
          ns("output_type"),
          label = "File Type",
          choices = c(
            "pdf" = "pdf",
            "svg" = "svg",
            "png" = "png"
          ),
          selected = "pdf"
        ),
        shiny::uiOutput(ns("download_button")),
        shiny::div(
          class = "clearfix"
        ),
        size = "md",
        icon = shiny::icon("floppy-disk"),
        status = "primary",
        right = TRUE
      ),
      style =
        "float: right;"
    ),
    shiny::div(class = "clearfix")
  )
}

save_plot_server <- function(
  id,
  plot_output,
  is_rendered
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      `%>%` <- magrittr::`%>%`

      ns <- session$ns

      shiny::observe({
        output$download_button <- NULL
        shiny::req(
          is_rendered()
        )
        print("loading download button")
        output$download_button <- shiny::renderUI(
          shiny::downloadButton(
            ns("save_button"),
            icon = shiny::icon("floppy-disk"),
            style = "float: right;"
          )
        )
        output$save_button <- shiny::downloadHandler(
          filename = function() {
            paste(
              input$filename, input$output_type, sep = "."
            )
          },
          content = function(file) {
            if (input$output_type == "pdf") {
              grDevices::pdf(
                file = file,
                width = as.numeric(input$width) / 96,
                height = as.numeric(input$height) / 96,
              )
            } else if (input$output_type == "svg") {
              grDevices::svg(
                file = file,
                width = as.numeric(input$width) / 96,
                height = as.numeric(input$height) / 96,
              )
            } else {
              grDevices::png(
                file = file,
                width = as.numeric(input$width),
                height = as.numeric(input$height),
                res = 92
              )
            }
            plot_output()
            dev.off()
          }
        )
      }) %>% shiny::bindEvent(is_rendered())
    }
  )
}