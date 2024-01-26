save_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      shinyWidgets::dropdown(
        shiny::textInput(
          ns("width"),
          label = "Width (px)",
          value = "1200"
        ),
        shiny::textInput(
          ns("height"),
          label = "Height (px)",
          value = "1200"
        ),
        shiny::textInput(
          ns("filename"),
          label = "File Name",
          value = "plot"
        ),
        shinyWidgets::pickerInput(
          ns("output_type"),
          label = "File Type",
          choices = c(
            "png" = "png",
            "pdf" = "pdf"
          ),
          selected = "png"
        ),
        shiny::uiOutput(ns("download_button")),
        shiny::div(
          class = "clearfix"
        ),
        size = "sm",
        icon = shiny::icon("floppy-disk"),
        status = "primary",
        right = TRUE
      ),
      style =
        "float: right;"
    )
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
        print(input$output_type)
        output$download_button <- shiny::renderUI(
          shiny::downloadButton(
            ns("save_button"),
            icon = shiny::icon("floppy-disk"),
            style = "float: right;"
          )
        )
        output$save_button <- shiny::downloadHandler(
          filename = function(){paste(
            input$filename, input$output_type, sep = ".")
          },
          content = function(file) {
            if (input$output_type == "pdf") {
              grDevices::pdf(
                file = file,
                width = as.numeric(input$width) / 96,
                height = as.numeric(input$height) / 96,
              )
            } else {
              grDevices::png(
                file = file,
                width = as.numeric(input$width),
                height = as.numeric(input$height),
                res = 300
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