stan_settings_ui <- function(
  id
) {
  ns <- shiny::NS(id)
  shiny::uiOutput("stan_settings")
}

stan_settings_server <- function(
  id,
  bayesian_analysis,
  bayesian_ready
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      `%>%` <- magrittr::`%>%`

      ns <- session$ns

      stan_settings <- shiny::reactiveValues()

      shiny::observe({
        output$stan_settings <- shiny::renderUI(
          shiny::tagList(
            shiny::div(
              shinyWidgets::dropdown(
                shiny::textInput(
                  ns("chains"),
                  label = "No. of Chains",
                  value = "3"
                ),
                shiny::textInput(
                  ns("warmup"),
                  label = "Warmup (Burn-in)",
                  value = "1000"
                ),
                shiny::textInput(
                  ns("iter"),
                  label = "No. of Iterations",
                  value = "3000"
                ),
                shiny::textInput(
                  ns("seed"),
                  label = "Seed",
                  value = "12345"
                ),
                shiny::textInput(
                  ns("max_treedepth"),
                  label = "Maximum Tree Depth",
                  value = "10"
                ),
                shiny::textInput(
                  ns("adapt_delta"),
                  label = "Adapt Delta",
                  value = "0.95"
                ),
                shiny::textInput(
                  ns("Step_Siz"),
                  label = "Step Size",
                  value = "0.01"
                ),
                shiny::uiOutput(ns("update_button")),
                shiny::div(
                  class = "clearfix"
                ),
                size = "sm",
                icon = shiny::icon("gear"),
                status = "primary",
                right = TRUE
              ),
              style =
                "float: right;"
            ),
            shiny::div(class = "clearfix")
          )
        )
      }) %>% shiny::bindEvent(
        bayesian_ready()
      )

      return(stan_settings)

    }
  )
}