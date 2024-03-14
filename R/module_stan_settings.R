stan_settings_ui <- function(
  id
) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("stan_settings"))
}

stan_settings_server <- function(
  id,
  bayesian_ready,
  bayesian_reactives,
  shared_stan_settings
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {

      `%>%` <- magrittr::`%>%`

      ns <- session$ns

      shiny::observe({
        shiny::req(
          bayesian_ready(),
          cancelOutput = TRUE
        )
        output$stan_settings <- shiny::renderUI(
          shiny::tagList(
            shiny::div(
              shinyWidgets::dropdown(
                shinyWidgets::autonumericInput(
                  ns("chains"),
                  label = "No. of Chains",
                  value = shared_stan_settings$chains(),
                  decimalPlaces = 0
                ),
                shinyWidgets::autonumericInput(
                  ns("warmup"),
                  label = "Warmup (Burn-in)",
                  value = shared_stan_settings$warmup(),
                  decimalPlaces = 0
                ),
                shinyWidgets::autonumericInput(
                  ns("iter"),
                  label = "No. of Iterations",
                  value = shared_stan_settings$iter(),
                  decimalPlaces = 0
                ),
                shinyWidgets::autonumericInput(
                  ns("seed"),
                  label = "Seed",
                  value = shared_stan_settings$seed(),
                  decimalPlaces = 0,
                  digitGroupSeparator = ""
                ),
                shinyWidgets::autonumericInput(
                  ns("max_treedepth"),
                  label = "Maximum Tree Depth",
                  value = shared_stan_settings$max_treedepth(),
                  decimalPlaces = 0
                ),
                shinyWidgets::autonumericInput(
                  ns("adapt_delta"),
                  label = "Adapt Delta",
                  value = shared_stan_settings$adapt_delta()
                ),
                shinyWidgets::autonumericInput(
                  ns("stepsize"),
                  label = "Step Size",
                  value = shared_stan_settings$stepsize()
                ),
                shiny::actionButton(
                  ns("update_button"),
                  "Update Sampler Settings",
                  shiny::icon("rotate-right"),
                  style =
                    "
                      color: #fff;
                      background-color:#dc3545;
                      border-color:#dc3545;
                      float: left;
                    "
                ),
                shiny::div(
                  class = "clearfix"
                ),
                size = "md",
                icon = shiny::icon("gear"),
                status = "primary",
                right = TRUE,
                label = "Stan Sampler Options"
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

      shiny::observe({
        shared_stan_settings$chains(input$chains)
        shared_stan_settings$warmup(input$warmup)
        shared_stan_settings$iter(input$iter)
        shared_stan_settings$seed(input$seed)
        shared_stan_settings$max_treedepth(input$max_treedepth)
        shared_stan_settings$adapt_delta(input$adapt_delta)
        shared_stan_settings$stepsize(input$stepsize)
        shared_stan_settings$update_settings(shared_stan_settings$update + 1)
        bayesian_reactives$model(NULL)
      }) %>% shiny::bindEvent(
        input$update_button
      )

      shiny::observe({
        print("Updating Stan Settings")
        shinyWidgets::updateAutonumericInput(
          inputId = ns("chains"),
          value = shared_stan_settings$chains()
        )
        shinyWidgets::updateAutonumericInput(
          inputId = ns("warmup"),
          value = shared_stan_settings$warmup()
        )
        shinyWidgets::updateAutonumericInput(
          inputId = ns("iter"),
          value = shared_stan_settings$iter()
        )
        shinyWidgets::updateAutonumericInput(
          inputId = ns("seed"),
          value = shared_stan_settings$seed()
        )
        shinyWidgets::updateAutonumericInput(
          inputId = ns("max_treedepth"),
          value = shared_stan_settings$max_treedepth()
        )
        shinyWidgets::updateAutonumericInput(
          inputId = ns("adapt_delta"),
          value = shared_stan_settings$adapt_delta()
        )
        shinyWidgets::updateAutonumericInput(
          inputId = ns("stepsize"),
          value = shared_stan_settings$stepsize()
        )
      }) %>% shiny::bindEvent(
        shared_stan_settings$update_settings()
      )

      #return(shared_stan_settings())

    }
  )
}