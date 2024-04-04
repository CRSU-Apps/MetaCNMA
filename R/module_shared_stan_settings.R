shared_stan_settings_server <- function(
  id
) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {
      `%>%` <- magrittr::`%>%`

      ns <- session$ns

      shared_stan_settings <- shiny::reactiveValues()

      shared_stan_settings$chains <- shiny::reactiveVal(3)
      shared_stan_settings$warmup <- shiny::reactiveVal(1000)
      shared_stan_settings$iter <- shiny::reactiveVal(3000)
      shared_stan_settings$seed <- shiny::reactiveVal(12345)
      shared_stan_settings$max_treedepth <- shiny::reactiveVal(10)
      shared_stan_settings$adapt_delta <- shiny::reactiveVal(0.95)
      shared_stan_settings$stepsize <- shiny::reactiveVal(0.01)
      shared_stan_settings$update_settings <- shiny::reactiveVal(0)
      return(shared_stan_settings)
    }
  )
}