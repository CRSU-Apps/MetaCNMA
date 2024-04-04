##################################################################
##                         Shiny Server                         ##
##                          CRSU CNMA                           ##
##                          Ryan Field                          ##
##################################################################
shinyServer(function(input, output, session) {

  # Reactive Values
  # Current tab as a reactive
  tab <- reactive(input$tabs)
  log <- reactiveValues()
  load_default_data <- reactiveVal(TRUE)

  cookie_server(
    id = "cookies",
    cookies = reactive(input$cookies),
    google_analytics_id = "G-DYBCT85P4W",
    tab = tab
  )

  data_type <- data_type_module_server("data_type")

  home_tab_server("home_tab")

  data_reactives <- data_upload_tab_server(
    "data_upload_tab",
    data_type,
    load_default_data
  )

  view_data_tab_server(
    "view_data_tab",
    data_reactives,
    tab
  )

  freq_options <- model_outcome_tab_server(
    "freq_outcome_tab",
    data_reactives,
    tab
  )

  freq_reactives <- freq_analysis_server(
    "freq_analysis",
    data_reactives,
    freq_options
  )

  data_summary_tab_server(
    "data_summary",
    data_reactives,
    freq_options,
    freq_reactives,
    tab
  )

  network_plot_tab_server(
    "net_graph",
    data_reactives,
    freq_options,
    freq_reactives,
    tab
  )

  correlation_plot_tab_server(
    "correlation_plot",
    data_reactives,
    freq_options,
    freq_reactives,
    tab
  )

  upset_plot_tab_server(
    "upset_plot",
    data_reactives,
    freq_options,
    freq_reactives,
    tab
  )

  freq_sens_data_reactives <- exclude_tab_server(
    "freq_exclude",
    data_reactives,
    freq_options,
    tab
  )

  freq_sens_reactives <- freq_analysis_server(
    "freq_sens_analysis",
    freq_sens_data_reactives,
    freq_options
  )

  forest_plot_tab_server(
    "freq_forest_plot",
    data_reactives,
    freq_options,
    freq_reactives,
    freq_sens_reactives,
    tab
  )

  bayesian_options <- model_outcome_tab_server(
    "bayesian_outcome_tab",
    data_reactives,
    tab,
    freq = FALSE
  )

  bayesian_reactives <- bayesian_analysis_server(
    "bayesian_analysis",
    data_reactives,
    bayesian_options
  )

  shared_stan_settings <- shared_stan_settings_server(
    "stan_settings"
  )

  bayes_sens_data_reactives <- exclude_tab_server(
    "bayesian_exclude",
    data_reactives,
    bayesian_options,
    tab
  )

  bayesian_sens_reactive <- bayesian_analysis_server(
    "bayesian_sens_analysis",
    bayes_sens_data_reactives,
    bayesian_options
  )

  bayes_forest_plot_tab_server(
    "bayesian_forest_plot",
    data_reactives,
    bayesian_options,
    bayesian_reactives,
    bayes_sens_data_reactives,
    bayesian_sens_reactive,
    shared_stan_settings,
    tab
  )

  model_diagnostics_tab_server(
    "model_diagnostics",
    data_reactives,
    bayesian_options,
    bayesian_reactives,
    bayes_sens_data_reactives,
    bayesian_sens_reactive,
    shared_stan_settings,
    tab
  )

})