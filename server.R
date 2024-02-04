##################################################################
##                         Shiny Server                         ##
##                          CRSU CNMA                           ##
##                          Ryan Field                          ##
##################################################################
shinyServer(function(input, output, session){
  # Reactive Values
  # reactive_data <- Data$new()$reactive()
  # reactive_freq <- Freq$new()$reactive()
  # Tabs as a reactive
  tab <- reactive(input$tabs)
  log <- reactiveValues()

  data_type <- data_type_module_server("data_type", data_type)

  home_tab_server("home_tab")

  data_reactives <- data_upload_tab_server(
    "data_upload_tab",
    data_type,
    parent_session
  )

  view_data_tab_server("view_data_tab", data_reactives)

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

  forest_plot_tab_server(
    "freq_forest_plot",
    data_reactives,
    freq_options,
    freq_reactives,
    tab
  )

  freq_sens_reactive <- exclude_tab_server(
    "freq_exclude",
    data_reactives,
    freq_options,
    freq_reactives,
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

  bayes_forest_plot_tab_server(
    "bayesian_forest_plot",
    data_reactives,
    bayesian_options,
    bayesian_reactives,
    tab
  )

  bayes_sens_reactive <- exclude_tab_server(
    "bayesian_exclude",
    data_reactives,
    bayesian_options,
    bayesian_reactives,
    tab
  )

})