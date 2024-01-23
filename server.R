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

  freq_options <- freq_outcome_tab_server(
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
    freq_options,
    freq_reactives,
    tab
  )

  network_plot_tab_server(
    "net_graph",
    freq_options,
    freq_reactives,
    tab
  )

  correlation_plot_tab_server(
    "correlation_plot",
    freq_options,
    freq_reactives,
    tab
  )

  upset_plot_tab_server(
    "upset_plot",
    freq_options,
    freq_reactives,
    tab
  )

  # Load the cookie module from R/cookies.R
  # cookieServer(
  #   id = "cookies_1",
  #   cookies = reactive(input$cookies),
  #   open_privacy_policy = reactive(input$open_privacy_policy),
  #   parent_session = reactive(session)
  # )
  # home_tab_server(
  #   "home_1",
  #   reactive_data,
  #   reactive_freq
  # )
  # data_upload_tab_server(
  #   "data_upload_1",
  #   reactive_data,
  #   reactive_freq,
  #   parent_session = reactive(session)
  # )
  # view_data_tab_server(
  #   "view_data_1",
  #   reactive_data,
  #   reactive_freq
  # )
  # freq_outcome_tab_server(
  #   "freq_outcome_1",
  #   reactive_data,
  #   reactive_freq,
  #   tabs
  # )
  # freq_exclude_tab_server(
  #   "freq_exclude_1",
  #   reactive_data,
  #   reactive_freq,
  #   tabs
  # )
  # data_summary_tab_server(
  #   "data_summary_1",
  #   reactive_data,
  #   reactive_freq,
  #   tabs
  # )
  # network_plot_tab_server(
  #   "net_graph_1",
  #   reactive_data,
  #   reactive_freq,
  #   tabs
  # )
  # correlation_plot_tab_server(
  #   "correlation_plot_1",
  #   reactive_data,
  #   reactive_freq,
  #   tabs
  # )
  # upset_plot_tab_server(
  #   "upset_plot_1",
  #   reactive_data,
  #   reactive_freq,
  #   tabs
  # )
  # forest_plot_tab_server(
  #   "forest_plot_1",
  #   reactive_data,
  #   reactive_freq,
  #   tabs
  # )
})