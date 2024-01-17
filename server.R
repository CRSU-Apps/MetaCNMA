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

  parent_session <- reactive(session)

  data_type <- home_tab_server("home_tab")

  data_reactives <- data_upload_tab_server(
    "data_upload_tab",
    data_type,
    parent_session
  )

  data <- data_reactives$data
  is_default_data <- data_reactives$is_default_data
  invalidate_count <- data_reactives$invalidate_count

  view_data_tab_server("view_data_tab", data, data_type, invalidate_count)

  freq_outcome_tab_server(
    "freq_outcome_tab",
    data, data_type,
    is_default_data,
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