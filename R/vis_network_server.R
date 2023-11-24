vis_network_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      shiny::actionButton(
        ns("reload_button"),
        "",
        shiny::icon("rotate-right"),
        style =
          "color: #fff;
          background-color:
          #dc3545;
          border-color: #dc3545;
          float: right;"
      )
    ),
    visNetwork::visNetworkOutput(ns("network"), height = "75vh")
  )
}

vis_network_server <- function(id, nm, components) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {
      `%>%` <- magrittr::`%>%`
      ns <- session$ns
      print("renderinet_graph_obj visNetwork")
      # Use netgraph to get node and edges
      net_graph_obj <- netmeta::netgraph(
        nm,
        seq = components
      )

      nodes <- data.frame(
        id = seq_len(length(net_graph_obj$nodes$labels)),
        label = net_graph_obj$nodes$labels
      )

      # Function to map label to id
      label_to_id <- function(label) {
        return(nodes[nodes$label == label, ]$id)
      }

      # Vectorise the function to use with tidydata
      vlab <- Vectorize(label_to_id)

      # Refine edges maping from netgraph
      edges <- data.frame(
        to = net_graph_obj$edges$treat1,
        from = net_graph_obj$edges$treat2
      ) %>%
        dplyr::mutate(
          to = vlab(to),
          from = vlab(from)
        ) %>%
        dplyr::select(to, from)

      vn <- visNetwork::visNetwork(nodes, edges) %>%
        visNetwork::visIgraphLayout(layout = "layout_in_circle") %>%
        visNetwork::visInteraction(navigationButtons = TRUE) %>%
        visNetwork::visExport()

      shiny::observeEvent(input$reload_button, {
        print("Reloading")
        print(ns("network"))
        output$network <- NULL
        output$network <- visNetwork::renderVisNetwork(
          vn
        )
      })

      output$network <- visNetwork::renderVisNetwork(
        vn
      )

    }
  )
}