vis_network_server <- function(id, nm, components) {
  shiny::moduleServer(
    id,
    function(input,
             output,
             session) {
      `%>%` <- magrittr::`%>%`
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

      vis_network_obj <- visNetwork::visNetwork(nodes, edges) %>%
        visNetwork::visIgraphLayout(layout = "layout_in_circle") %>%
        visNetwork::visOptions(collapse = TRUE, nodesIdSelection = TRUE)
      return(vis_network_obj)

    }
  )
}