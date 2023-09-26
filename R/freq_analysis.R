require(dplyr)
freq_pairwise <- function(reactive_data, reactive_freq) {
  tryCatch({
    # Reset pairwise
    reactive_freq()$pairwise(NULL)
    # If the data is not valid do not format the data
    if (! reactive_data()$valid()) {
      print("This error occured trying to format the data")
      stop("There is a problem with the data, 
      please check data has been uploaded and is valid")
    }
    # If the data has not been formatted yet, format it now.
    if (is.null(reactive_freq()$formatted_data())) {
      shiny::withProgress({
        format_data(reactive_data, reactive_freq) # nolint: object_usage
      },
      message = "Formatting Data")
    }
    # Initialise temporary dataframe
    tmp_data <- NULL
    if (reactive_data()$format() == "wide") {
      tmp_data <- data_wide_to_long(reactive_freq()$formatted_data())
    } else if (reactive_data()$format() == "long") {
      tmp_data <- reactive_freq()$formatted_data()
    } else {
      stop("Error: DF001 data format unknown")
    }
    print("running pairwise")
    if (reactive_data()$data_type() == "continuous") {
      reactive_freq()$pairwise(run_pairwise_continuous(tmp_data))
    } else if (reactive_data()$data_type() == "binary") {
      reactive_freq()$pairwise(run_pairwise_binary(tmp_data))
    } else {
      stop("Error: DT001 data type unknown")
    }
  },
  error = function(e) {
    error_alert(e$message)
    invalidate_reactive(reactive_data, reactive_freq)
    return(FALSE)
  })
}

run_pairwise_continuous <- function(df) {

  netmeta::pairwise(
    treat = components,
    n = total,
    mean = mean,
    sd = sd,
    studlab = study,
    data = df
  )

}

run_pairwise_binary <- function(df) {

  netmeta::pairwise(
    treat = components,
    n = total,
    event = events,
    studlab = study,
    data = df
  )

}

get_components <- function(pw) {
  # Get all the components (treat columns)
  components <- pw %>% select(contains("treat"))
  tmp_comp <- NULL
  for (i in seq_len(ncol(components))) {
    tmp_comp <- cbind(tmp_comp, components[[i]])
  }
  components <- tmp_comp
  # Add all components to a single character
  # vector separating with extra + signs
  components <- paste(components, collapse = "+")
  # Split the single vector apart by +
  components <- strsplit(components, "\\+")[[1]]
  # Convert to factor (for speed)
  components <- as.factor(components)
  # Use levels to get unique components
  components <- levels(components)
}

get_components_no_reference <- function(pw) {
  # Get all components
  components <- get_components(pw)
  # Get the reference component
  reference <- get_most_freq_component(pw)
  # Remove the reference component
  components <- components[! components == reference]
  return(components)
}

get_combination_components <- function(pw) {
  components <- pw %>% select(contains("treat"))
  tmp_comp <- NULL
  for (i in seq_len(ncol(components))) {
    tmp_comp <- cbind(tmp_comp, components[[i]])
  }
  components <- as.factor(tmp_comp)
  level_list <- list()
  for (i in seq_len(length(levels(components)))) {
    level_list[levels(components)[[i]]] <- 0
  }
  for (component in components){
    level_list[[component]] <-  level_list[[component]] + 1
  }
  return(level_list)
}

get_summary <- function(pw) {
  list(
    n_studies = nrow(pw),
    components = get_components(pw),
    combination_components = get_combination_components(pw)
  )
}

component_summary_as_df <- function(component_summary){
  components <- tibble::as_tibble_col(
    as.numeric(component_summary), column_name = "Number of Studies"
  )
  components <- as_tibble(components)
  components <- cbind(
    `Combination of Components` = names(component_summary),
    components
  )
  components <- components %>% arrange(desc(`Number of Studies`))
  return(components)
}

render_freq_summary <- function(pw, n_connection) {
  pw_summary <- get_summary(pw)
  print(pw_summary)
  shiny::renderUI(
    shiny::tagList(
      shiny::tags$ul(
        shiny::tags$li(paste0("Number of Studies: ",
          pw_summary$n_studies
        )),
        shiny::tags$li(paste0("Number of Components: ",
          length(pw_summary$components)
        )),
        shiny::tags$li(paste0("Components: ",
          paste0(pw_summary$components, collapse = ", ")
        )),
        shiny::tags$li(paste0("Is the Network Connected: ",
          !n_connection$details.disconnected
        ))
      ),
      DT::renderDataTable(
        component_summary_as_df(pw_summary$combination_components),
        filter = "top",
        options = list(scrollX = TRUE,
          pageLength = 10,
          info = FALSE,
          lengthMenu = list(c(10, -1), c("10", "All"))
        )
      )
    )
  )
}

get_most_freq_component <- function(pw) {
  pw_summary <- get_summary(pw)
  component_summary <- component_summary_as_df(
    pw_summary$combination_components
  )
  component_summary$`Combination of Components`[1]
}

run_net_connection <- function(pw) {
  return(netmeta::netconnection(pw))
}

run_netmeta <- function(pw, ref = "Control", random_eff = FALSE) {
  print("running netmeta")
  return(netmeta::netmeta(pw, ref = ref, comb.random = random_eff))
}

render_netplot <- function(nm) {
  shiny::renderPlot(
    netmeta::netgraph(nm)
  )
}

run_netcomb <- function(nm, inactive = "Control") {
  print("running netcomb")
  return(netmeta::netcomb(nm, inactive = inactive))
}

netcomb_summary <- function(nc) {
  nc.summary <- summary(nc)
  data.frame(
    "Characteristic" = c(
      "Number of Studies",
      "Number of Components",
      "Number of Interventions"
    ),
    "Value" = c(
      nc$k,
      nc$c,
      nc$n
    )
  )
}

render_net_graph <- function(nm, components) {
  print("Rendering netgraph")
  shiny::renderPlot(
    netmeta::netgraph(
      nm,
      plastic = FALSE,
      col = "black",
      points = TRUE,
      col.points = "blue",
      number.of.studies = FALSE,
      seq = components,
      thickness = "number.of.studies",
      cex.points = 2,
      offset = 0.05,
      scale = 0.7,
      cex = 0.8

    )
  )
}

render_correlation_plot <- function(data, components) {
  print(components)
  components <- levels(as.factor(components))
  components <- paste(components, collapse = "+")
  components <- strsplit(components, "\\+")[[1]]
  # Convert to factor (for speed)
  components <- as.factor(components)
  # Use levels to get unique components
  components <- levels(components)

  tmp_df <- dplyr::select(data, study, components)

  study_components <- setNames(
    data.frame(
      matrix(ncol = length(components), nrow = nrow(tmp_df))
    ),
    as.character(components)
  )

  for (i in seq_len(nrow(tmp_df))) {
    tmp_components <- list()
    for (component in components) {
      current_components <- tmp_df[i, ]$components
      current_components <- paste(current_components, collapse = "+")
      current_components <- strsplit(current_components, "\\+")[[1]]
      tmp_components[component] <- ifelse(
        component %in% current_components, 1, 0
      )
    }
    study_components[i, ] <- rbind(tmp_components)
  }

  y <- cor(study_components)

  shiny::renderPlot(
    # create correlation plot:
    corrplot::corrplot(
      y,
      method = "color",
      type = "upper",
      diag = FALSE,
      tl.cex = 1,
      tl.col = "black",
      number.cex = 1,
      number.font = 2,
      number.digits = 2,
      col = corrplot::COL2("PRGn"),
      addCoef.col = "black"
    )
  )
}

render_net_forest <- function(
  nc,
  data_type,
  outcome_measure = "Outcome Measure",
  component_labels = NULL,
  component = TRUE
) {
  if (component) {
    print("rendering plot")
    return(
      shiny::renderPlot(
        metafor::forest(
          ifelse(rep(nc$random, length(nc$comps)),
            nc$Comp.random, nc$Comp.common
          ),
          se = ifelse(rep(nc$random, length(nc$comps)),
            nc$seComp.random, nc$seComp.common
          ),
          slab = ifelse(rep(!is.null(component_labels), length(nc$comps)),
            component_labels, nc$comps
          ),
          xlab = outcome_measure,
          refline = ifelse(data_type == "binary", 1, 0),
          transf = ifelse(data_type == "binary",
            exp,
            function(x){x} # nolint: brace_linter
          ),
          header = c("Component", paste0(outcome_measure, " (95% CI)"))
        )
      )
    )
  }
  shiny::renderPlot(metafor::forest(nc))
}
