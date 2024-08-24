#################################################################
##          Helper Functions for Frequentist Analysis          ##
#################################################################

freq_pairwise <- function(df, data_type, summary_measure) {
  tryCatch({
    #print("running pairwise")
    if (data_type == "continuous") {
      return(run_pairwise_continuous(df, summary_measure))
    } else if (data_type == "binary") {
      run_pairwise_binary(df, summary_measure)
    } else {
      stop("Error: DT001 data type unknown")
    }
  },
  error = function(e) {
    error_alert(e$message) # nolint: object_name
    return(NULL)
  })
}

run_pairwise_continuous <- function(df, summary_measure = "MD") {

  netmeta::pairwise(
    treat = components, # nolint: object_name
    n = total, # nolint: object_name
    mean = mean,
    sd = sd,
    studlab = study, # nolint: object_name
    data = df,
    sm = summary_measure
  )

}

run_pairwise_binary <- function(df, summary_measure = "OR") {

  netmeta::pairwise(
    treat = components, # nolint: object_name
    n = total, # nolint: object_name
    event = events, # nolint: object_name
    studlab = study, # nolint: object_name
    data = df,
    sm = summary_measure
  )

}

get_components <- function(formatted_df) {
  return(MetaCNMABayes:::get_unique_components(formatted_df$components))
}

get_components_no_reference <- function(formatted_df, reference_component) {
  return(
    MetaCNMABayes:::get_components_no_ref(
      formatted_df$components,
      reference_component
    )
  )
}

get_combination_components <- function(formatted_df) {
  #`%>%` <- magrittr::`%>%`
  #components <- pw %>% dplyr::select(dplyr::contains("treat"))
  components <- as.factor(formatted_df$components)
  # tmp_comp <- NULL
  # for (i in seq_len(ncol(components))) {
  #   tmp_comp <- cbind(tmp_comp, components[[i]])
  # }
  # components <- as.factor(tmp_comp)
  level_list <- list()
  for (i in seq_len(length(levels(components)))) {
    level_list[levels(components)[[i]]] <- 0
  }
  for (component in components){
    level_list[[component]] <-  level_list[[component]] + 1
  }
  return(level_list)
}

get_summary <- function(formatted_df) {
  list(
    n_studies = length(levels(as.factor(formatted_df$study))),
    components = get_components(formatted_df),
    combination_components = get_combination_components(formatted_df)
  )
}

component_summary_as_df <- function(component_summary) {
  `%>%` <- magrittr::`%>%`
  components <- tibble::as_tibble_col(
    as.numeric(component_summary), column_name = "Number of Studies"
  )
  components <- dplyr::as_tibble(components)
  components <- cbind(
    `Combination of Components` = names(component_summary),
    components
  )
  components <- components %>% dplyr::arrange(
    dplyr::desc(`Number of Studies`) # nolint: object_name
  )
  return(components)
}

get_individual_components <- function(formatted_df) {
  return(
    levels(
      as.factor(
        MetaCNMABayes:::get_single_components(formatted_df$components)
      )
    )
  )
}

get_most_freq_component <- function(formatted_df) {
  combination_components <- get_combination_components(formatted_df)
  individual_components <- get_individual_components(formatted_df)
  combination_components <- combination_components[
    names(combination_components) %in% individual_components
  ]
  most_freq_component <- combination_components[
    which.max(combination_components)
  ]
  return(names(most_freq_component))
}

run_net_connection <- function(pw) {
  return(netmeta::netconnection(pw))
}

is_connected <- function(netconnection) {
  if (!is.null(netconnection$n.subnets)) {
    return(netconnection$n.subnets == 1)
  } else {
    return("UNKNOWN")
  }
}

run_freq <- function(
  pw,
  is_network_connected,
  ref,
  random_eff,
  summary_measure
) {
  #print("Fitting Frequentist Model")
  #print(summary_measure)
  if (is_network_connected) {
    nm <- run_netmeta(pw, ref, random_eff, summary_measure)
    nc <- run_netcomb(nm, inactive = ref, summary_measure)
    return(nc)
  } else {
    return(
      run_discomb(pw, ref, random_eff, summary_measure)
    )
  }
}

run_netmeta <- function(pw, ref, random_eff, summary_measure) {
  #print("running netmeta")
  random_eff <- any(as.logical(random_eff))
  return(
    netmeta::netmeta(
      pw,
      ref = ref,
      comb.random = random_eff,
      sm = summary_measure
    )
  )
}

run_netcomb <- function(nm, inactive, summary_measure) {
  #print("running netcomb")
  return(
    netmeta::netcomb(
      nm,
      inactive = inactive,
      sm = summary_measure
    )
  )
}

run_discomb <- function(
  pw,
  ref,
  random_eff,
  summary_measure
) {
  netmeta::discomb(
    TE = pw$TE,
    seTE = pw$TE,
    treat1 = pw$treat1,
    treat2 = pw$treat2,
    studlab = pw$studlab,
    inactive = ref,
    sm = summary_measure,
    random = random_eff,
  )
}

get_study_components <- function(data, components) {
  components <- levels(as.factor(components))
  components <- paste(components, collapse = "+")
  components <- strsplit(components, "\\+")[[1]]
  # Convert to factor (for speed)
  components <- as.factor(components)
  # Use levels to get unique components
  components <- levels(components)

  tmp_df <- dplyr::select(
    data, study, components # nolint: object_name
  )

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
  return(study_components)
}

get_correlation_plot <- function(data, components) {
  study_components <- get_study_components(data, components)

  y <- cor(study_components)
  return(
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

get_heatmap <- function(data, components) {
  study_components <- get_study_components(data, components)
  x <- study_components %>% dplyr::rowwise() %>%
    dplyr::mutate(n_comps = sum(dplyr::c_across(where(is.numeric))))
  n_comps <- length(components)

  # Set up empty matrix for matrix with
  # number of pairs of component combinations
  a <- matrix(nrow = n_comps, ncol = n_comps,
    dimnames = list(as.character(components), as.character(components))
  )

  comp <- as.character(components)

  #   Fill in the matrix:
  for (i in 1: n_comps){
    r <- x[,comp[i]]
    for (j in 1:n_comps){
      c <- x[,comp[j]]
      a[i,j] <- with(x, sum(r == 1 & c == 1))
      rm(j,c)
    }
    rm(i,r)
  }

  # Choose colour scale for heatmap
  col_fun <- circlize::colorRamp2(c(0, 30), c("white", "red"))

  # Render the heatmap
  return(
    ComplexHeatmap::Heatmap(
      a,
      name = "N arms with\ncombination\n",
      col = col_fun,
      cluster_rows = FALSE, 
      cluster_columns = FALSE, 
      row_names_side = "left", 
      row_names_max_width = grid::unit(10, "cm"),
      row_names_gp = grid::gpar(fontsize = 15),
      column_names_side = "bottom",
      column_names_max_height = grid::unit(10, "cm"),
      column_names_gp = grid::gpar(fontsize = 15),
      column_title_gp = grid::gpar(fontsize = 16, fontface = "bold"),
      rect_gp = grid::gpar(type = "none"),
      cell_fun = function(j, i, x, y, w, h, fill) {
        if (i >= j) {
          grid::grid.rect(x, y, w, h,
            gp = grid::gpar(fill = fill, col = fill)
          )
          grid::grid.text(sprintf("%.0f", a[i, j]),
            x, y,
            gp = grid::gpar(fontsize = 15)
          )
        }
      }
    )
  )
}

get_upset_plot <- function(data, components) {
  study_components <- get_study_components(data, components)
  n_components <- ncol(study_components)

  return(
    UpSetR::upset(
      study_components,
      nsets = n_components,
      nintersects = NA,
      number.angles = 0,
      point.size = 2,
      line.size = 0.7,
      order.by = "freq",
      matrix.color = "#1a5276",
      main.bar.color = "#6c3483",
      sets.bar.color = "#138d75",
      set_size.show = TRUE,
      mainbar.y.label = "No. of Trial Arms featuring combination",
      sets.x.label = "No. of Trial Arms featuring component",
      mainbar.y.max = 40,
      text.scale = 1.5
    )
  )
}

get_net_forest <- function(
  nc,
  data_type,
  outcome_measure = "Outcome Measure",
  component_labels = NULL,
  component = TRUE
) {
  if (component) {
    #print("rendering plot")
    return(
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
  } else {
    return(metafor::forest(nc))
  }
}
