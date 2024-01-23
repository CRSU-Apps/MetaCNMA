site_info <- read_yaml("site_info.yaml")

get_site_info_property <- function(property) {
  tryCatch({
    if (is.null(site_info[property])) {
      return("")
    }
    return(site_info[property])
  },
  error = function(e) {
    return("")
  },
  warning = function(w) {
    return("")
  })
}

get_title <- function(){
  return(as.character(get_site_info_property("title")))
}

get_version <- function(){
  return(as.character(get_site_info_property("version")))
}

get_accepted_file_formats <- function(){
  return(get_site_info_property("accepted_file_formats")[[1]])
}

get_cookie_message <- function(){
  shiny::includeHTML("html/cookie.html")
}

get_description <- function(){
  return(as.character(get_site_info_property("description")))
}

get_keywords <- function() {
  return(paste(get_site_info_property("keywords")[[1]], collapse = ","))
}

get_required_binary_long_columns <- function() { # nolint
  return(get_site_info_property("required_binary_long_columns")[[1]])
}

get_required_continuous_long_columns <- function() { # nolint
  return(get_site_info_property("required_continuous_long_columns")[[1]])
}

get_required_binary_wide_columns <- function() { # nolint
  return(get_site_info_property("required_binary_wide_columns")[[1]])
}

get_required_continuous_wide_columns <- function() { # nolint
  return(get_site_info_property("required_continuous_wide_columns")[[1]])
}

# invalidate_reactive <- function(reactive_data, reactive_freq) {
#   reactive_data()$invalidate()
#   reactive_freq()$invalidate()
# }

get_outcome_measure <- function(outcome_measure) {
  if (is.null(outcome_measure)) {
    return("Outcome Measure")
  }
  dplyr::case_when(
    outcome_measure == "md" ~ "Mean Difference (MD)",
    outcome_measure == "smd" ~ "Standardised Mean Difference (SMD)",
    outcome_measure == "or" ~ "Odds Ratio (OR)",
    outcome_measure == "rr" ~ "Risk Ratio (RR)",
    outcome_measure == "rd" ~ "Risk Difference (RD)",
    TRUE ~ "Outcome Measure"
  )
}

as_logical <- function(x) {
  print(x)
  if (!is.numeric(x)) {
    stop("Input must be numerical")
  } else if (x == 1) {
    return(TRUE)
  } else if (x == 0) {
    return(FALSE)
  }
  stop("Input must be 1 or 0")
}