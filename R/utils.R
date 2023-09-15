site_info <- read_yaml("site_info.yaml")

get_site_info_property <- function(property) {
  rtn <- tryCatch({
    if (is.null(site_info[property])) {
      return("")
    }
    site_info[property]
  },
  error = function(e) {
    return("")
  },
  warning = function(w) {
    return("")
  })
  return(rtn)
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

invalidate_reactive <- function(reactive_data, reactive_freq){
  reactive_data()$invalidate()
  reactive_freq()$invalidate()
}


#' Function to load the default data depending on selected outcome
#' data is loaded to \code{data$data} and sets appropriate values
#'
#' @param reactive_data reactive values variable for data (see global.R)
#' @param reactive_freq freq reactive values variable
#' for frequentest analysis (see global.R)
#'
#' @return True if default data was loaded
#' @export
#'
#' @examples
load_default_data <- function(reactive_data, reactive_freq) {
  print("Attempting to Load Default Data")
  # Try catch to display an error if one occures
  tryCatch({
    # Invalidate any currently loaded data
    invalidate_reactive(reactive_data, reactive_freq)
    # Determine whether to load binary or continous data
    if (reactive_data()$data_type() == "binary") {
      tmp_data <- default_data_binary() # nolint: object_name
    } else {
      tmp_data <- default_data_continuous() # nolint: object_name
    }
    # Load the data
    reactive_data()$load_data(
      tmp_data$format,
      tmp_data$data_frame,
      TRUE,
      tmp_data$measure,
      tmp_data$desirable,
      tmp_data$outcome_name
    )
    return(TRUE)
  },
  error = function(e) {
    error_alert(e$message) # nolint: object_name
    invalidate_reactive(reactive_data, reactive_freq)
    return(FALSE)
  })
}

#' Get the studies from the formatted data
#'
#' @param data reactive values variable for data (see global.R)
#' @param freq reactive values variable for frequentest analysis (see global.R)
#'
#' @return a \code{list} of studies derived from the formatted data (freq$data)
#' @export
#'
#' @examples
get_studies <- function(data, freq) {
  tryCatch({
    # If the data is not valid do not try to determine the studies
    if( !is_data_valid(data) | is.null(freq$data)) {
      print("this error occured trying to determine the studies from the data")
      stop("There is a problem with the data, please check data has been uploaded and is valid")
    }
    if(is.null(freq$studies)){
      studies <- as.factor(freq$data$study)
      freq$studies <- levels(studies)
    }
    return(freq$studies)
  },
  error = function(e) {
    error_alert(e$message) # nolint
    invalidate_data(data, freq)
  })
}

get_outcome_measure <- function(outcome_measure){
  if(is.null(outcome_measure)){
    return("Outcome Measure")
  }
  case_when(
    outcome_measure == "md" ~ "Mean Difference (MD)",
    outcome_measure == "smd" ~ "Standardised Mean Difference (SMD)",
    outcome_measure == "or" ~ "Odds Ratio (OR)",
    outcome_measure == "rr" ~ "Risk Ratio (RR)",
    outcome_measure == "rd" ~ "Risk Difference (RD)",
    T ~ "Outcome Measure"
  )
}
