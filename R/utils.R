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

invalidate_data <- function(data, freq){
  invalidate_freq(freq)
  data$valid = F
}

invalidate_freq <- function(freq){
  freq$data <- NULL
  freq$outcome <- NULL
  freq$desirable <- NULL
  freq$randomEffects <- NULL
  freq$outcomeName <- NULL
  freq$pairwise <- NULL
  freq$nm <- NULL
  freq$nc <- NULL
  freq$valid = F
}


#' Function to load the default data depending on selected outcome
#' data is loaded to \code{data$data} and sets appropriate values
#'
#' @param data reactive values variable for data (see global.R)
#' @param freq freq reactive values variable for frequentest analysis (see global.R)
#'
#' @return True if default data was loaded
#' @export
#'
#' @examples
load_default_data <- function(data, freq) {
  tryCatch({
    invalidate_data(data, freq)
    # Determine which data to load and store in a temporary dataframe
    if (data$type == "binary") {
      tmpData <- default_data_binary()
    } else {
      tmpData <- default_data_continuous()
    }
    # Set reactive values (ensure data$valid is set last)
    data$format <- tmpData$format
    data$measure <- tmpData$measure
    data$desirable <- tmpData$desirable
    data$outcomeName <- tmpData$outcomeName
    data$randomEffects <- tmpData$randomEffects
    data$default = T
    data$data <- NULL
    data$data <- tmpData$dataFrame
    freq$valid <- F
    data$valid = T
    return(T)
  },
  error = function(e) {
    error_alert(e$message)
    invalidate_data(data, freq)
    return(F)
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
