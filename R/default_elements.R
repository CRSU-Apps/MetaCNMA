##################################################################
##                       Default Elements                       ##
##                         Allows Reuse                         ##
##################################################################

message_tag_list <- function(ns) {
  return(
    shiny::tagList(
      shiny::uiOutput(ns("warning")),
      shiny::uiOutput(ns("info"))
    )
  )
}


default_file_input <- function(ns) {
  return(
    shiny::fileInput(
      inputId = ns("data"),
      label = "",
      buttonLabel = list(shiny::icon("file"), "Select File"),
      placeholder = "No file selected",
      accept = c(".csv", ".xlsx")
    )
  )
}

default_no_data <- function(ns) {
  shiny::tagList(
    shiny::p("No data loaded"),
    shiny::p("Please upload data or:"),
    shiny::div(
      shiny::actionButton(
        ns("default_data"),
        "Reload Default Data",
        shiny::icon("arrows-rotate"),
        style =
          "color: #fff; background-color: #dc3545; border-color: #dc3545" # nolint line_length
      )
    )
  )
}

default_data_continuous <- function() {
  return(list(
    type = "continuous",
    format = "long",
    measure = "md",
    desirable = 1,
    random_effects = 0,
    outcome_name = "Length of Stay",
    reference_component = "Usual Care"
  ))
}

default_data_binary <- function() {
  return(list(
    type = "binary",
    format = "long",
    measure = "or",
    desirable = 0,
    random_effects = 0,
    outcome_name = "Response after Treatment",
    reference_component = "Placebo"
  ))
}

default_data_properties <- function(data_type) {
  if (data_type == "binary") {
    return(default_data_binary())
  } else if (data_type == "continuous") {
    return(default_data_continuous())
  } else {
    return(NULL)
  }
}

#' Function to load the default data depending on selected outcome
#' data is loaded to \code{data$data} and sets appropriate values
#'
#' @param data_type type of data to load (either "binary", or "continuous")
#' for frequentest analysis (see global.R)
#'
#' @return True if default data was loaded
#' @export
#'
#' @examples
default_data <- function(data_type) {
  #print("Attempting to Load Default Data")
  # Try catch to display an error if one occures
  tryCatch({
    # Determine whether to load binary or continous data
    if (data_type == "binary") {
      return(MetaCNMABayes::binary) # nolint: object_name
    } else if (data_type == "continuous") {
      return(MetaCNMABayes::continuous) # nolint: object_name
    } else {
      return(NULL)
    }
  },
  error = function(e) {
    error_alert(e$message) # nolint: object_name
    return(NULL)
  })
}


default_reload_button <- function(ns, button_text = "Delete Data") {
  shiny::div(
    shiny::actionButton(
      ns("reload_button"),
      button_text,
      shiny::icon("trash"),
      style =
        "color: #fff; background-color: #dc3545; border-color: #dc3545"
    ) # nolint line_length
  )
}

default_outcome_measure <- function(data_type, is_default_data) {
  if (is_default_data) {
    return(default_data_properties(data_type)$measure)
  }
  if (data_type == "continuous") {
    return("md")
  } else if (data_type == "binary") {
    return("or")
  } else {
    return(NULL)
  }
}

default_outcome_desirable <- function(data_type, is_default_data) {
  if (is_default_data) {
    return(default_data_properties(data_type)$desirable)
  }
  if (data_type == "continuous") {
    return(1)
  } else if (data_type == "binary") {
    return(0)
  } else {
    return(NULL)
  }
}

default_random_effects <- function(data_type, is_default_data) {
  if (is_default_data) {
    return(default_data_properties(data_type)$random_effects)
  }
  if (data_type == "continuous") {
    return(0)
  } else if (data_type == "binary") {
    return(0)
  } else {
    return(NULL)
  }
}

default_desirable_text <- function(data_type) {
  if (data_type == "continuous") {
    return("For treatment rankings a smaller 
                outcome value (MD / SMD) is:")
  } else if (data_type == "binary") {
    return("For treatment rankings an outcome value (OR / RR / RR) 
                less than 1 is:")
  } else {
    return(NULL)
  }
}

default_outcome_name <- function(data_type, is_default_data) {
  if (is_default_data) {
    return(default_data_properties(data_type)$outcome_name)
  } else {
    return("Outcome Name")
  }
}

default_reference_component <- function(data_type) {
  return(default_data_properties(data_type)$reference_component)
}