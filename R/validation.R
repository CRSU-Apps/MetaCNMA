#################################################################
##                Functions for Data Validation                ##
#################################################################

#' @title Valid File Format
#' @description Checks if the given file extention is valid
#' @param file_ext The file extention to be validated
#' @return \code{TRUE} if valid otherwise \code{FALSE}
#' @details Checks to see if the given file extension is
#' in the accepted file formats list (see site_info.yaml)
#' @examples
#' \dontrun{
#' if(interactive()){
#'  is_valid_file_format("csv")
#'  }
#' }
#' @rdname is_valid_file_format
is_valid_file_format <- function(file_ext) {
  return(file_ext %in% get_accepted_file_formats()) # nolint: object_name
}

#' @title File Exists
#' @description Checks to see if a given file exists
#' wrapper for file.exists
#' @param file_path path to file to be checked
#' @return \code{TRUE} if file exists otherwise \code{FALSE}
#' @details Prints the file path to the console fo debugging
#' @examples
#' \dontrun{
#' if(interactive()){
#'  is_file_exists("test.csv")
#'  }
#' }
#' @rdname is_file_exists
is_file_exists <- function(file_path) {
  #print(paste0("Checking file ", file_path, " exists"))
  return(file.exists(file_path))
}

#' @title Determine if a dataframe is in a wide format
#' @description attempts to determine if a given
#' datafreame is in a wide format
#' @param df \code{data.frame} to be checked
#' @return \code{TRUE} if wide else \code{FALSE}
#' @details This function checks for the presence of
#' \code{.} in the column names to determine if the
#' \code{data.frame} is considered to be wide format
#' @examples
#' \dontrun{
#' if(interactive()){
#'  is_wide(df)
#'  }
#' }
#' @rdname is_wide
is_wide <- function(df) {
  return(any(grepl("(.+)\\.(\\d+)", names(df))))
}

#' @title Check for required columns
#' @description Check if required columns are present in
#' a given \code{data.frame}
#' @param df \code{data.frame} to be checked
#' @param required_names \{character} of required columns
#' @return \code{TRUE} if all required columns are present
#' Otherwise \code{FALSE}
#' @details Given a \code{character} vector of required
#' columns and a \code{data.frame} check if all of the columns
#' are present in the \code{data.frame}. On error return \code{FALSE}
#' and display the error message
#' @examples
#' \dontrun{
#' if(interactive()){
#'  validate_column_names(
#'    df
#'    c("Study", "Components"),
#'  )
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{keep}}, \code{\link[purrr]{reexports}}
#' @rdname validate_column_names
#' @importFrom purrr compact is_empty
validate_column_names <- function(df, required_names) {
  # Get the column names
  column_names <- names(df)
  # Use a try catch
  tryCatch({
    # Loop through required columns and check to see
    # if they are in the given data.fram
    missing_columns <- lapply(required_names, function(name) {
      if (!tolower(name) %in% tolower(column_names)) {
        return(name)
      }
    })
    # Collapse the missing columns into a string
    missing_columns <- purrr::compact(missing_columns)
    # If there are no missing columns return TRUE
    if (purrr::is_empty(missing_columns)) {
      return(TRUE)
    } else {
      # Otherwise display an error with the missing columns
      # and return false
      error_alert( # nolint: object_name
        paste0("Error column(s): '",
          paste0(missing_columns, collapse = ", "),
          "' are missing from the data"
        )
      )
      return(FALSE)
    }
  },
  error = function(e) {
    # If something goes wrong display the error and
    # return FALSE
    error_alert(e$message) # nolint: object_name
    return(FALSE)
  })
}

split_wide_columns <- function(columns) {
  # Forward declare cols list (list of column names)
  cols <- list()
  # forward declare col_numbers list (list of numbers in column names)
  col_numbers <- list()
  # loop through columns
  for (col in columns) {
    # Get the column name (before the .)
    col_name <- sub("(.+)\\.(\\d+)", "\\1", col)
    # Get the column number (after the .)
    col_number <- sub("(.+)\\.(\\d+)", "\\2", col)
    # Store these in the lists initalised previously
    cols <- append(cols, col_name)
    col_numbers <- append(col_numbers, as.numeric(col_number))
  }
  # The number of arms will be the maximum of the column numbers
  n_arms <- max(as.numeric(col_numbers))
  # return the lists and number of arms
  return(list(cols = cols, col_numbers = col_numbers, n_arms = n_arms))
}

get_wide_columns <- function(column_names, required_columns) {
  # get only the columns needed for wide format
  wide_columns <- required_columns[grep("(.+)\\.(\\d+)", required_columns)]
  # get the column names without the .n
  return(gsub("(.+)\\.(\\d+)", "\\1", wide_columns))
}

get_wide_column_names <- function(column_names, wide_columns) {
  # get only the wide format column names from the dataframe column names
  return(column_names[grep(paste(wide_columns, collapse = "|"), column_names)])
}

validate_wide <- function(df, required_columns) {
  tryCatch({
    required_columns <- tolower(required_columns)
    column_names <- tolower(names(df)) # Get the column names from the dataframe

    wide_columns <- get_wide_columns(column_names, required_columns)
    wide_column_names <- get_wide_column_names(column_names, wide_columns)

    split_cols <- split_wide_columns(wide_column_names)
    cols <- split_cols$cols
    col_numbers <- split_cols$col_numbers # nolint: object_name
    n_arms <- split_cols$n_arms

    n_expected_cols <- n_arms * length(wide_columns)
    missing_columns <- c()
    # Iterate through the wide columns (column names without the .n)
    for (col in wide_columns) {
      # Iterate the number of arms (derived from the maximum .n)
      for (i in 1:n_arms) {
        # Check that for each arm there is a corresponding wide column
        if (!paste(col, i, sep = ".") %in% wide_column_names) {
          missing_columns <- c(missing_columns, paste(col, i, sep = "."))
        }
      }
    }
    # Check the number of columns are as expected
    if (! is.null(missing_columns)) {
      error_alert( # nolint: object_name
        paste0("Error column(s): '",
          paste0(missing_columns, collapse = ", "),
          "' are missing from the data"
        )
      )
      return(FALSE)
    }

    if (length(cols) != n_expected_cols) {
      error_alert("There is a problem with the number of columns, did you miss a column?") # nolint: object_name
      return(FALSE)
    }

  },
  error = function(e) {
    error_alert(e$message) # nolint: object_name
    return(FALSE)
  })
  return(TRUE)
}

validate_binary_long <- function(df) {
  required_columns <- get_required_binary_long_columns() # nolint: object_name
  validate_column_names(df, required_columns)
}

validate_continuous_long <- function(df) {
  required_columns <- get_required_continuous_long_columns() # nolint: object_name
  validate_column_names(df, required_columns)
}

validate_binary_wide <- function(df) {
  required_columns <- get_required_binary_wide_columns() # nolint: object_name
  validate_wide(df, required_columns)
}

validate_continuous_wide <- function(df) {
  required_columns <- get_required_continuous_wide_columns() # nolint: object_name
  validate_wide(df, required_columns)
}

validate_input <- function(input_file, type) { # nolint: cyclocomp
  tryCatch({
    #print("Checking input file")
    if (is.null(input_file)) {
      #print("Null input file")
      error_alert("There is a problem with the uploaded file please try again") # nolint: object_name
      return(FALSE)
    }
    if (is.null(type)) {
      #print("Null Type")
      error_alert("An internal error occured, please try again, if this problem persist please contact the developers", "Error #TP001") # nolint: object_name
      return(FALSE)
    }

    if (is.null(input_file$datapath)) {
      #print("Null datapath")
      error_alert("An error occured, please try again, if this problem persist please contact the developers", "Error #FU001") # nolint: object_name
      return(FALSE)
    }

    if (!is_file_exists(input_file$datapath)) {
      #print("file doesn't exist")
      error_alert("An internal rror occured, please try again, if this problem persist please contact the developers", "Error #FU002") # nolint: object_name
      return(FALSE)
    }

    if (!is_valid_file_format(tools::file_ext(input_file$name))) {
      error_alert(paste0("Accepted Files: ", paste0(get_accepted_file_formats(), collapse = ", ")), "Error File Extension not Supported") # nolint: object_name
      return(FALSE)
    }

    df <- rio::import(input_file$datapath)

    if (is.null(df)) {
      #print("Null Data Frame")
      error_alert("An internal error occured, the data appears to be empty, please try again, if this problem consiste please contact the developers", "Error #FU003") # nolint: object_name
      return(FALSE)
    }

    if (is_wide(df)) {
      #print("Assuming Wide Format")
      if (type == "binary") {
        return(validate_binary_wide(df))
      } else if (type == "continuous") {
        return(validate_continuous_wide(df))
      }
    } else {
      #print("Assuming long Format")
      if (type == "binary") {
        return(validate_binary_long(df))
      } else if (type == "continuous") {
        return(validate_continuous_long(df))
      }
    }
  },
  error = function(e) {
    error_alert(e$message) # nolint: object_name
    return(FALSE)
  })
}

check_outcome_measure <- function(
  data_type,
  outcome_measure
) {
  if (is.null(data_type) || is.null(outcome_measure)) {
    return(FALSE)
  } else if (data_type == "continuous") {
    if (outcome_measure %in% c("MD", "SMD")){
      return(TRUE)
    }
  } else if (data_type == "binary") {
    if (outcome_measure %in% c("OR", "RR", "RD")) {
      return(TRUE)
    }
  }

  return(FALSE)

}

check_reference_component <- function(
  reference_component,
  components
) {
  if (is.null(reference_component) || is.null(components)) {
    return(FALSE)
  } else if (reference_component %in% components) {
    return(TRUE)
  }
  return(FALSE)
}