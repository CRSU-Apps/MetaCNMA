# This file contains functions used for validation of the data

#' @title Valid File Format
#' @description Checks if the given file extention is valid
#' @param file_ext The file extention to be validated
#' @return TRUE if valid otherwise FALSE
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
  return(file_ext %in% get_accepted_file_formats()) # nolint: object_usage
}

is_file_exists <- function(file_path) {
  print(paste0("checking file ", file_path, " exists"))
  return(file.exists(file_path))
}

is_wide <- function(df) {
  return(any(grepl("(.+)\\.(\\d+)", names(df))))
}

validate_column_names <- function(df, required_names) {
  column_names <- names(df)
  tryCatch({
    missing_columns <- lapply(required_names, function(name) {
      if (!tolower(name) %in% tolower(column_names)) {
        return(name)
      }
    })
    missing_columns <- purrr::compact(missing_columns)
    if (purrr::is_empty(missing_columns)) {
      return(TRUE)
    } else {
      error_alert( # nolint: object_usage
        paste0("Error column(s): '",
          paste0(missing_columns, collapse = ", "),
          "' are missing from the data"
        )
      )
      return(FALSE)
    }
  },
  error = function(e) {
    error_alert(e$message) # nolint: object_usage
    return(FALSE)
  })
}

split_wide_columns <- function(columns) {
  cols <- list()
  col_numbers <- list()
  for (col in columns) {
    # get the column name (before the .)
    col_name <- sub("(.+)\\.(\\d+)", "\\1", col)
    # get the column number (after the .)
    col_number <- sub("(.+)\\.(\\d+)", "\\2", col)
    # store these in the lists initalised previously
    cols <- append(cols, col_name)
    col_numbers <- append(col_numbers, as.numeric(col_number))
  }
  n_arms <- max(as.numeric(col_numbers))
  return(list(cols = cols, col_numbers = col_numbers, n_arms = n_arms))
}

get_wide_columns <- function(column_names, required_columns) {
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
    col_numbers <- split_cols$col_numbers # nolint: object_usage
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
          return(FALSE)
        }
      }
    }
    # Check the number of columns are as expected

    if (! is.null(missing_columns)) {
      error_alert( # nolint: object_usage
        paste0("Error column(s): '",
          paste0(missing_columns, collapse = ", "),
          "' are missing from the data"
        )
      )
    }

    if (length(cols) != n_expected_cols) {
      error_alert("There is a problem with the number of columns, did you miss a column?") # nolint: object_usage
      return(FALSE)
    }

  },
  error = function(e) {
    error_alert(e$message) # nolint: object_usage
    return(FALSE)
  })
  return(TRUE)
}

validate_binary_long <- function(df) {
  required_columns <- get_required_binary_long_columns() # nolint: object_usage
  validate_column_names(df, required_columns)
}

validate_continuous_long <- function(df) {
  required_columns <- get_required_continuous_long_columns() # nolint: object_usage
  validate_column_names(df, required_columns)
}

validate_binary_wide <- function(df) {
  required_columns <- get_required_binary_wide_columns() # nolint: object_usage
  validate_wide(df, required_columns)
}

validate_continuous_wide <- function(df) {
  required_columns <- get_required_continuous_wide_columns() # nolint: object_usage
  validate_wide(df, required_columns)
}

validate_input <- function(input_file, type) { # nolint: cyclocomp
  tryCatch({
    print("Checking input file")
    if (is.null(input_file)) {
      print("Null input file")
      error_alert("There is a problem with the uploaded file please try again") # nolint: object_usage
      return(FALSE)
    }
    if (is.null(type)) {
      print("Null Type")
      error_alert("An internal error occured, please try again, if this problem persist please contact the developers", "Error #TP001") # nolint: object_usage
      return(FALSE)
    }

    if (is.null(input_file$datapath)) {
      print("Null datapath")
      error_alert("An error occured, please try again, if this problem persist please contact the developers", "Error #FU001") # nolint: object_usage
      return(FALSE)
    }

    if (!is_file_exists(input_file$datapath)) {
      print("file doesn't exist")
      error_alert("An internal rror occured, please try again, if this problem persist please contact the developers", "Error #FU002") # nolint: object_usage
      return(FALSE)
    }

    if (!is_valid_file_format(tools::file_ext(input_file$name))) {
      error_alert(paste0("Accepted Files: ", paste0(get_accepted_file_formats(), collapse = ", ")), "Error File Extension not Supported") # nolint: object_usage
      return(FALSE)
    }

    df <- rio::import(input_file$datapath)

    if (is.null(df)) {
      print("Null Data Frame")
      error_alert("An internal error occured, the data appears to be empty, please try again, if this problem consiste please contact the developers", "Error #FU003") # nolint: object_usage
      return(FALSE)
    }

    if (is_wide(df)) {
      print("Assuming Wide Format")
      if (type == "binary") {
        return(validate_binary_wide(df))
      } else if (type == "continuous") {
        validate_continuous_wide(df)
      }
    } else {
      print("Assuming long Format")
      if (type == "binary") {
        return(validate_binary_long(df))
      } else if (type == "continuous") {
        return(validate_continuous_long(df))
      }
    }
  },
  error = function(e) {
    error_alert(e$message) # nolint: object_usage
    return(FALSE)
  })
}
