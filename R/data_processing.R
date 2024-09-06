#' @title Get the required columns
#' @description Function to get the required columns for validation
#' give the data_type and whether or not the data.frame
#' is in wide format.
#' @param data_type \code(character) Data type either 'continuous'
#' or 'binary'
#' @param is_wide \code{boolean} is the data in wide format
#' @param tmp_df \code{data.frame} the data
#' @return a \code{character} vector of required columns
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'    get_required_columns(
#'      "continuous",
#'      TRUE,
#'      df
#'    )
#'  }
#' }
#' @rdname get_required_columns
get_required_columns <- function(data_type, is_wide, tmp_df) {
  req_columns <- NULL # Forward declaration
  # Long
  if (!is_wide) { # nolint: object_name
    if (data_type == "continuous") { # continuous long
      req_columns <- tolower(get_required_continuous_long_columns()) # nolint: object_name
    } else if (data_type == "binary") { # binary long
      req_columns <- tolower(get_required_binary_long_columns()) # nolint: object_name
    }
  } else { # Wide
    if (data_type == "continuous") { # continuous wide
      req_columns <- tolower(get_required_continuous_wide_columns()) # nolint: object_name
    } else if (data_type == "binary") { # continuous long
      req_columns <- tolower(get_required_binary_wide_columns()) # nolint: object_name
    }
    # Get the number of arms
    wide_columns <- get_wide_columns(names(tmp_df), req_columns) # nolint: object_name
    wide_column_names <- get_wide_column_names(names(tmp_df), wide_columns) # nolint: object_name
    n_arms <- split_wide_columns(wide_column_names)$n_arms # nolint: object_name
    # Add each arm to the required columns
    for (col in wide_columns){
      for (i in 2:n_arms){
        req_columns <- append(req_columns, paste(col, i, sep = "."))
      }
    }
  }
  if (is.null(req_columns)) { # Unknown format (sanity)
    print("this error occured trying to format the data")
    stop("An error occured with the data,
    the format of the data could not be determined.")
  }
  return(req_columns)
}

#' Format the data \code{data$data} if it exists and is valid,
#' removing unnecessary columns and converting column names to lower case
#' if the function fails an error will be printed and return False
#'
#' @param data data_frame containing data
#' @param data_type type of the data either "continuous" or "binary"
#'
#' @return \code{data.frame} if the data was successfully stored
#' NULL otherwise
#' @export
#'
#' @examples
format_data <- function(df, data_type) {
  `%>%` <- magrittr::`%>%`
  tryCatch({
    #print("formatting data")
    # Copy data from uploaded data to temporary data frame
    tmp_df <- df
    # Use lowercase column names
    names(tmp_df) <- tolower(names(tmp_df))
    # Initialize required columns
    req_columns <- get_required_columns(data_type, is_wide(df), tmp_df)
    # Keep only required columns
    tmp_df <- dplyr::select(tmp_df, dplyr::all_of(req_columns))
    # If wide convert to long
    if (is_wide(tmp_df)) {
      tmp_df <- data_wide_to_long(tmp_df)
    }
    # Remove non UTF-Characters
    tmp_df <- as.data.frame(lapply(tmp_df, function(x) {
      if (is.character(x)) {
        iconv(x, from = "ISO-8859-1", to = "UTF-8")
      } else {
        x
      }
    }))
    # Filter any rows which contain NAs
    if (data_type == "continuous") {
      tmp_df <- tmp_df %>% filter(! is.na(mean))
    } else {
      tmp_df <- tmp_df %>% filter(! is.na(events))
    }
    #print("Saving Formatted Data")
    return(tmp_df)
  },
  error = function(e) {
    error_alert(e$message) # nolint: object_name
    return(NULL)
  })
}

#' @title Convert from wide to long format
#' @description Change a \code{data.frame} from wide (on trial per row)
#' to long (one arm per row) format
#' @param df A \code{data.frame} in wide format with
#' column seperated by `.`
#' @return A \code{data.frame} in long format (one arm per row)
#' @details Given a data frame with columns in the format of
#' column.1, column.2 convert that data frame so each arm (.n)
#' is represented by it's own row.
#' @examples
#' \dontrun{
#' if(interactive()){
#'    data_wide_to_long(df)
#'  }
#' }
#' @seealso
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{reexports}}
#' @rdname data_wide_to_long
#' @importFrom tidyr pivot_longer contains
data_wide_to_long <- function(df) {
  tidyr::pivot_longer(
    df,
    cols = tidyr::contains("."),
    names_pattern = "(.+)\\.(\\d+)",
    names_to = c(".value", "arm")
  )
}