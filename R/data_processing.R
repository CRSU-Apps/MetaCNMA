#' Format the data \code{data$data} if it exists and is valid,
#' removing unnecessary columns and converting column names to lower case
#' if the function fails an error will be printed and return False
#'
#' @param data data_frame containing data
#' @param data_type type of the data either "continuous" or "binary"
#'
#' @return \code{logical} TRUE if the data was successfully stored
#' FALSE otherwise
#' @export
#'
#' @examples
format_data <- function(df, data_type) {
  `%>%` <- magrittr::`%>%`
  tryCatch({
    print("formatting data")
    # Copy data from uploaded data to temporary data frame
    tmp_df <- df
    # Use lowercase column names
    names(tmp_df) <- tolower(names(tmp_df))
    # Initialize required columns
    req_columns <- NULL
    # If long format
    if (!is_wide(tmp_df)) { # nolint: object_name
      if (data_type == "continuous") {
        req_columns <- tolower(get_required_continuous_long_columns()) # nolint: object_name
      } else {
        req_columns <- tolower(get_required_binary_long_columns()) # nolint: object_name
      }
    } else if (is_wide(tmp_df)) { # Else if wide
      if (data_type == "continuous") {
        req_columns <- tolower(get_required_continuous_wide_columns()) # nolint: object_name
      } else {
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
    } else { # Else unknown format (sanity)
      print("this error occured trying to format the data")
      stop("An error occured with the data, 
      the format of the data could not be determined.")
    }
    tmp_df <- dplyr::select(tmp_df, dplyr::all_of(req_columns))
    if (is_wide(tmp_df)) {
      tmp_df <- data_wide_to_long(tmp_df)
    }
    tmp_df <- as.data.frame(lapply(tmp_df, function(x) {
      if (is.character(x)) {
        iconv(x, from = "ISO-8859-1", to = "UTF-8")
      } else {
        x
      }
    }))
    print("Saving Formatted Data")
    tmp_df <- tmp_df %>% filter(! is.na(mean))
    return(tmp_df)
  },
  error = function(e) {
    error_alert(e$message) # nolint: object_name
    return(FALSE)
  })
}

data_wide_to_long <- function(df) {
  tidyr::pivot_longer(
    df,
    cols = tidyr::contains("."),
    names_pattern = "(.+)\\.(\\d+)",
    names_to = c(".value", "arm")
  )
}