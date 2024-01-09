`%>%` <- magrittr::`%>%`
#' Format the data \code{data$data} if it exists and is valid,
#' removing unnecessary columns and converting column names to lower case
#' storing the new data in \code{freq$data}
#' if the function fails it will invalidate the original data and return False
#'
#' @param reactive_data reactive values variable for data (see global.R)
#' @param reactive_freq freq reactive values variable for
#' frequentest analysis (see global.R)
#'
#' @return \code{logical} True if the data was successfully stored F otherwise
#' @export
#'
#' @examples
format_data <- function(reactive_data, reactive_freq) {
  tryCatch({
    # If the data is not valid do not format the data
    if (! reactive_data()$valid()) {
      print("This error occured trying to format the data")
      stop("There is a problem with the data, 
      please check data has been uploaded and is valid")
    }
    print("formatting data")
    # Copy data from uploaded data to temporary data frame
    tmp_df <- reactive_data()$data()
    # Use lowercase column names
    names(tmp_df) <- tolower(names(tmp_df))
    # Initialize required columns
    req_columns <- NULL
    # If long format
    if (reactive_data()$format() == "long") {
      if (reactive_data()$data_type() == "continuous") {
        req_columns <- tolower(get_required_continuous_long_columns()) # nolint: object_name
      } else {
        req_columns <- tolower(get_required_binary_long_columns()) # nolint: object_name
      }
    } else if (reactive_data()$format() == "wide") { # Else if wide
      if (reactive_data()$data_type() == "continuous") {
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
    print("Saving formatted data")
    reactive_freq()$formatted_data(dplyr::select(tmp_df, all_of(req_columns)))
    return(TRUE)
  },
  error = function(e) {
    error_alert(e$message) # nolint: object_name
    invalidate_reactive(reactive_data, reactive_freq) # nolint: object_name
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