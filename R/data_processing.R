require(dplyr)
#' Check the data is valid for further analysis
#'
#' @param data reactive values variable for data (see global.R)
#'
#' @return \code{logical} True if the data is valid for further analysis
#' @export
#'
#' @examples
is_data_valid <- function(data){
  return( all( (!is.null(data$valid) & all(as.logical(data$valid))) & !is.null(data$type) & !is.null(data$format) & !is.null(data$data) ) )
}


#' Format the data \code{data$data} if it exists and is valid,
#' removing unnecessary columns and converting column names to lower case
#' storing the new data in \code{freq$data}
#' if the function fails it will invalidate the original data and return False
#'
#' @param data reactive values variable for data (see global.R)
#' @param freq freq reactive values variable for frequentest analysis (see global.R)
#'
#' @return \code{logical} True if the data was successfully stored F otherwise
#' @export
#'
#' @examples
format_data <- function(data, freq) {
  tryCatch({
    # If the data is not valid do not format the data
    if( !is_data_valid(data) ){
      print("This error occured trying to format the data")
      stop("There is a problem with the data, please check data has been uploaded and is valid")
    }
    print("formatting data")
    # Copy data from uploaded data to temporary data frame
    tmpDf <- data$data
    # Use lowercase column names
    names(tmpDf) <- tolower(names(tmpDf))
    # Initialize required columns
    reqColumns <- NULL
    # If long format
    if(data$format == "long"){
      if(data$type == "continuous"){
        reqColumns <- tolower(get_required_continuous_long_columns())
      }
      else {
        reqColumns <- tolower(get_required_binary_long_columns())
      }
    }
    # Else if wide
    else if(data$format == "wide") {
      if(data$type == "continuous"){
        reqColumns <- tolower(get_required_continuous_wide_columns())
      }
      else {
        reqColumns <- tolower(get_required_binary_wide_columns())
      }
      # Get the number of arms
      wideColumns <- get_wide_columns(names(tmpDf), reqColumns)
      wideColumnNames <- get_wide_column_names(names(tmpDf), wideColumns)
      nArms <- split_wide_columns(wideColumnNames)$nArms
      # Add each arm to the required columns
      for(col in wideColumns){
        for(i in 2:nArms){
          reqColumns <- append(reqColumns, paste(col, i, sep = "."))
        }
      }
    } else { # Else unknown format (sanity)
      print("this error occured trying to format the data")
      stop("An error occured with the data, 
      the format of the data could not be determined.")
    }
    freq$data <- tmpDf %>% select(all_of(reqColumns))
    return(TRUE)
  },
  error = function(e) {
    error_alert(e$message)
    invalidate_data(data, freq)
    return(FALSE)
  })
}

data_wide_to_long <- function(df){
  tidyr::pivot_longer(
    df,
    cols = tidyr::contains("."),
    names_pattern = "(.+)\\.(\\d+)",
    names_to = c(".value", "arm")
  )
}