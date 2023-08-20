#' Check the data is valid for further analysis
#'
#' @param data reactive values variable for data (see global.R)
#'
#' @return \code{logical} True if the data is valid for further analysis
#' @export
#'
#' @examples
isDataValid <- function(data){
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
formatData <- function(data, freq) {
  tryCatch({
    # If the data is not valid do not format the data
    if( !isDataValid(data) ){
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
        reqColumns <- tolower(getRequiredContinuousLongColumns())
      }
      else {
        reqColumns <- tolower(getRequiredBinaryLongColumns())
      }
    }
    # Else if wide
    else if(data$format == "wide") {
      if(data$type == "continuous"){
        reqColumns <- tolower(getRequiredContinuousWideColumns())
      }
      else {
        reqColumns <- tolower(getRequiredBinaryWideColumns())
      }
      # Get the number of arms
      wideColumns <- getWideColumns(names(tmpDf), reqColumns)
      wideColumnNames <- getWideColumnNames(names(tmpDf), wideColumns)
      nArms <- splitWideColumns(wideColumnNames)$nArms
      # Add each arm to the required columns
      for(col in wideColumns){
        for(i in 2:nArms){
          reqColumns <- append(reqColumns, paste(col, i, sep = "."))
        }
      }
    }
    # Else unknown format (sanity)
    else {
      print("this error occured trying to format the data")
      stop("An error occured with the data, the format of the data could not be determined.")
    }
    freq$data <- tmpDf %>% select(all_of(reqColumns))
    return(T)
  },
  error = function(e) {
    errorAlert(e$message)
    invalidateData(data, freq)
    return(F)
  })
}

dataWideToLong <- function(df){
  pivot_longer(
    df,
    cols = contains("."),
    names_pattern = "(.+)\\.(\\d+)",
    names_to = c(".value", "arm")
  )
}