siteInfo <- read_yaml("site_info.yaml")

getSiteInfoProperty <- function(property) {
  rtn <- tryCatch({
    if (is.null(siteInfo[property])) {
      return("")
    }
    siteInfo[property]
  },
  error = function(e) {
    return("")
  },
  warning = function(w) {
    return("")
  })
  return(rtn)
}

getTitle <- function(){
  return(as.character(getSiteInfoProperty("title")))
}

getVersion <- function(){
  return(as.character(getSiteInfoProperty("version")))
}

getAcceptedFileFormats <- function(){
  return(getSiteInfoProperty("accepted_file_formats")[[1]])
}

getCookieMessage <- function(){
  includeHTML("html/cookie.html")
}

getDescription <- function(){
  return(as.character(getSiteInfoProperty("description")))
}

getKeywords <- function() {
  return(paste(getSiteInfoProperty("keywords")[[1]], collapse = ","))
}

getRequiredBinaryLongColumns <- function() {
  return(getSiteInfoProperty("required_binary_long_columns")[[1]])
}

getRequiredContinousLongColumns <- function() {
  return(getSiteInfoProperty("required_continous_long_columns")[[1]])
}

getRequiredBinaryWideColumns <- function() {
  return(getSiteInfoProperty("required_binary_wide_columns")[[1]])
}

getRequiredContinousWideColumns <- function() {
  return(getSiteInfoProperty("required_continous_wide_columns")[[1]])
}

invalidateData <- function(globalData, globalFreq){
  globalData$valid = F
  globalFreq$valid = F
}

loadDefaultData <- function(globalData, globalFreq) {
  tryCatch({
    if (globalData$type == "binary") {
      tmpData <- defaultDataBinary()
    } else {
      tmpData <- defaultDataContinous()
    }
    globalData$format <- tmpData$format
    globalData$measure <- tmpData$measure
    globalData$desirable <- tmpData$desirable
    globalData$default = T
    globalData$data <- NULL
    globalData$data <- tmpData$dataFrame
    globalData$valid = T
  },
  error = function(e) {
    errorAlert(e$message)
    invalidateData(globalData, globalFreq)
  })
  
}

isDataValid <- function(globalData){
  return( all( (!is.null(globalData$valid) & all(as.logical(globalData$valid))) & !is.null(globalData$type) & !is.null(globalData$format) & !is.null(globalData$data) ) )
}

formatData <- function(globalData, globalFreq) {
  tryCatch({
    # If the data is not valid do not format the data
    if( !isDataValid(globalData) ){
      print("This error occured trying to format the data")
      stop("There is a problem with the data, please check data has been uploaded and is valid")
    }
    # Copy data from uploaded data to temporary data frame
    tmpDf <- globalData$data
    # Use lowercase column names
    names(tmpDf) <- tolower(names(tmpDf))
    # Initialize required columns
    reqColumns <- NULL
    # If long format
    if(globalData$format == "long"){
      if(globalData$type == "continous"){
        reqColumns <- tolower(getRequiredContinousLongColumns())
      }
      else {
        reqColumns <- tolower(getRequiredBinaryLongColumns())
      }
    }
    # Else if wide
    else if(globalData$format == "wide") {
      if(globalData$type == "continous"){
        reqColumns <- tolower(getRequiredContinousWideColumns())
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
    globalFreq$data <- tmpDf %>% select(all_of(reqColumns))
    return(T)
  },
  error = function(e) {
    errorAlert(e$message)
    invalidateData(globalData, globalFreq)
  })
}


#' Get the studies from the formatted data
#'
#' @param globalData reactive values variable for data (see global.R)
#' @param globalFreq reactive values variable for frequentest analysis (see global.R)
#'
#' @return a \code{list} of studies derived from the formatted data (globalFreq$data)
#' @export
#'
#' @examples
getStudies <- function(globalData, globalFreq){
  tryCatch({
    # If the data is not valid do not try to determine the studies
    if( !isDataValid(globalData) | is.null(globalFreq$data)){
      print("this error occured trying to determine the studies from the data")
      stop("There is a problem with the data, please check data has been uploaded and is valid")
    }
    if(is.null(globalFreq$studies)){
      studies <- as.factor(globalFreq$data$study)
      globalFreq$studies <- levels(studies)
    }
    return(globalFreq$studies)
  },
  error = function(e) {
    errorAlert(e$message)
    invalidateData(globalData, globalFreq)
  })
}
