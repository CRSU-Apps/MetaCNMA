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
  invalidateFreq(globalFreq)
}

invalidateFreq <- function(globalFreq){
  globalFreq$data <- NULL
  globalFreq$pairwise <- NULL
  globalFreq$nm <- NULL
  globalFreq$nc <- NULL
  globalFreq$valid = F
}


#' Function to load the default data depending on selected outcome
#' data is loaded to \code{globalData$data} and sets appropriate values
#'
#' @param globalData reactive values variable for data (see global.R)
#' @param globalFreq globalFreq reactive values variable for frequentest analysis (see global.R)
#'
#' @return True if default data was loaded
#' @export
#'
#' @examples
loadDefaultData <- function(globalData, globalFreq) {
  tryCatch({
    # Determine which data to load and store in a temporary dataframe
    if (globalData$type == "binary") {
      tmpData <- defaultDataBinary()
    } else {
      tmpData <- defaultDataContinous()
    }
    # Set reactive values (ensure globalData$valid is set last)
    globalData$format <- tmpData$format
    globalData$measure <- tmpData$measure
    globalData$desirable <- tmpData$desirable
    globalData$default = T
    globalData$data <- NULL
    globalData$data <- tmpData$dataFrame
    globalFreq$valid <- F
    globalData$valid = T
    return(T)
  },
  error = function(e) {
    errorAlert(e$message)
    invalidateData(globalData, globalFreq)
    return(F)
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
