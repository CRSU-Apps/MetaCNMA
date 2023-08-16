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

invalidateData <- function(data, freq){
  data$valid = F
  invalidateFreq(freq)
}

invalidateFreq <- function(freq){
  freq$data <- NULL
  freq$pairwise <- NULL
  freq$nm <- NULL
  freq$nc <- NULL
  freq$valid = F
}


#' Function to load the default data depending on selected outcome
#' data is loaded to \code{data$data} and sets appropriate values
#'
#' @param data reactive values variable for data (see global.R)
#' @param freq freq reactive values variable for frequentest analysis (see global.R)
#'
#' @return True if default data was loaded
#' @export
#'
#' @examples
loadDefaultData <- function(data, freq) {
  tryCatch({
    # Determine which data to load and store in a temporary dataframe
    if (data$type == "binary") {
      tmpData <- defaultDataBinary()
    } else {
      tmpData <- defaultDataContinous()
    }
    # Set reactive values (ensure data$valid is set last)
    data$format <- tmpData$format
    data$measure <- tmpData$measure
    data$desirable <- tmpData$desirable
    data$default = T
    data$data <- NULL
    data$data <- tmpData$dataFrame
    freq$valid <- F
    data$valid = T
    return(T)
  },
  error = function(e) {
    errorAlert(e$message)
    invalidateData(data, freq)
    return(F)
  })
  
}

#' Get the studies from the formatted data
#'
#' @param data reactive values variable for data (see global.R)
#' @param freq reactive values variable for frequentest analysis (see global.R)
#'
#' @return a \code{list} of studies derived from the formatted data (freq$data)
#' @export
#'
#' @examples
getStudies <- function(data, freq){
  tryCatch({
    # If the data is not valid do not try to determine the studies
    if( !isDataValid(data) | is.null(freq$data)){
      print("this error occured trying to determine the studies from the data")
      stop("There is a problem with the data, please check data has been uploaded and is valid")
    }
    if(is.null(freq$studies)){
      studies <- as.factor(freq$data$study)
      freq$studies <- levels(studies)
    }
    return(freq$studies)
  },
  error = function(e) {
    errorAlert(e$message)
    invalidateData(data, freq)
  })
}
