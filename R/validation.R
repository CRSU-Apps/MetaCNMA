isValidFileFormat <- function(fileExt){
  if(fileExt %in% getAcceptedFileFormats()){
    return(T)
  }
  return(F)
}

isFileExists <- function(file_path){
  print(paste0("checking file ", file_path, " exists"))
  return(file.exists(file_path))
}

isWide <- function(df){
  return(any(grepl("(.+)\\.(\\d+)", names(df))))
}

validateColumnNames <- function(df, requiredNames) {
  columnNames <- names(df)
  tryCatch({
    columnsExist <- lapply(requiredNames, function(name) {
      if (!tolower(name) %in% tolower(columnNames)) {
        errorAlert(paste0("Error column ", name, " is missing from the data"))
        return(F)
      } else {
        return(T)
      }
    })
    if (all(as.logical(columnsExist))) {
      return(T)
    }
    else {
      return(F)
    }
  },
  error = function(e) {
    errorAlert(e$message)
    return(F)
  })
  return(F)
}

splitWideColumns <- function(columns){
  cols <- list()
  colNumbers <- list()
  for(col in columns){
    # get the column name (before the .)
    colName <- sub("(.+)\\.(\\d+)", "\\1", col)
    # get the column number (after the .)
    colNumber <- sub("(.+)\\.(\\d+)", "\\2", col)
    # store these in the lists initalised previously
    cols <- append(cols, colName)
    colNumbers <- append(colNumbers, as.numeric(colNumber))
  }
  nArms <- max(as.numeric(colNumbers))
  return(list("cols" = cols, "colNumbers" = colNumbers, "nArms" = nArms))
}

getWideColumns <- function(columnNames, requiredColumns){
  wideColumns <- requiredColumns[grep("(.+)\\.(\\d+)", requiredColumns)]
  # get the column names without the .n
  return(gsub("(.+)\\.(\\d+)", "\\1", wideColumns))
}

getWideColumnNames <-function(columnNames, wideColumns){
  # get only the wide format column names from the dataframe column names
  return(columnNames[grep(paste(wideColumns, collapse = "|"), columnNames)])
}

validateWide <- function(df, requiredColumns){
  tryCatch({
    requiredColumns <- tolower(requiredColumns)
    columnNames <- tolower(names(df)) # Get the column names from the dataframe

    wideColumns <- getWideColumns(columnNames, requiredColumns)
    wideColumnNames <- getWideColumnNames(columnNames, wideColumns)
    
    splitCols <- splitWideColumns(wideColumnNames)
    cols <- splitCols$cols
    colNumbers <- splitCols$colNumbers
    nArms <- splitCols$nArms
    
    nExpectedCols <- nArms * length(wideColumns)
    
    # Iterate through the wide columns (column names without the .n)
    for(col in wideColumns){
      # Iterate the number of arms (derived from the maximum .n)
      for(i in 1:nArms){
        # Check that for each arm there is a corresponding wide column
        if(!paste(col, i, sep = ".") %in% wideColumnNames){
          errorAlert(paste0("Expected column: ", paste(col, i, sep = "."), " but missing"))
          return(F)
        }
      }
    }
    # Check the number of columns are as expected
    
    if (length(cols) != nExpectedCols){
      errorAlert("There is a problem with the number of columns, did you miss a column?")
      return(F)
    }
    
  },
  error = function(e) {
    errorAlert(e$message)
    return(F)
  })
  return(T)
}

validateBinaryLong <- function(df){
  requiredColumns <- getRequiredBinaryLongColumns()
  validateColumnNames(df, requiredColumns)
}

validateContinuousLong <- function(df){
  requiredColumns <- getRequiredContinuousLongColumns()
  validateColumnNames(df, requiredColumns)
}

validateBinaryWide <- function(df){
  requiredColumns <- getRequiredBinaryWideColumns()
  validateWide(df, requiredColumns)
}

validateContinuousWide <- function(df){
  requiredColumns <- getRequiredContinuousWideColumns()
  validateWide(df, requiredColumns)
}

validateInput <- function(inputFile, type){
  tryCatch({
    print("Checking input file")
    if(is.null(inputFile)){
      print("Null input file")
      errorAlert("There is a problem with the uploaded file please try again")
      return(F)
    }
    if(is.null(type)){
      print("Null Type")
      errorAlert("An error occured, please try again, if this problem consiste please contact the developers", "Error #TP001")
      return(F)
    }
    
    if(is.null(inputFile$datapath)){
      print("Null datapath")
      errorAlert("An error occured, please try again, if this problem consiste please contact the developers", "Error #FU001")
      return(F)
    }
    
    if(!isFileExists(inputFile$datapath)){
      print("file doesn't exist")
      errorAlert("An error occured, please try again, if this problem consiste please contact the developers", "Error #FU002")
      return(F)
    }
    
    if(!isValidFileFormat(tools::file_ext(inputFile$name))){
      errorAlert(paste0("Accepted Files: ", paste0(getAcceptedFileFormats(), collapse = ", ")), "Error File Extension not Supported")
      return(F)
    }
    
    df <- rio::import(inputFile$datapath)
    
    if(is.null(df)){
      print("Null Data Fram")
      errorAlert("An error occured, the data appears to be empty, please try again, if this problem consiste please contact the developers", "Error #FU003")
      return(F)
    }
    
    if(isWide(df)){
      print("Assuming Wide Format")
      if(type == "binary"){
        return(validateBinaryWide(df))
      }
      else if(type == "continuous"){
        validateContinuousWide(df)
      }
    }
    else {
      print("Assuming long Format")
      if(type == "binary"){
        return(validateBinaryLong(df))
      }
      else if(type == "continuous"){
        return(validateContinuousLong(df))
      }
    }
    return(F)
  },
  error = function(e) {
    errorAlert(e$message)
    return(F)
  })
  return(F)
}