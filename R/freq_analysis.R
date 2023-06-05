freqPairwise <- function(globalData, globalFreq){
  tryCatch({
    # Reset pairwise
    globalFreq$pairwise <- NULL
    # If the data is not valid do not format the data
    if( !isDataValid(globalData) ){
      print("This error occured trying to format the data")
      stop("There is a problem with the data, please check data has been uploaded and is valid")
    }
    # If the data has not been formatted yet, format it now.
    if(is.null(globalFreq$data)){
      if (is.null(globalFreq$data)) {
        withProgress({
          formatData(globalData, globalFreq)
        },
        message = "Formatting Data")
      }
    }
    # Initialise temporary dataframe
    tmpData <- NULL
    if(globalData$format == "wide"){
      tmpData <- dataWideToLong(globalFreq$data)
    }
    else if(globalData$format == "long"){
      tmpData <- globalFreq$data
    }
    else{
      stop("Error: DF001 data format unknown")
    }
    print("running pairwise")
    if(globalData$type == "continous"){
      globalFreq$pairwise <- runPairwiseContinous(tmpData)
    }
    else if(globalData$type == "binary"){
      globalFreq$pairwise <- runPairwiseBinary(tmpData)
    }
    else{
      stop("Error: DT001 data type unknown")
    }
  },
  error = function(e) {
    errorAlert(e$message)
    invalidateData(globalData, globalFreq)
    return(F)
  })
}

runPairwiseContinous <- function(df){
  
  pairwise(
    treat = components,
    n = total,
    mean = mean,
    sd = sd,
    studlab = study,
    data = df
  )
  
}

runPairwiseLong <- function(df){
  
  pairwise(
    treat = components,
    n = total,
    event = events,
    studlab = author,
    data = df
  )
  
}

getComponents <- function(pw){
  # Get all the components (treat columns)
  components <- pw %>% select(contains("treat"))
  tmpComp <- NULL
  for(i in 1:ncol(components)){
    tmpComp <- cbind(tmpComp, components[[i]])
  }
  components <- tmpComp
  # Add all components to a single character vector separating with extra + signs
  components <- paste(components, collapse = "+")
  # Split the single vector apart by +
  components <- strsplit(components, "\\+")[[1]]
  # Convert to factor (for speed)
  components <- as.factor(components)
  # Use levels to get unique components
  components <- levels(components)
}

getCombinationComponents <- function(pw){
  components <- pw %>% select(contains("treat"))
  tmpComp <- NULL
  for(i in 1:ncol(components)){
    tmpComp <- cbind(tmpComp, components[[i]])
  }
  components <- as.factor(tmpComp)
  levelList <- list()
  for(i in 1:length(levels(components))){
    levelList[levels(components)[[i]]] = 0
  }
  for(component in components){
    print(component)
    print(levelList[[component]])
    levelList[[component]] =  levelList[[component]] + 1
  }
  return(levelList)
}

getSummary <- function(pw){
  list(
    nStudies = nrow(pw),
    components = getComponents(pw),
    combinationComponents = getCombinationComponents(pw)
  )
}

runNetmeta <- function(pw, ref = "Control", comb.random = T){
  net1 <- netmeta(pw, ref= ref, comb.random= comb.random)
}

renderNetplot <- function(nm){
  renderPlot(
    netgraph(nm)
  )
}

runNetcomb <- function(nm, inactive = "Control"){
  netcomb(nm, inactive = inactive)
}

netcombSummary <- function(nc){
  nc.summary <- summary(nc)
  data.frame(
    "Characteristic" = c(
      "Number of Studies",
      "Number of Components",
      "Number of Interventions"
    ),
    "Value" = c(
      nc$k,
      nc$c,
      nc$n
    )
  )
}

renderNetForest <- function(nc, component = T){
  if(component){
    print("rendering plot")
    return(renderPlot(forest(nc$Comp.random,
                             sei= nc$seComp.random,
                             slab = nc$comps)))
  }
  renderPlot(forest(nc))
}
