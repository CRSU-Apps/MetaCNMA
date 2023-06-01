freqPairwise <- function(globalData, globalFreq){
  tryCatch({
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
    if(globalFreq$format == "wide"){
      tmpData <- dataWideToLong(globalFreq$data)
    }
    else if(globalFreq$format == "long"){
      tmpData <- globalFreq$data
    }
    else{
      stop("Error: DF001 data format unknown")
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
    treat = componentd,
    n = total,
    event = events,
    studlab = author,
    data = df
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
