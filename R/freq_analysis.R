runPairwise <- function(df){
  
  pairwise(
    treat = trt,
    n = n,
    mean = mean,
    sd = sd,
    studlab = author,
    data = df
  )
  
}

runNetmeta <- function(pw, ref = "Control", comb.random = T){
  net1 <-netmeta(pw, ref= ref, comb.random= comb.random)
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
