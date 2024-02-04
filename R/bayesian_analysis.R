fit_model <- function(
  df,
  data_type,
  reference_component,
  random_effects,
  outcome_measure,
  chains = 3,
  warmup = 1000,
  iter = 3000,
  seed = 12345,
  max_treedepth = 10,
  adapt_delta = 0.95,
  stepsize = 0.01
) {
  if (data_type == "binary") {
    if (random_effects) {
      return(
        MetaCNMABayes::fit_binary_re(
          df,
          reference_component,
          outcome_measure,
          chains,
          warmup,
          iter,
          seed,
          max_treedepth,
          adapt_delta,
          stepsize
        )
      )
    } else {
      return(
        MetaCNMABayes::fit_binary_re(
          df,
          reference_component,
          outcome_measure,
          chains,
          warmup,
          iter,
          seed,
          max_treedepth,
          adapt_delta,
          stepsize
        )
      )
    }
  } else if (data_type == "continuous") {
    if (random_effects) {
      return(
        MetaCNMABayes::fit_continuous_re(
          df,
          reference_component,
          outcome_measure,
          chains,
          warmup,
          iter,
          seed,
          max_treedepth,
          adapt_delta,
          stepsize
        )
      )
    } else {
      return(
        MetaCNMABayes::fit_continuous_re(
          df,
          reference_component,
          outcome_measure,
          chains,
          warmup,
          iter,
          seed,
          max_treedepth,
          adapt_delta,
          stepsize
        )
      )
    }
  }
}