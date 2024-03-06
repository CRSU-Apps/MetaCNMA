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
        MetaCNMABayes::fit_binary_fe(
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
        MetaCNMABayes::fit_continuous_fe(
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

get_sampler_diagnostics <- function(stan_fit) {
  no_divergent <- rstan::get_num_divergent(stan_fit)
  no_tree_depth <- rstan::get_num_max_treedepth(stan_fit)
  return(
    data.frame(
      Info = c(
        "divergent transitions",
        "iterations which exceeded max treedepth"
      ),
      Number = c(
        no_divergent,
        no_tree_depth
      ),
      Category = c(
        dplyr::case_when(
          no_divergent < 1 ~ "good",
          no_divergent > 1 ~ "bad"
        ),
        dplyr::case_when(
          no_tree_depth < 1 ~ "good",
          no_tree_depth > 1 ~ "bad"
        )
      )
    )
  )
}

get_pars <- function(random_effects) {
  if (random_effects) {
    return(
      c(
        "mu",
        "d",
        "delta",
        "sdbt"
      )
    )
  }
  return (
    c(
      "mu",
      "d"
    )
  )
}

get_rhat_diagnostics <- function(stan_fit, random_effects) {
  pars <- get_pars(random_effects)
  rhats <- round(
    rstan::summary(
      stan_fit, pars = pars
    )$summary[, "Rhat"], 2
  )
  return(
    dplyr::tibble(
      Parameter = names(rhats),
      Rhat = rhats,
      Category = dplyr::case_when(
        Rhat < 1.05 ~ "good",
        Rhat %in% c(1.05, 1.10) ~ "borderline",
        Rhat > 1.10 ~ "bad"
      )
    )
  )
}

get_denisty_plots <- function(
  stan_fit,
  random_effects,
  ncol = 5
) {
  pars <- get_pars(random_effects)
  return(
    rstan::stan_dens(
      stan_fit,
      pars = pars
    )
  )
}

get_trace_plots <- function(
  stan_fit,
  random_effects
) {
  pars <- get_pars(random_effects)
  return(
    rstan::stan_trace(
      stan_fit,
      pars = pars
    )
  )
}