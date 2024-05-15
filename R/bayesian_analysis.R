### Bayesian Anaylisis Functions

#' @title Function to fit STAN CNMA models
#' @description Wrapper function to fit appropriate STAN CNMA model
#' for a given data type and whether or not the model should be
#' fit using random effects
#' @param df A formatted data frame containing the appropriate fields
#' see related functions for more information
#' @param data_type Either "binary" or "continuous"
#' @param reference_component The reference components as a character vector
#' @param random_effects Boolean: should the model use random effects
#' @param outcome_measure The outcome measure in uppercase e.g. "MD"
#' see related functions for supported outcome measures
#' @param chains Number of chains, Default: 3
#' @param warmup Number of warmup iterations (burn-in), Default: 1000
#' @param iter Number of iterations, Default: 3000
#' @param seed Seed, Default: 12345
#' @param max_treedepth Tree depth, Default: 10
#' @param adapt_delta Adapt delta, Default: 0.95
#' @param stepsize Stepsize, Default: 0.01
#' @return A object of type MetaCNMABayes, see related functions
#' for further information
#' @details This function calls the appropriate MetaBayesCNMA functions
#' based of the data_type and random_effects parameters, in future this function
#' will also use the outcome measure as 'SMD' models will be added.
#' @examples
#' \dontrun{
#' if(interactive()){
#'    fit_model(
#'      df,
#'      "continuous",
#'      TRUE,
#'      "MD",
#'    )
#'  }
#' }
#' @seealso
#' \code{\link[MetaCNMABayes]{fit_binary_re}},
#' \code{\link[MetaCNMABayes]{fit_binary_fe}},
#' \code{\link[MetaCNMABayes]{fit_continuous_re}},
#' \code{\link[MetaCNMABayes]{fit_continuous_fe}}
#' @rdname fit_model
#' @importFrom MetaCNMABayes fit_binary_re fit_binary_fe
#' @importFrom MetaCNMABayes fit_continuous_re fit_continuous_fe
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

#' @title Get STAN parameter names
#' @description Given whether or not the model was
#' fitted using random effects, returns a list of
#' STAN parameters of interest
#' @param random_effects BOOLEAN was the model fit with
#' random effects?
#' @return A character vector of STAN parameters
#' @details Internal function to return which STAN parameters
#' are of interest given whether or not the model was fit
#' using random effects.
#' @examples
#' \dontrun{
#' if(interactive()){
#'    get_pars(TRUE)
#'  }
#' }
#' @rdname get_pars
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

#' @title Get Rhat Diagnostics
#' @description Produces a tibble of Rhat diagnostics
#' @param stan_fit Fitted Stan Model
#' @param random_effects BOOLEAN was the model fit with
#' random effects?
#' @return tibble of Rhat diagnostics
#' @details Produces a tibble of Rhat diagnostics for parameters
#' dependant on whether or not the model used random effects
#' including the Rhat values and there respective category:
#' good: <1.05
#' borderline: ≥1.05 ≤1.10
#' bad > 1.10
#' @examples
#' \dontrun{
#' if(interactive()){
#'    get_rhat_diagnostics(
#'      model_fit,
#'      TRUE
#'    )
#'  }
#' }
#' @seealso
#'  \code{\link[rstan]{character(0)}}
#'  \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{case_when}}
#' @rdname get_rhat_diagnostics
#' @importFrom rstan summary
#' @importFrom dplyr tibble case_when
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