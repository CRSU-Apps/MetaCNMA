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

#' @title Get Bayesian Sampler Diagnostics
#' @description Returns the sampler diagnostics from a fitter STAN model
#' @param stan_fit a fitted STAN model
#' @return a data.frame containing divergent transitions
#' and iterations which exceeded the max treedepth
#' @details Internal function to get sampler diagnostics from
#' a fitted stan model, categorising them as good or bad
#' @examples
#' \dontrun{
#' if(interactive()){
#'  get_sampler_diagnostics(model)
#'  }
#' }
#' @seealso
#'  \code{\link[rstan]{check_hmc_diagnostics}}
#'  \code{\link[dplyr]{case_when}}
#' @rdname get_sampler_diagnostics
#' @importFrom rstan get_num_divergent get_num_max_treedepth
#' @importFrom dplyr case_when
get_sampler_diagnostics <- function(stan_fit) {
  # Get the number of divergent transitions
  no_divergent <- rstan::get_num_divergent(stan_fit)
  # Get the number of iterations which exceeded the max tree depth
  no_tree_depth <- rstan::get_num_max_treedepth(stan_fit)
  # Return as a data.frame, categorising them as good or bad
  # if they exceeded 1.
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
          no_divergent >= 1 ~ "bad"
        ),
        dplyr::case_when(
          no_tree_depth < 1 ~ "good",
          no_tree_depth >= 1 ~ "bad"
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
  return(
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
  # Get the relevent stan parameters
  pars <- get_pars(random_effects)
  # Extract the RHATS (rounding to 2dp)
  rhats <- round(
    rstan::summary(
      stan_fit, pars = pars
    )$summary[, "Rhat"], 2
  )
  # Return as tibble categorising based on thresholds
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

#' @title Get density plots
#' @description Get the density plots from a fitted STAN model.
#' @param stan_fit A fitted STAN model
#' @param random_effects Whether the model was fit with random effects
#' @param ncol Number of columns per row of plots, Default: 5
#' @return A ggplot2 object of density plots
#' @details Returns a ggplot2 object of density plots
#' from a given fitted STAN model.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  get_density_plots(stan_fit, TRUE)
#'  }
#' }
#' @seealso
#'  \code{\link[rstan]{Plots}}
#' @rdname get_density_plots
#' @importFrom rstan stan_dens
get_density_plots <- function(
  stan_fit,
  random_effects,
  ncol = 5
) {
  # Get the model parametes of interest
  pars <- get_pars(random_effects)
  # Return the density plots
  return(
    rstan::stan_dens(
      stan_fit,
      pars = pars
    )
  )
}

#' @title Get Trace Plots
#' @description Get the trace plots from a fitted STAN model.
#' @param stan_fit A fitted STAN model
#' @param random_effects Whether the model was fit with random effects
#' @return A ggplot2 object of trace plots
#' @details Returns a ggplot3 object of trace plots
#' from a given STAN model.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  get_tract_plots(stan_fit, TRUE)
#'  }
#' }
#' @seealso 
#'  \code{\link[rstan]{Plots}}
#' @rdname get_trace_plots
#' @export 
#' @importFrom rstan stan_trace
get_trace_plots <- function(
  stan_fit,
  random_effects
) {
  # Get the model parametes of interest
  pars <- get_pars(random_effects)
  # Return the trace plots
  return(
    rstan::stan_trace(
      stan_fit,
      pars = pars
    )
  )
}