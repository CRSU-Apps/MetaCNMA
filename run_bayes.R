#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

source("R/bayesian_analysis.R")

df <- rio::import(args[1])
data_type <- args[3]
reference_component <- args[4]
random_effects <- as.logical(args[5])
outcome_measure <- args[6]

model_output <- fit_model(
  df,
  data_type,
  reference_component,
  random_effects,
  outcome_measure
)

rio::export(model_output, args[2])