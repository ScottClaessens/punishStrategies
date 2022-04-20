library(targets)
library(tarchetypes)
library(tidyverse)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("ggridges", "rethinking", "rstan"))

# full workflow
list(
  
  # load and compile stan model
  tar_target(fileModel, "stan/punishStrat.stan", format = "file"),
  tar_target(compiledModel, stan_model(fileModel)),
  # simulation
  tar_target(simProbs, c(0.04, 0.04, 0.15, 0.03, 0.05, 0.07, 0.05, 0.02, 0.55)),
  tar_target(dSim, simulateData(probs = simProbs, errorRate = 0.05)),
  # fit model to simulated data
  tar_target(fitSimModel, fitModel(dSim, compiledModel)),
  tar_target(simPost, extract(fitSimModel)),
  # plot results against simulated probabilities
  tar_target(plotSim, plotSimResults(simProbs, simPost))
  
)