library(targets)
library(tarchetypes)
library(tidyverse)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("ggridges", "rstan"))

# full workflow
list(
  # simulation
  tar_target(simProbs, c(0.10, 0.40, 0.25, 0.25)),
  tar_target(dSim, simulateData(probs = simProbs, errorRate = 0.05)),
  # fit model
  tar_target(fileModel, "stan/punishStrat.stan", format = "file"),
  tar_target(compiledModel, stan_model(fileModel)),
  tar_target(fitSimModel, fitModel(dSim, compiledModel)),
  tar_target(simPost, extract(fitSimModel)),
  # plot results against simulated probabilities
  tar_target(plotSim, plotSimResults(simProbs, simPost))
)