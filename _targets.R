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
  tar_target(simAlphas, c(-0.5, -0.5, 0.7, -0.7, -0.5, -0.3, -0.5, -0.5, 2)),
  tar_target(simBetas, c(0, 0, 1, 0, 0, 0, 0, 0, -1)),
  tar_target(dSim, simulateData(n = 500, errorRate = 0.05, alphas = simAlphas, betas = simBetas)),
  # fit model to simulated data
  tar_target(fitSimModel, fitModel(dSim, compiledModel)),
  tar_target(simPost, extract(fitSimModel)),
  # plot results
  tar_target(plotSim, plotSimResults1(simAlphas, simPost))
  
)