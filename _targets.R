library(targets)
library(tarchetypes)
library(tidyverse)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("cowplot", "ggridges", "rethinking", 
                            "rstan", "tidybayes", "tidytext",
                            "wordcloud"))

# full workflow
list(
  
  ################################
  # Load and compile stan models #
  ################################
  
  # model files
  tar_target(fileModel1, "stan/punishStrat_noPredictor.stan", format = "file"),
  tar_target(fileModel2, "stan/punishStrat_withPredictor.stan", format = "file"),
  # compile models
  tar_target(compiledModel1, stan_model(fileModel1)),
  tar_target(compiledModel2, stan_model(fileModel2)),
  
  ##############
  # Simulation #
  ##############
  
  # simulate data
  tar_target(simAlphas, c(0, -0.2, 1.5, -1, -1, -0.5, -0.1, -1, -0.5, 2)),
  tar_target(simBetas, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
  tar_target(dSim, simulateData(n = 50, errorRate = 0, 
                                alphas = simAlphas, betas = simBetas)),
  # fit model to simulated data
  tar_target(fitSimModel, fitModel2(dSim, compiledModel2, predictor = "x")),
  tar_target(simPost, extract(fitSimModel)),
  # plot results
  tar_target(plotSim1, plotSimResults1(simAlphas, simPost)),
  tar_target(plotSim2, plotSimResults2(dSim, simPost)),
  
  ###########
  # Study 1 #
  ###########
  
  # load study 1 data
  tar_target(fileStudy1, "data/study1/study1clean.csv", format = "file"),
  tar_target(d1, loadData1(fileStudy1)),
  # plot sample characteristics
  tar_target(plotSample, plotSampleStudy1(d1)),
  # plot wordcloud
  tar_target(plotWords, plotWordcloud(d1))
  
)