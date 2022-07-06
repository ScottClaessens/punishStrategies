library(targets)
library(tarchetypes)
library(tidyverse)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("ggridges", "qualtRics", "rethinking", 
                            "rstan", "tidybayes"))

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
  
  ##############
  # Pilot data #
  ##############
  
  # load pilot data
  tar_target(filePilotData, "data/pilot/PILOT_PUN_BATTERY_GAMES_July+1,+2022_10.41.csv",
             format = "file"),
  tar_target(dPilot, loadPilotData(filePilotData)),
  # fit models to pilot data
  tar_target(fitPilotModel1, fitModel1(dPilot, compiledModel1)),
  tar_target(fitPilotModel2, fitModel2(dPilot %>% filter(!is.na(SelfRate_9_std)), 
                                       compiledModel2, predictor = "SelfRate_9_std")),
  tar_target(pilotPost1, extract(fitPilotModel1)),
  tar_target(pilotPost2, extract(fitPilotModel2)),
  # plot results
  tar_target(plotPilot1, plotPilotResults1(pilotPost1)),
  tar_target(plotPilot2, plotPilotResults2(dPilot, pilotPost2, predictor = "SelfRate_9_std",
                                           xaxis = "Self-reported usage of anti-punish strategy (std)"))
  
)