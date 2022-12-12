library(targets)
library(tarchetypes)
options(tidyverse.quiet = TRUE)
library(tidyverse)
source("R/functionsModelFitting.R")
source("R/functionsSimulation.R")
source("R/functionsStudy1.R")
tar_option_set(packages = c("cowplot", "ggridges", "rethinking", "rstan",
                            "tidybayes", "tidytext", "tidyverse", "wordcloud"),
               deployment = "main")
options(
  clustermq.scheduler = "slurm", 
  clustermq.template = "slurm_clustermq.tmpl"
)

# full workflow
list(
  
  #### Load and compile Stan models ####
  
  # model files
  tar_target(fileModel1, "stan/punishStrat_noPredictor.stan", format = "file"),
  tar_target(fileModel2, "stan/punishStrat_withPredictor.stan", format = "file"),
  tar_target(fileModel3, "stan/punishStrat_withCategoricalPredictor.stan", format = "file"),
  # compile models
  tar_target(compiledModel1, stan_model(fileModel1), deployment = "worker"),
  tar_target(compiledModel2, stan_model(fileModel2), deployment = "worker"),
  tar_target(compiledModel3, stan_model(fileModel3), deployment = "worker"),
  
  #### Simulation ####
  
  # simulate data
  tar_target(simAlphas, c(0, -0.2, 1.5, -1, -1, -0.5, -0.1, -1, -0.5, 2)),
  tar_target(simBetas, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
  tar_target(dSim, simulateData(n = 50, errorRate = 0, 
                                alphas = simAlphas, betas = simBetas)),
  # fit model to simulated data
  tar_target(fitSimModel, fitModel2(dSim, compiledModel2, predictor = "x"),
             deployment = "worker"),
  tar_target(simPost, extract(fitSimModel)),
  # plot results
  tar_target(plotSim1, plotSimResults1(simAlphas, simPost)),
  tar_target(plotSim2, plotSimResults2(dSim, simPost)),
  
  #### Study 1 ####
  
  # load study 1 data
  tar_target(fileStudy1, "data/study1/study1clean.csv", format = "file"),
  tar_target(d1, loadData1(fileStudy1)),
  
  ### plots
  # sample characteristics
  tar_target(plotSample, plotSampleStudy1(d1)),
  # wordcloud
  tar_target(plotWords, plotWordcloud(d1)),
  # punishment decisions
  tar_target(plotDecisions, plotPunDecisions(d1)),
  # slider ratings
  tar_target(plotSliders1, plotSliderRatings1(d1)),
  tar_target(plotSliders2, plotSliderRatings2(d1)),
  
  ### tables
  # comprehension rates
  tar_target(tableComp, makeCompTable(d1)),
  # raw strategy counts
  tar_target(tableStrategyCounts, makeStrategyCountTable(d1)),
  # top five behaviour patterns
  tar_target(tablePatterns, makePatternsTable(d1)),
  
  ### fit model of initial frequencies
  # model fitting
  tar_target(m1, fitModel1(d1, compiledModel1, error = 0.05), deployment = "worker"),
  # posterior samples
  tar_target(post1, extract(m1)),
  # plot
  tar_target(plot1, plotModel1(post1)),
  
  ### fit models with predictors
  tar_map(
    values = tibble(
      stdPred = c("AgeStd", paste0("SelfRate", 1:11, "Std")),
      unstdPred = c("Age", paste0("SelfRate_", 1:11)),
      xlab = c("Age", "Slider 1 (punish people who harmed others)",
               "Slider 2 (have a higher final bonus than others)",
               "Slider 3 (avoid having a lower final bonus than others)",
               "Slider 4 (wanted all players to have the same final bonus)",
               "Slider 5 (stop others from cheating)",
               "Slider 6 (show that I disapproved of others' actions)",
               "Slider 7 (made decisions at random)",
               "Slider 8 (punish people who DID NOT harm me or others)",
               "Slider 9 (didn't want to reduce anyone's bonus)",
               "Slider 10 (didn't want to PAY to reduce bonus, but would have if free)",
               "Slider 11 (punish if they harmed me but not if they harmed others)"),
      xBreaks = c(list(c(20, 40, 60, 80)), rep(list(c(0, 25, 50, 75, 100)), times = 11))
    ),
    names = "unstdPred",
    # model fitting
    tar_target(m2, fitModel2(d1, compiledModel2, error = 0.05, predictor = stdPred),
               deployment = "worker"),
    # posterior samples
    tar_target(post2, extract(m2)),
    # plots
    tar_target(plot2, plotModelPred(d1, post2, stdPred, unstdPred, xlab, xBreaks,
                                    file = paste0("figures/study1/predictors/", unstdPred, ".pdf")))
    
  ),
  
  ### report
  tar_render(report, "report.Rmd"),
  
  #### Session Info ####
  
  # print session info for reproducibility
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
  
)
