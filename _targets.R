library(targets)
library(tarchetypes)
options(tidyverse.quiet = TRUE)
library(tidyverse)
source("R/functionsModelFitting.R")
source("R/functionsSimulation.R")
source("R/functionsStudy1.R")
source("R/functionsPowerAnalysis.R")
tar_option_set(
  packages = c("cowplot", "ggcorrplot", "ggridges", "ltm", 
               "rethinking", "rstan", "tidybayes", "tidytext", 
               "tidyverse", "wordcloud"),
  deployment = "main"
)
options(
  clustermq.scheduler = "slurm", 
  clustermq.template = "slurm_clustermq.tmpl"
)

# power analysis static branching (see below)
powerMap <-
  tar_map(
    values = tibble(strategy = 1:10),
    # seeds for data simulation
    tar_target(powerSeed, 1:100),
    # simulate data for power analysis
    tar_target(dPower, simulateData(n = 1019, errorRate = 0, seed = powerSeed,
                                    # use intercepts from m1 model (UK only)
                                    alphas = c(-1.48, -0.31, 1.43, 0.22, -0.39,
                                               -0.22, 0.22, -0.87, -0.47, 2.31),
                                    # detecting medium effect for one strategy
                                    # no effect for all other strategies
                                    betas = c(rep(0, times = strategy - 1), 1, rep(0, times = 10 - strategy))),
               pattern = map(powerSeed)),
    # fit model to simulated data and return posterior summary
    tar_target(powerModel, fitPowerModel(dPower, compiledModel2, strategy),
               deployment = "worker", pattern = map(dPower)),
    # power
    tar_target(power, mean(powerModel$`Q2.5` > 0))
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
  
  ### load study 1 data
  tar_target(fileStudy1, "data/study1/study1_clean.csv", format = "file"),
  tar_target(d1, loadData1(fileStudy1)),
  
  ### data exclusions
  tar_target(d1Exc, excludeData1(d1)),
  
  ### cronbach's alpha for scales
  tar_target(alphaSDO,    cronbach.alpha(d1[,paste0("SDO",    1:8)], na.rm = TRUE)),
  tar_target(alphaRWA,    cronbach.alpha(d1[,paste0("RWA",    1:6)], na.rm = TRUE)),
  tar_target(alphaOpen,   cronbach.alpha(d1[,paste0("Open",   1:4)], na.rm = TRUE)),
  tar_target(alphaConsc,  cronbach.alpha(d1[,paste0("Consc",  1:4)], na.rm = TRUE)),
  tar_target(alphaExtra,  cronbach.alpha(d1[,paste0("Extra",  1:4)], na.rm = TRUE)),
  tar_target(alphaAgree,  cronbach.alpha(d1[,paste0("Agree",  1:4)], na.rm = TRUE)),
  tar_target(alphaNeur,   cronbach.alpha(d1[,paste0("Neur",   1:4)], na.rm = TRUE)),
  tar_target(alphaHonest, cronbach.alpha(d1[,paste0("Honest", 1:4)], na.rm = TRUE)),
  
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
  # survey correlations
  tar_target(plotSurveyCor, plotSurveyCorrelations(d1)),
  
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
  
  ### fit models with continuous predictor
  tar_map(
    values = tibble(
      pred = c("Age", paste0("SelfRate", 1:11), "RWA", "SDO",
               "Open", "Consc", "Extra", "Agree", "Neur", "Honest",
               "PolSlider", "SVOangle", "GodC", "Religiosity",
               "SES", "BringDown", "BringUp"),
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
               "Slider 11 (punish if they harmed me but not if they harmed others)",
               "Right-wing authoritarianism","Social dominance orientation",
               "Openness to experience","Conscientiousness","Extraversion",
               "Agreeableness","Neuroticism","Honesty-humility",
               "Political ideology (0 = left-wing, 100 = right-wing)",
               "Social Value Orientation angle",
               "God controls the events in the world",
               "How religious are you?", "Socio-economic status (ladder)",
               "Bring people above me down a peg or two",
               "Bring people below me up a peg or two"),
      xBreaks = c(list(c(20, 40, 60, 80)), rep(list(c(0, 25, 50, 75, 100)), times = 11),
                  list(c(1, 3, 5, 7, 9)), rep(list(c(1, 3, 5, 7)), times = 7),
                  list(c(0, 25, 50, 75, 100)), list(c(-20, 0, 20, 40, 60)),
                  list(1:7), list(1:5), list(1:10), list(1:7), list(1:7))
    ),
    names = "pred",
    # model fitting
    tar_target(m2, fitModel2(d1, compiledModel2, error = 0.05, predictor = pred),
               deployment = "worker"),
    # posterior samples
    tar_target(post2, extract(m2)),
    # plots
    tar_target(plot2, plotModelPred(d1, post2, pred, xlab, xBreaks,
                                    file = paste0("figures/study1/predictors/", pred, ".pdf")))
    
  ),
  
  ### fit models with categorical predictor
  tar_map(
    values = tibble(
      pred = c("Gender", "Ethnicity", "Student", "Education"),
      xlab = c("Gender", "Ethnicity", "Student", "Education")
    ),
    names = "pred",
    # model fitting
    tar_target(m3, fitModel3(d1, compiledModel3, error = 0.05, predictor = pred),
               deployment = "worker"),
    # posterior samples
    tar_target(post3, extract(m3)),
    # plots
    tar_target(plot3, plotModelPredCat(d1, post3, pred, xlab,
                                       file = paste0("figures/study1/predictors/", pred, ".pdf")))
  ),
  
  ### report
  tar_render(report, "report.Rmd"),
  
  #### Power analysis ####
  
  # power to detect a medium effect (1)
  powerMap,
  tar_combine(power, powerMap[["power"]]),
  tar_target(plotPower, plotPowerAnalysis(power)),
  
  #### Session Info ####
  
  # print session info for reproducibility
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
  
)
