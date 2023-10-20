library(targets)
library(tarchetypes)
options(tidyverse.quiet = TRUE)
library(tidyverse)
source("R/functions_modelFitting.R")
source("R/functions_simulation.R")
source("R/functions_powerAnalysis.R")
source("R/functions_loadData.R")
source("R/functions_summariseData.R")
source("R/functions_modelResults.R")
tar_option_set(
  packages = c("ape", "cowplot", "ggcorrplot", "ggrepel", 
               "ggridges", "knitr", "ltm", "papaja", "phangorn", 
               "rethinking", "rstan", "tanggle", "tidybayes", 
               "tidytext", "tidyverse", "wordcloud"),
  deployment = "main"
)
options(
  clustermq.scheduler = "slurm", 
  clustermq.template = "slurm_clustermq.tmpl"
)

## power analysis static branching (see below)
#powerMap <-
#  tar_map(
#    values = tibble(strategy = 1:10),
#    # seeds for data simulation
#    tar_target(powerSeed, 1:100),
#    # simulate data for power analysis
#    tar_target(dPower, simulateData(n = 1019, errorRate = 0, seed = powerSeed,
#                                    # use intercepts from m1 model (UK only)
#                                    alphas = c(-1.48, -0.31, 1.43, 0.22, -0.39,
#                                               -0.22, 0.22, -0.87, -0.47, 2.31),
#                                    # detecting medium effect for one strategy
#                                    # no effect for all other strategies
#                                    betas = c(rep(0, times = strategy - 1), 1, 
#                                              rep(0, times = 10 - strategy))),
#               pattern = map(powerSeed)),
#    # fit model to simulated data and return posterior summary
#    tar_target(powerModel, fitPowerModel(dPower, compiledModel2, strategy),
#               deployment = "worker", pattern = map(dPower)),
#    # power
#    tar_target(power, mean(powerModel$`Q2.5` > 0))
#  )

# full workflow
list(
  
  #### Load and compile Stan models ####
  
  # model files
  tar_target(fileModel1.1, "stan/noTPP/punishStrat_noTPP_noPredictor.stan", format = "file"),
  tar_target(fileModel2.1, "stan/noTPP/punishStrat_noTPP_withPredictor.stan", format = "file"),
  tar_target(fileModel3.1, "stan/noTPP/punishStrat_noTPP_withTwoPredictors.stan", format = "file"),
  tar_target(fileModel4.1, "stan/noTPP/punishStrat_noTPP_withCategoricalPredictor.stan", format = "file"),
  tar_target(fileModel1.2, "stan/withTPP/punishStrat_withTPP_noPredictor.stan", format = "file"),
  tar_target(fileModel2.2, "stan/withTPP/punishStrat_withTPP_withPredictor.stan", format = "file"),
  tar_target(fileModel3.2, "stan/withTPP/punishStrat_withTPP_withTwoPredictors.stan", format = "file"),
  tar_target(fileModel4.2, "stan/withTPP/punishStrat_withTPP_withCategoricalPredictor.stan", format = "file"),
  # compile models
  tar_target(compiledModel1.1, stan_model(fileModel1.1), deployment = "worker"),
  tar_target(compiledModel2.1, stan_model(fileModel2.1), deployment = "worker"),
  tar_target(compiledModel3.1, stan_model(fileModel3.1), deployment = "worker"),
  tar_target(compiledModel4.1, stan_model(fileModel4.1), deployment = "worker"),
  tar_target(compiledModel1.2, stan_model(fileModel1.2), deployment = "worker"),
  tar_target(compiledModel2.2, stan_model(fileModel2.2), deployment = "worker"),
  tar_target(compiledModel3.2, stan_model(fileModel3.2), deployment = "worker"),
  tar_target(compiledModel4.2, stan_model(fileModel4.2), deployment = "worker"),
  
  #### Simulation ####
  
  # simulate data
  tar_target(simAlphas, c(0, -0.2, 1.5, -1, -1, -0.5, -0.1, -1, -0.5, 2)),
  tar_target(simBetas, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
  tar_target(dSim, simulateData(n = 100, errorRate = 0, alphas = simAlphas, betas = simBetas)),
  # fit model to simulated data
  tar_target(fitSimModel, fitModel2(dSim, compiledModel2.1, predictor = "x"), deployment = "worker"),
  tar_target(simPost, extract(fitSimModel)),
  # plot results
  tar_target(plotSim1, plotSimResults1(simAlphas, simPost)),
  tar_target(plotSim2, plotSimResults2(dSim, simPost)),
  
  #### Power analysis ####
  
  ## power to detect a medium effect (1)
  #powerMap,
  #tar_combine(power, powerMap[["power"]]),
  #tar_target(plotPower, plotPowerAnalysis(power)),
  
  #### Load and summarise UK/US data ####
  
  # load data
  tar_target(fileStudy1, "data/study1/study1_clean.csv", format = "file"),
  tar_target(fileStudy2, "data/study2/study2_clean.csv", format = "file"),
  tar_target(d, loadData(fileStudy1, fileStudy2)),
  # data exclusions
  tar_target(dExc, excludeData(d)),
  # cronbach's alpha for scales
  tar_target(alphaSDO,    cronbach.alpha(d[,paste0("SDO",    1:8)], na.rm = TRUE)),
  tar_target(alphaRWA,    cronbach.alpha(d[,paste0("RWA",    1:6)], na.rm = TRUE)),
  tar_target(alphaOpen,   cronbach.alpha(d[,paste0("Open",   1:4)], na.rm = TRUE)),
  tar_target(alphaConsc,  cronbach.alpha(d[,paste0("Consc",  1:4)], na.rm = TRUE)),
  tar_target(alphaExtra,  cronbach.alpha(d[,paste0("Extra",  1:4)], na.rm = TRUE)),
  tar_target(alphaAgree,  cronbach.alpha(d[,paste0("Agree",  1:4)], na.rm = TRUE)),
  tar_target(alphaNeur,   cronbach.alpha(d[,paste0("Neur",   1:4)], na.rm = TRUE)),
  tar_target(alphaHonest, cronbach.alpha(d[,paste0("Honest", 1:4)], na.rm = TRUE)),
  # plot sample characteristics
  tar_target(plotSample_UK, plotSampleStudy(d, country = "United Kingdom")),
  tar_target(plotSample_US, plotSampleStudy(d, country = "United States")),
  # plot wordclouds
  tar_target(plotWords_UK, plotWordcloud(d, country = "United Kingdom")),
  tar_target(plotWords_US, plotWordcloud(d, country = "United States")),
  # plot punishment decisions
  tar_target(plotDecisions, plotPunDecisions(d)),
  # plot slider ratings
  tar_target(plotSliders1, plotSliderRatings1(d)),
  tar_target(plotSliders2, plotSliderRatings2(d)),
  # plot overall survey correlations
  tar_target(plotSurveyCor, plotSurveyCorrelations(d)),
  # table of strategies
  tar_target(tableStrategies, makeStrategyTable()),
  # tables of comprehension rates
  tar_target(tableComp, makeCompTable(d)),
  # tables of raw strategy counts
  tar_target(tableStrategyCounts, makeStrategyCountTable(d)),
  tar_target(tableStrategyCounts_Mistake1, makeStrategyCountTableWithMistakes(d, nMistakes = 1)),
  tar_target(tableStrategyCounts_Mistake2, makeStrategyCountTableWithMistakes(d, nMistakes = 2)),
  tar_target(tableStrategyCounts_Mistake3, makeStrategyCountTableWithMistakes(d, nMistakes = 3)),
  # tables of top behaviour patterns
  tar_target(tablePatterns, makePatternsTable(d)),
  # plot splits graph of behaviour patterns
  tar_target(plotSplits, plotSplitsGraph(d)),
  
  #### Fit model of initial frequencies ####
  
  # model fitting
  tar_target(m1.1, fitModel1(d, compiledModel1.1, error = 0.05), deployment = "worker"),
  tar_target(m1.2, fitModel1(d, compiledModel1.2, error = 0.05), deployment = "worker"),
  # posterior samples
  tar_target(post1.1, extract(m1.1)),
  tar_target(post1.2, extract(m1.2)),
  # plot
  tar_target(plot1.1, plotModel1.1(post1.1)),
  tar_target(plot1.2, plotModel1.2(post1.2)),
  
  #### Fit models with continuous predictor ####
  
  tar_map(
    values = tibble(
      pred = c(
        "Age", paste0("SelfRate", 1:11), "RWA", "SDO",
        "Open", "Consc", "Extra", "Agree", "Neur", "Honest",
        "PolSlider", "SVOangle", "GodC", "Religiosity",
        "SES", "BringDown", "BringUp"
      ),
      xlab = c(
        "Age", "Slider 1 (punish people who harmed others)",
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
        "Bring people below me up a peg or two"
      ),
      xBreaks = c(
        list(c(20, 40, 60, 80)), rep(list(c(0, 25, 50, 75, 100)), times = 11),
        list(c(1, 3, 5, 7, 9)), rep(list(c(1, 3, 5, 7)), times = 7),
        list(c(0, 25, 50, 75, 100)), list(c(-20, 0, 20, 40, 60)),
        list(1:7), list(1:5), list(1:10), list(1:7), list(1:7)
      )
    ),
    names = "pred",
    # model fitting
    tar_target(m2.1, fitModel2(d, compiledModel2.1, error = 0.05, predictor = pred), deployment = "worker"),
    tar_target(m2.2, fitModel2(d, compiledModel2.2, error = 0.05, predictor = pred), deployment = "worker"),
    # posterior samples
    tar_target(post2.1, extract(m2.1)),
    tar_target(post2.2, extract(m2.2)),
    # plots
    tar_target(plot2.1, plotModel2.1(d, post2.1, pred, xlab, xBreaks, file = paste0("figures/modelResults/noTPP/model2_", pred, ".pdf"))),
    tar_target(plot2.2, plotModel2.2(d, post2.2, pred, xlab, xBreaks, file = paste0("figures/modelResults/withTPP/model2_", pred, ".pdf")))
  ),
  
  #### Fit models with two continuous predictors ####
  
  tar_map(
    values = tibble(
      pred1 = "RWA",
      pred2 = "SDO"
    ),
    names = c("pred1","pred2"),
    # model fitting
    tar_target(m3.1, fitModel3(d, compiledModel3.1, error = 0.05, predictor1 = pred1, predictor2 = pred2), deployment = "worker"),
    tar_target(m3.2, fitModel3(d, compiledModel3.2, error = 0.05, predictor1 = pred1, predictor2 = pred2), deployment = "worker"),
    # posterior samples
    tar_target(post3.1, extract(m3.1)),
    tar_target(post3.2, extract(m3.2))
  ),
  
  #### Fit models with categorical predictor ####
  
  tar_map(
    values = tibble(
      pred = c("Gender", "Ethnicity", "Student", "Education"),
      xlab = c("Gender", "Ethnicity", "Student", "Education")
    ),
    names = "pred",
    # model fitting
    tar_target(m4.1, fitModel4(d, compiledModel4.1, error = 0.05, predictor = pred), deployment = "worker"),
    tar_target(m4.2, fitModel4(d, compiledModel4.2, error = 0.05, predictor = pred), deployment = "worker"),
    # posterior samples
    tar_target(post4.1, extract(m4.1)),
    tar_target(post4.2, extract(m4.2)),
    # plots
    tar_target(plot4.1, plotModel4.1(d, post4.1, pred, xlab, file = paste0("figures/modelResults/noTPP/model3_", pred, ".pdf"))),
    tar_target(plot4.2, plotModel4.2(d, post4.2, pred, xlab, file = paste0("figures/modelResults/withTPP/model3_", pred, ".pdf")))
  ),
  
  #### Summary plots of model results ####
  
  ### no third-party strategy
  tar_target(plotSlidersNoTPP, plotAllSlidersNoTPP(list(post2.1_SelfRate1, post2.1_SelfRate2, post2.1_SelfRate3,
                                                        post2.1_SelfRate4, post2.1_SelfRate5, post2.1_SelfRate6,
                                                        post2.1_SelfRate7, post2.1_SelfRate8, post2.1_SelfRate9,
                                                        post2.1_SelfRate10, post2.1_SelfRate11))),
  tar_target(plotPersNoTPP, plotAllPersNoTPP(list(post2.1_Agree, post2.1_Consc, post2.1_Extra, post2.1_Honest,
                                                  post2.1_Neur, post2.1_Open, post2.1_SVOangle))),
  tar_target(plotPolRelNoTPP, plotAllPolRelNoTPP(list(post2.1_PolSlider, post3.1_RWA_SDO, post2.1_BringUp,
                                                      post2.1_BringDown, post2.1_Religiosity, post2.1_GodC))),
  tar_target(plotDemsNoTPP, plotAllDemsNoTPP(list(post2.1_Age, post2.1_SES, post4.1_Gender, post4.1_Student))),
  ### with third-party strategy
  tar_target(plotSlidersWithTPP, plotAllSlidersWithTPP(list(post2.2_SelfRate1, post2.2_SelfRate2, post2.2_SelfRate3,
                                                            post2.2_SelfRate4, post2.2_SelfRate5, post2.2_SelfRate6,
                                                            post2.2_SelfRate7, post2.2_SelfRate8, post2.2_SelfRate9,
                                                            post2.2_SelfRate10, post2.2_SelfRate11))),
  tar_target(plotPersWithTPP, plotAllPersWithTPP(list(post2.2_Agree, post2.2_Consc, post2.2_Extra, post2.2_Honest,
                                                      post2.2_Neur, post2.2_Open, post2.2_SVOangle))),
  tar_target(plotPolRelWithTPP, plotAllPolRelWithTPP(list(post2.2_PolSlider, post3.2_RWA_SDO, post2.2_BringUp,
                                                          post2.2_BringDown, post2.2_Religiosity, post2.2_GodC))),
  tar_target(plotDemsWithTPP, plotAllDemsWithTPP(list(post2.2_Age, post2.2_SES, post4.2_Gender, post4.2_Student))),
  
  #### Render report ####
  
  tar_render(report, "report.Rmd"),
  
  #### Render manuscript ####
  
  tar_render(manuscript, "manuscript.Rmd"),
  
  #### Session info ####
  
  # print session info for reproducibility
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
  
)
