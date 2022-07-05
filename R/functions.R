# custom functions

# simulate data
simulateData <- function(n = 500, seed = 2113, errorRate = 0, alphas, betas) {
  # set simulation seed
  set.seed(seed)
  # n = number of participants
  # each participant has a score on an individual difference measure: x
  x <- rnorm(n)
  # assume that each participant follows a different punishment strategy
  # 01. Competitive
  # 02. Avoid disadvantageous inequality
  # 03. Egalitarian
  # 04. Seek advantageous inequality
  # 05. Retributive
  # 06. Deterrent
  # 07. Norm-enforcing
  # 08. Exclusively antisocial punishment
  # 09. Random choice
  # 10. Never punish
  # the punishment strategy is predicted by x
  strategies <- c()
  for (i in 1:n) {
    # predict probabilities with x
    probs <- softmax(alphas + betas*x[i])
    strategies[i] <- sample(1:10, size = 1, replace = TRUE, prob = probs)
  }
  # each participant plays a series of games developed to tease apart strategies
  # we measure punishment behaviours in these games
  # 0 = no punish, 1 = punish
  # create function to simulate punishment behaviour based on strategy and error rate
  simulateBehaviour <- function(punStrats = c()) {
    out <- c()
    for (i in 1:n) {
      if (strategies[i] %in% punStrats) {
        out[i] <- rbinom(1, 1, prob = 1 - errorRate) # punish (with error)
      } else if (strategies[i] == 9) {
        out[i] <- rbinom(1, 1, prob = 0.5)           # random choice
      } else if (!(strategies[i] %in% punStrats)) {
        out[i] <- rbinom(1, 1, prob = 0 + errorRate) # don't punish (with error)
      }
      # small chance of failed comprehension and missing data
      out[i] <- ifelse(rbinom(1, 1, prob = 0.01) == 0, out[i], -999)
    }
    return(out)
  }
  # game 1 - No DI 1
  pun1_1 <- simulateBehaviour(punStrats = c(1, 5, 6, 7))
  pun1_2 <- simulateBehaviour(punStrats = c(1, 8))
  # game 2 - No DI 2
  pun2_1 <- simulateBehaviour(punStrats = c(1, 4, 5, 6, 7))
  pun2_2 <- simulateBehaviour(punStrats = c(1, 8))
  # game 3 - No DI 3 (Computer)
  pun3_1 <- simulateBehaviour(punStrats = c(1, 4, 5))
  pun3_2 <- simulateBehaviour(punStrats = c(1, 8))
  # game 4 - No DI 4 (1:1 Fee-Fine)
  pun4_1 <- simulateBehaviour(punStrats = c(5, 6, 7))
  pun4_2 <- simulateBehaviour(punStrats = c(8))
  # game 5 - DI
  pun5_1 <- simulateBehaviour(punStrats = c(1, 2, 3, 5, 6, 7))
  pun5_2 <- simulateBehaviour(punStrats = c(1, 8))
  # game 6 - 3PP
  pun6_1 <- simulateBehaviour(punStrats = c(1, 3, 7))
  pun6_2 <- simulateBehaviour(punStrats = c(1, 8))
  # put together final dataset
  out <- tibble(id = 1:n, strategies, x, 
                pun1_1, pun1_2, pun2_1, pun2_2, 
                pun3_1, pun3_2, pun4_1, pun4_2,
                pun5_1, pun5_2, pun6_1, pun6_2)
  return(out)
}

# fit stan model without predictor
fitModel1 <- function(d, compiledModel1, error = 0) {
  # list for stan
  dataList <-
    list(
      N = nrow(d),
      pun1_1 = d$pun1_1,
      pun1_2 = d$pun1_2,
      pun2_1 = d$pun2_1,
      pun2_2 = d$pun2_2,
      pun3_1 = d$pun3_1,
      pun3_2 = d$pun3_2,
      pun4_1 = d$pun4_1,
      pun4_2 = d$pun4_2,
      pun5_1 = d$pun5_1,
      pun5_2 = d$pun5_2,
      pun6_1 = d$pun6_1,
      pun6_2 = d$pun6_2,
      error = error # assumed error rate
    )
  # fit model
  out <- sampling(compiledModel1, data = dataList, cores = 4, seed = 2113)
  return(out)
}

# fit stan model with predictor
fitModel2 <- function(d, compiledModel2, error = 0) {
  # list for stan
  dataList <-
    list(
      N = nrow(d),
      x = d$x,
      pun1_1 = d$pun1_1,
      pun1_2 = d$pun1_2,
      pun2_1 = d$pun2_1,
      pun2_2 = d$pun2_2,
      pun3_1 = d$pun3_1,
      pun3_2 = d$pun3_2,
      pun4_1 = d$pun4_1,
      pun4_2 = d$pun4_2,
      pun5_1 = d$pun5_1,
      pun5_2 = d$pun5_2,
      pun6_1 = d$pun6_1,
      pun6_2 = d$pun6_2,
      error = error # assumed error rate
    )
  # fit model
  out <- sampling(compiledModel2, data = dataList, cores = 4, seed = 2113)
  return(out)
}

# plot simulation results
plotSimResults1 <- function(simAlphas, simPost) {
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish")
  # get simulated probabilities (intercepts)
  simProbs <- softmax(simAlphas)
  # calculate probabilities
  P <- simPost$alpha
  for (i in 1:nrow(P)) P[i,] <- softmax(P[i,])
  # plot
  out <- 
    tibble(
      p = c(P[,1], P[,2], P[,3], P[,4], P[,5], 
            P[,6], P[,7], P[,8], P[,9], P[,10]),
      strategy = rep(strategies, each = length(P[,1]))
    ) %>%
    mutate(strategy = factor(strategy, levels = strategies)) %>%
    ggplot(aes(x = p, y = fct_rev(strategy))) +
    geom_density_ridges(rel_min_height = 0.01) +
    geom_point(data = tibble(strategy = strategies, p = simProbs), colour = "blue") +
    labs(x = "Probability of using punishment strategy", y = NULL) +
    theme_classic()
  # save
  ggsave(out, filename = "figures/simulationResults1.pdf", height = 5, width = 6)
  return(out)
}

# plot simulated effect of x on outcome variable
plotSimResults2 <- function(dSim, simPost) {
  # x sequence
  x <- seq(min(dSim$x), max(dSim$x), length.out = 100)
  # post
  post <- tibble()
  # median and 95% CIs for plotting
  for (i in 1:length(x)) {
    # on logit scale
    p <- simPost$alpha + simPost$beta*x[i]
    # on probability scale
    for (j in 1:nrow(p)) p[j,] <- softmax(p[j,])
    # add to post
    post <- bind_rows(post, tibble(x = x[i], strategy = 1:10, med = apply(p, 2, median),
                                   lower = apply(p, 2, quantile, 0.025),
                                   upper = apply(p, 2, quantile, 0.975)))
  }
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish")
  post$strategy <- strategies[post$strategy]
  post$strategy <- factor(post$strategy, levels = strategies)
  # plot
  out <-
    ggplot(post, aes(y = med, x = x, ymin = lower, ymax = upper)) +
    geom_ribbon(fill = "grey") +
    geom_line() +
    facet_wrap(. ~ strategy) +
    labs(x = "Predictor", y = "Probability of using strategy") +
    theme_classic()
  # save plot
  ggsave(out, filename = "figures/simulationResults2.pdf", height = 6, width = 6)
  return(out)
}

# load pilot data
loadPilotData <- function(filePilotData) {
  # load data
  read_survey(filePilotData) %>%
    # create variables
    mutate(
      # numeric punishment vars
      pun1_1 = ifelse(str_starts(NoDI1_Take,    "Yes"), 1, 0),
      pun1_2 = ifelse(str_starts(NoDI1_Nothing, "Yes"), 1, 0),
      pun2_1 = ifelse(str_starts(NoDI2_Take,    "Yes"), 1, 0),
      pun2_2 = ifelse(str_starts(NoDI2_Nothing, "Yes"), 1, 0),
      pun3_1 = ifelse(str_starts(NoDI3_Take,    "Yes"), 1, 0),
      pun3_2 = ifelse(str_starts(NoDI3_Nothing, "Yes"), 1, 0),
      pun4_1 = ifelse(str_starts(NoDI4_Take,    "Yes"), 1, 0),
      pun4_2 = ifelse(str_starts(NoDI4_NoTake,  "Yes"), 1, 0),
      pun5_1 = ifelse(str_starts(DI_Take,       "Yes"), 1, 0),
      pun5_2 = ifelse(str_starts(DI_Nothing,    "Yes"), 1, 0),
      pun6_1 = ifelse(str_starts(`3PP_Take`,    "Yes"), 1, 0),
      pun6_2 = ifelse(str_starts(`3PP_Nothing`, "Yes"), 1, 0),
      # comprehension failures
      fail1 = !is.na(NoDI1_TryAgain) & NoDI1_TryAgain != "I would have £0.40, Player 2 would have £0.00",
      fail2 = !is.na(NoDI2_TryAgain) & NoDI2_TryAgain != "I would have £0.40, Player 2 would have £0.20",
      fail3 = !is.na(NoDI3_TryAgain) & NoDI3_TryAgain != "I would have £0.40, Player 2 would have £0.20",
      fail4 = !is.na(NODI4_TryAgain) & NODI4_TryAgain != "I would have £0.40, Player 2 would have £0.40",
      fail5 = !is.na(DI_TryAgain)    & DI_TryAgain    != "I would have £0.40, Player 2 would have £0.40",
      fail6 = !is.na(`3PP_TryAgain`) & `3PP_TryAgain` != "I would have £0.90, Player 1 would have £0.50, Player 2 would have £0.60"
    ) %>%
    # exclude data from comprehension failures
    transmute(
      pun1_1 = ifelse(fail1, -999, pun1_1),
      pun1_2 = ifelse(fail1, -999, pun1_2),
      pun2_1 = ifelse(fail2, -999, pun2_1),
      pun2_2 = ifelse(fail2, -999, pun2_2),
      pun3_1 = ifelse(fail3, -999, pun3_1),
      pun3_2 = ifelse(fail3, -999, pun3_2),
      pun4_1 = ifelse(fail4, -999, pun4_1),
      pun4_2 = ifelse(fail4, -999, pun4_2),
      pun5_1 = ifelse(fail5, -999, pun5_1),
      pun5_2 = ifelse(fail5, -999, pun5_2),
      pun6_1 = ifelse(fail6, -999, pun6_1),
      pun6_2 = ifelse(fail6, -999, pun6_2)
    )
}

# plot pilot results
plotPilotResults <- function(pilotPost) {
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish")
  # calculate posterior probabilities
  P <- pilotPost$alpha
  for (i in 1:nrow(P)) P[i,] <- softmax(P[i,])
  # plot
  out <- 
    tibble(
      p = c(P[,1], P[,2], P[,3], P[,4], P[,5], 
            P[,6], P[,7], P[,8], P[,9], P[,10]),
      strategy = rep(strategies, each = length(P[,1]))
    ) %>%
    mutate(strategy = factor(strategy, levels = strategies)) %>%
    ggplot(aes(x = p, y = fct_rev(strategy))) +
    stat_halfeye() +
    labs(x = "Probability of using punishment strategy", y = NULL) +
    theme_classic()
  # save
  ggsave(out, filename = "figures/pilotResults.pdf", height = 5, width = 6)
  return(out)
}