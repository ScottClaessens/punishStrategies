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
                pun5_1, pun5_2, pun6_1, pun6_2,
                # empty strings for raw behaviours
                # (necessary to avoid missing data deletion in model fitting function)
                NoDI1_Take    = " ", 
                NoDI1_Nothing = " ",
                NoDI2_Take    = " ",
                NoDI2_Nothing = " ",
                NoDI3_Take    = " ",
                NoDI3_Nothing = " ",
                NoDI4_Take    = " ",
                NoDI4_Nothing = " ",
                DI_Take       = " ",
                DI_Nothing    = " ",
                `3PP_Take`    = " ",
                `3PP_Nothing` = " ",
                # country ID necessary for stan...
                Country = "United Kingdom")
  return(out)
}

# plot simulation results
plotSimResults1 <- function(simAlphas, simPost) {
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Never punish")
  # get simulated probabilities (intercepts)
  simProbs <- softmax(simAlphas)
  # calculate probabilities
  P <- simPost$alpha
  for (i in 1:nrow(P)) P[i,1,] <- softmax(P[i,1,])
  # plot
  out <- 
    tibble(
      p = c(P[,1,1], P[,1,2], P[,1,3], P[,1,4], P[,1,5], 
            P[,1,6], P[,1,7], P[,1,8], P[,1,9], P[,1,10]),
      strategy = rep(strategies, each = length(P[,1,1]))
    ) %>%
    mutate(strategy = factor(strategy, levels = c("Deterrent", "Norm-enforcing", "Retributive", 
                                                  "Avoid DI", "Egalitarian", "Seek AI",
                                                  "Competitive", "Antisocial", 
                                                  "Never punish", "Random choice"))) %>%
    ggplot(aes(x = p, y = fct_rev(strategy))) +
    geom_density_ridges(rel_min_height = 0.01) +
    geom_point(data = tibble(strategy = strategies, p = simProbs), colour = "blue") +
    labs(x = "Probability of using punishment strategy", y = NULL) +
    theme_classic()
  # save
  ggsave(out, filename = "figures/simulation/simulationResults1.pdf", height = 5, width = 6)
  return(out)
}

# plot simulated effect of x on outcome variable
plotSimResults2 <- function(dSim, simPost) {
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Never punish")
  # x sequence
  x <- seq(min(dSim$x), max(dSim$x), length.out = 100)
  # post
  post <- tibble()
  # median and 95% CIs for plotting
  for (i in 1:length(x)) {
    # on logit scale
    p <- simPost$alpha[,1,] + simPost$beta[,1,]*x[i]
    # on probability scale
    for (j in 1:nrow(p)) p[j,] <- softmax(p[j,])
    # add to post
    post <- 
      bind_rows(
        post, 
        tibble(
          x = x[i],
          strategy = factor(strategies, levels = c("Deterrent", "Norm-enforcing", "Retributive", 
                                                   "Avoid DI", "Egalitarian", "Seek AI",
                                                   "Competitive", "Antisocial", 
                                                   "Never punish", "Random choice")),
          med = apply(p, 2, median),
          lower = apply(p, 2, quantile, 0.025),
          upper = apply(p, 2, quantile, 0.975)
        )
      )
  }
  # plot
  out <-
    ggplot(post, aes(y = med, x = x, ymin = lower, ymax = upper)) +
    geom_ribbon(fill = "grey") +
    geom_line() +
    facet_wrap(. ~ strategy) +
    labs(x = "Predictor", y = "Probability of using strategy") +
    theme_classic()
  # save plot
  ggsave(out, filename = "figures/simulation/simulationResults2.pdf", height = 6, width = 6)
  return(out)
}
