# custom functions

# simulate data
simulateData <- function(n = 300, seed = 2113, errorRate = 0, probs) {
  # set simulation seed
  set.seed(seed)
  # n = number of participants
  # assume that each participant follows a different punishment strategy
  # 1. Avoid DI (disadvantageous inequity to self)
  # 2. Competitive
  # 3. Egalitarian
  # 4. Retributive
  # 5. Never punish
  strategies <- sample(1:5, size = n, replace = TRUE, prob = probs)
  # each participant plays a series of games developed to tease apart strategies
  # 0 = no punish, 1 = punish
  # cheat1 - cheating game with no disvantageous inequity
  cheat1 <- c()
  for (i in 1:n) {
    if (strategies[i] %in% c(1, 3, 5)) {
      cheat1[i] <- rbinom(1, 1, prob = 0 + errorRate) # 0
    } else if (strategies[i] %in% c(2, 4)) {
      cheat1[i] <- rbinom(1, 1, prob = 1 - errorRate) # 1
    }
  }
  # cheat2 - cheating game with disadvantageous inequity
  cheat2 <- c()
  for (i in 1:n) {
    if (strategies[i] == 5) {
      cheat2[i] <- rbinom(1, 1, prob = 0 + errorRate) # 0
    } else if (strategies[i] %in% 1:4) {
      cheat2[i] <- rbinom(1, 1, prob = 1 - errorRate) # 1
    }
  }
  # nocheat1 - game without cheating or disadvantageous inequity
  nocheat1 <- c()
  for (i in 1:n) {
    if (strategies[i] %in% c(1, 3, 4, 5)) {
      nocheat1[i] <- rbinom(1, 1, prob = 0 + errorRate) # 0
    } else if (strategies[i] == 2) {
      nocheat1[i] <- rbinom(1, 1, prob = 1 - errorRate) # 1
    }
  }
  # nocheat2 - game without cheating but disadvantageous inequity
  nocheat2 <- c()
  for (i in 1:n) {
    if (strategies[i] %in% 1:3) {
      nocheat2[i] <- rbinom(1, 1, prob = 1 - errorRate) # 1
    } else if (strategies[i] %in% 4:5) {
      nocheat2[i] <- rbinom(1, 1, prob = 0 + errorRate) # 0
    }
  }
  # tppcheat - third party game with disadvantageous inequity
  tppcheat <- c()
  for (i in 1:n) {
    if (strategies[i] %in% c(1, 5)) {
      tppcheat[i] <- rbinom(1, 1, prob = 0 + errorRate) # 0
    } else if (strategies[i] %in% 2:4) {
      tppcheat[i] <- rbinom(1, 1, prob = 1 - errorRate) # 1
    }
  }
  # put together final dataset
  out <- tibble(id = 1:n, strategies, cheat1, cheat2, 
                nocheat1, nocheat2, tppcheat)
  return(out)
}

# fit stan model
fitModel <- function(dSim, compiledModel) {
  # list for stan
  dataList <-
    list(
      N = nrow(dSim),
      id = dSim$id,
      cheat1 = dSim$cheat1,
      cheat2 = dSim$cheat2,
      nocheat1 = dSim$nocheat1, 
      nocheat2 = dSim$nocheat2,
      tppcheat = dSim$tppcheat,
      error = 0.05 # assumed error rate
    )
  # fit model
  out <- sampling(compiledModel, data = dataList, cores = 4, seed = 2113, init = "0")
  return(out)
}

# plot simulation results
plotSimResults <- function(simProbs, simPost) {
  # strategy vector
  strategies <- c("Avoid DI", "Competitive", "Egalitarian", "Retributive", "Never punish")
  # plot
  out <- 
    tibble(
      p = c(simPost$p[,1], simPost$p[,2], simPost$p[,3], simPost$p[,4], simPost$p[,5]),
      strategy = rep(strategies, each = length(simPost$p[,1]))
    ) %>%
    mutate(strategy = factor(strategy, levels = strategies)) %>%
    ggplot(aes(x = p, y = fct_rev(strategy))) +
    geom_density_ridges(rel_min_height = 0.01) +
    geom_point(data = tibble(strategy = strategies, p = simProbs), colour = "blue") +
    labs(x = "Probability of using punishment strategy", y = NULL) +
    theme_classic()
  # save
  ggsave(out, filename = "figures/simulationResults.pdf", height = 3.5, width = 6)
  return(out)
}