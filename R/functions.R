# custom functions

# simulate data
simulateData <- function(n = 300, seed = 2113, errorRate = 0, probs) {
  # set simulation seed
  set.seed(seed)
  # n = number of participants
  # assume that each participant follows a different punishment strategy
  # 1. Competitive
  # 2. Avoid DI (self-referential)
  # 3. Egalitarian
  # 4. AI-seeking
  # 5. Retributive (self-referential)
  # 6. Norm-enforcer
  # 7. Exclusively antisocial punishment
  # 8. Random choice
  # 9. Never punish
  strategies <- sample(1:9, size = n, replace = TRUE, prob = probs)
  # each participant plays a series of games developed to tease apart strategies
  # we measure punishment behaviours in these games
  # 0 = no punish, 1 = punish
  # create function to simulate punishment behaviour based on strategy and error rate
  simulateBehaviour <- function(punStrats = c()) {
    out <- c()
    for (i in 1:n) {
      if (strategies[i] %in% punStrats) {
        out[i] <- rbinom(1, 1, prob = 1 - errorRate) # punish (with error)
      } else if (strategies[i] == 8) {
        out[i] <- rbinom(1, 1, prob = 0.5)           # random choice
      } else if (!(strategies[i] %in% punStrats)) {
        out[i] <- rbinom(1, 1, prob = 0 + errorRate) # don't punish (with error)
      }
    }
    return(out)
  }
  # game 1 - taking game (v1) with no disvantageous inequity
  pun1_1 <- simulateBehaviour(punStrats = c(1, 5, 6))
  pun1_2 <- simulateBehaviour(punStrats = c(1, 7))
  # game 2 - taking game (v2) with no disadvantageous inequity
  pun2_1 <- simulateBehaviour(punStrats = c(1, 4, 5, 6))
  pun2_2 <- simulateBehaviour(punStrats = c(1, 7))
  # game 3 - taking game (v3) with disadvantageous inequity
  pun3_1 <- simulateBehaviour(punStrats = c(1, 2, 3, 4, 5, 6))
  pun3_2 <- simulateBehaviour(punStrats = c(1, 7))
  # game 4 - third party game with disadvantageous inequity
  pun4_1 <- simulateBehaviour(punStrats = c(1, 3, 6))
  pun4_2 <- simulateBehaviour(punStrats = c(1, 7))
  # put together final dataset
  out <- tibble(id = 1:n, strategies, pun1_1, pun1_2, pun2_1, 
                pun2_2, pun3_1, pun3_2, pun4_1, pun4_2)
  return(out)
}

# fit stan model to simulated data
fitModel <- function(dSim, compiledModel) {
  # list for stan
  dataList <-
    list(
      N = nrow(dSim),
      id = dSim$id,
      pun1_1 = dSim$pun1_1,
      pun1_2 = dSim$pun1_2,
      pun2_1 = dSim$pun2_1,
      pun2_2 = dSim$pun2_2,
      pun3_1 = dSim$pun3_1,
      pun3_2 = dSim$pun3_2,
      pun4_1 = dSim$pun4_1,
      pun4_2 = dSim$pun4_2,
      error = 0.05 # assumed error rate
    )
  # fit model
  out <- sampling(compiledModel, data = dataList, cores = 4, seed = 2113)
  return(out)
}

# plot simulation results
plotSimResults <- function(simProbs, simPost) {
  # strategy vector
  strategies <- c("Competitive", "Avoid-DI", "Egalitarian", "AI-seeking",
                  "Retributive", "Norm-enforcer", "Antisocial", "Random", 
                  "Never punish")
  # calculate probabilities
  P <- simPost$alpha
  for (i in 1:nrow(P)) P[i,] <- softmax(P[i,])
  # plot
  out <- 
    tibble(
      p = c(P[,1], P[,2], P[,3], 
            P[,4], P[,5], P[,6],
            P[,7], P[,8], P[,9]),
      strategy = rep(strategies, each = length(P[,1]))
    ) %>%
    mutate(strategy = factor(strategy, levels = strategies)) %>%
    ggplot(aes(x = p, y = fct_rev(strategy))) +
    geom_density_ridges(rel_min_height = 0.01) +
    geom_point(data = tibble(strategy = strategies, p = simProbs), colour = "blue") +
    labs(x = "Probability of using punishment strategy", y = NULL) +
    theme_classic()
  # save
  ggsave(out, filename = "figures/simulationResults.pdf", height = 5, width = 6)
  return(out)
}
