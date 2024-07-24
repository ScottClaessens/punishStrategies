# custom functions

# fit power model and return coefficient on x
fitPowerModel <- function(dPower, compiledModel2, strategy) {
  # fit model
  m <- 
    fitModel2(
      dPower,
      compiledModel2,
      predictor = "x",
      iter = 1000,
      warmup = 500,
      chains = 1,
      cores = 1
      )
  # extract posterior slope of x for particular strategy
  post <- rstan::extract(m)$beta[,strategy]
  # return posterior summary
  out <- 
    tibble(
      Estimate = median(post),
      `Q2.5` = quantile(post, 0.025),
      `Q97.5` = quantile(post, 0.975)
    )
  return(out)
}

# plot power analysis results
plotPowerAnalysis <- function(power) {
  # strategy vectors
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish")
  # plot
  out <-
    tibble(
      Strategy = factor(strategies, levels = strategies),
      Power = as.numeric(power)
    ) %>%
    ggplot(aes(x = Strategy, y = Power)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0.8, linetype = "dashed") +
    lims(y = 0:1) +
    labs(subtitle = "Power to detect medium effect size (log odds slope = 1.00)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  # save
  ggsave(out, filename = "figures/powerAnalysis/powerAnalysis.pdf", width = 5, height = 4)
  return(out)
}