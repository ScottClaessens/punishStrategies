# custom functions

# plot model results
plotModel1 <- function(post) {
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "No punish")
  # calculate probabilities
  P <- post$alpha
  for (i in 1:nrow(P)) # iterations
    for (j in 1:2)     # countries
      P[i,j,] <- softmax(P[i,j,])
  # plot
  out <- 
    tibble(
      p = c(P[,1,1], P[,1,2], P[,1,3], P[,1,4], P[,1,5], 
            P[,1,6], P[,1,7], P[,1,8], P[,1,9], P[,1,10],
            P[,2,1], P[,2,2], P[,2,3], P[,2,4], P[,2,5], 
            P[,2,6], P[,2,7], P[,2,8], P[,2,9], P[,2,10]),
      strategy = rep(rep(strategies, each = length(P[,1,1])), times = 2),
      Country = rep(c("United Kingdom", "United States"), each = length(P[,1,1]) * 10)
    ) %>%
    mutate(strategy = factor(strategy, levels = strategies)) %>%
    ggplot(aes(x = p, y = fct_rev(strategy), colour = Country)) +
    #geom_density_ridges(rel_min_height = 0.02, colour = "white", scale = 1) +
    stat_pointinterval(position = position_dodge(width = 0.5)) +
    scale_x_continuous(name = "Probability of using punishment strategy",
                       limits = c(0, 0.55), breaks = seq(0, 0.5, by = 0.1)) +
    ylab(NULL) +
    theme_classic()
  # save
  ggsave(out, filename = "figures/modelResults/model1.pdf", height = 5, width = 7)
  return(out)
}

# plot effect of predictor on strategy usage
plotModelPred <- function(d, post, pred, xlab, xBreaks, file) {
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish")
  # mean, sd, min, and max for predictor
  predMean <- d %>% pull(pred) %>% mean(na.rm = TRUE)
  predSD   <- d %>% pull(pred) %>% sd(na.rm = TRUE)
  predMin  <- d %>% pull(pred) %>% min(na.rm = TRUE)
  predMax  <- d %>% pull(pred) %>% max(na.rm = TRUE)
  # predictor sequence
  predSeq <- seq(
    d %>% pull(pred) %>% scale() %>% as.numeric() %>% min(na.rm = TRUE), 
    d %>% pull(pred) %>% scale() %>% as.numeric() %>% max(na.rm = TRUE), 
    length.out = 100
  )
  # get posterior predictions
  postPred <- tibble()
  # median and 95% CIs for plotting
  for (i in 1:length(predSeq)) {
      # on logit scale
      p <- post$alpha + post$beta*predSeq[i]
      # on probability scale
      for (j in 1:nrow(p))
        for (c in 1:2)
          p[j,c,] <- softmax(p[j,c,])
      # add to post
      for (c in 1:2) {
        postPred <- 
          bind_rows(
            postPred, 
            tibble(
              predictor = predSeq[i], 
              strategy = factor(strategies, levels = strategies),
              Country = ifelse(c == 1, "United Kingdom", "United States"),
              med = apply(p[,c,], 2, median),
              lower = apply(p[,c,], 2, quantile, 0.025),
              upper = apply(p[,c,], 2, quantile, 0.975)
            )
          )
      }
  }
  # limits
  limits <- c(
    ifelse(xBreaks[1] <= predMin, xBreaks[1], predMin),
    ifelse(tail(xBreaks, n = 1) >= predMax, tail(xBreaks, n = 1), predMax)
  )
  # plot
  out <-
    ggplot(data = postPred) +
    geom_line(aes(y = med, x = predictor, colour = Country)) +
    geom_ribbon(aes(x = predictor, ymin = lower, ymax = upper, fill = Country), alpha = 0.5) +
    facet_wrap(. ~ strategy, nrow = 2) +
    scale_y_continuous(name = "Probability of using strategy", limits = c(0, 1)) +
    scale_x_continuous(name = xlab, labels = function(x) round((x * predSD) + predMean, 0),
                       breaks = (xBreaks - predMean) / predSD,
                       limits = (limits - predMean) / predSD) +
    theme_classic()
  # save plot
  ggsave(out, filename = file, height = 4, width = 7.5)
  return(out)
}

# plot effect of categorical predictor on strategy usage
plotModelPredCat <- function(d, post, pred, xlab, file) {
  # posterior predictions
  postPred <- tibble()
  # categories
  categories <- d %>% drop_na(all_of(pred)) %>% pull(all_of(pred)) %>% as.factor() %>% levels()
  # median and 95% CIs for plotting
  for (i in 1:length(categories)) {
    for (c in 1:2) {
      # on logit scale
      p <- post$alpha[,c,i,]
      # on probability scale
      for (j in 1:nrow(p)) p[j,] <- softmax(p[j,])
      # add to post
      postPred <- 
        bind_rows(
          postPred, 
          tibble(
            predictor = categories[i],
            Country = ifelse(c == 1, "United Kingdom", "United States"),
            strategy = 1:10, 
            med = apply(p, 2, median),
            lower = apply(p, 2, quantile, 0.025),
            upper = apply(p, 2, quantile, 0.975)
          )
        )
    }
  }
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish")
  postPred$strategy <- strategies[postPred$strategy]
  postPred$strategy <- factor(postPred$strategy, levels = strategies)
  # reorder predictor for plot
  if (pred == "Gender")    postPred$predictor <- factor(postPred$predictor, levels = c("Male", "Female"))
  if (pred == "Ethnicity") postPred$predictor <- factor(postPred$predictor, levels = c("White", "Asian", "Black", "Mixed", "Other"))
  if (pred == "Student")   postPred$predictor <- factor(postPred$predictor, levels = c("Yes", "No"))
  if (pred == "Education") {
    postPred$predictor <- ifelse(postPred$predictor == "Diploma / other professional certificate", "Diploma", postPred$predictor)
    postPred$predictor <- factor(postPred$predictor, levels = c("High School", "Diploma", "Completed university", "Masters degree", "PhD or equivalent"))
  }
  # plot
  out <-
    ggplot(data = postPred, aes(x = predictor, y = med, ymin = lower,
                                ymax = upper, colour = Country)) +
    geom_pointrange(size = 0.3, position = position_dodge(width = 0.5)) +
    facet_wrap(. ~ strategy, nrow = 2) +
    scale_y_continuous(name = "Probability of using strategy", limits = c(0, 1)) +
    scale_x_discrete(name = xlab) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  # save plot
  ggsave(out, filename = file, height = 4, width = 7.5)
  return(out)
}

# plot model results with third-party strategy
plotModel4 <- function(post) {
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "No punish", "Third-party")
  # calculate probabilities
  P <- post$alpha
  for (i in 1:nrow(P)) # iterations
    for (j in 1:2)     # countries
      P[i,j,] <- softmax(P[i,j,])
  # plot
  out <- 
    tibble(
      p = c(P[,1,1], P[,1,2], P[,1,3], P[,1,4], P[,1,5], 
            P[,1,6], P[,1,7], P[,1,8], P[,1,9], P[,1,10], P[,1,11],
            P[,2,1], P[,2,2], P[,2,3], P[,2,4], P[,2,5], 
            P[,2,6], P[,2,7], P[,2,8], P[,2,9], P[,2,10], P[,2,11]),
      strategy = rep(rep(strategies, each = length(P[,1,1])), times = 2),
      Country = rep(c("United Kingdom", "United States"), each = length(P[,1,1]) * 11)
    ) %>%
    mutate(strategy = factor(strategy, levels = c(strategies[1:3], strategies[11], strategies[4:10]))) %>%
    ggplot(aes(x = p, y = fct_rev(strategy), colour = Country)) +
    #geom_density_ridges(rel_min_height = 0.02, colour = "white", scale = 1) +
    stat_pointinterval(position = position_dodge(width = 0.5)) +
    scale_x_continuous(name = "Probability of using punishment strategy",
                       limits = c(0, 0.55), breaks = seq(0, 0.5, by = 0.1)) +
    ylab(NULL) +
    theme_classic()
  # save
  ggsave(out, filename = "figures/modelResults/model4.pdf", height = 5, width = 7)
  return(out)
}


