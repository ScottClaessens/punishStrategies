# custom functions

# plot model results
plotModel1.1 <- function(post) {
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
  ggsave(out, filename = "figures/modelResults/noTPP/model1.pdf", height = 5, width = 7)
  return(out)
}

# plot effect of predictor on strategy usage
plotModel2.1 <- function(d, post, pred, xlab, xBreaks, file) {
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
plotModel4.1 <- function(d, post, pred, xlab, file) {
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
plotModel1.2 <- function(post) {
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
  ggsave(out, filename = "figures/modelResults/withTPP/model1.pdf", height = 5, width = 7)
  return(out)
}

# plot effect of predictor on strategy usage
plotModel2.2 <- function(d, post, pred, xlab, xBreaks, file) {
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish",
                  "Third-party")
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
            strategy = factor(strategies, levels = c(strategies[1:3], strategies[11], strategies[4:10])),
            Country = ifelse(c == 1, "UK", "US"),
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
    theme_classic() +
    theme(legend.position = c(0.95,0.2))
  # save plot
  ggsave(out, filename = file, height = 4, width = 7.5)
  return(out)
}

# plot effect of categorical predictor on strategy usage
plotModel4.2 <- function(d, post, pred, xlab, file) {
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
            Country = ifelse(c == 1, "UK", "US"),
            strategy = 1:11, 
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
                  "Antisocial", "Random choice", "Anti-punish", "Third-party")
  postPred$strategy <- strategies[postPred$strategy]
  postPred$strategy <- factor(postPred$strategy, 
                              levels = c(strategies[1:3], strategies[11], strategies[4:10]))
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
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7)) +
    theme(legend.position = c(0.95, 0.2))
  # save plot
  ggsave(out, filename = file, height = 4, width = 7.5)
  return(out)
}

# summary plot sliders (no tpp)
plotAllSlidersNoTPP <- function(postList) {
  # vectors
  countries <- c("UK", "US")
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish")
  predictors <- 
    paste0(
      c(
        "Punish harm others",
        "Seeking higher bonus",
        "Avoiding lower bonus",
        "All same bonus",
        "Stop cheating",
        "Show disapproval",
        "Random choice",
        "Punish if did not harm",
        "Didn't want to punish",
        "Didn't want to pay",
        "Punish harm me"
      ),
      " - Slider ",
      formatC(1:11, width = 2, format = "d", flag = "0")
    )
  # collect slopes
  slopes <- tibble()
  for (c in 1:2) {
    for (s in 1:10) {
      for (i in 1:11) {
        post <- postList[[i]]$beta[,c,s]
        slopes <-
          bind_rows(
            slopes,
            tibble(
              Country = countries[c],
              Strategy = strategies[s],
              Predictor = predictors[i],
              med = median(post),
              low = quantile(post, 0.025),
              upp = quantile(post, 0.975)
            )
          )
      }
    }
  }
  # plot
  out <-
    slopes %>%
    mutate(
      # determine whether slider and strategy match, set alpha level for plot
      match = ifelse(Predictor == predictors[2] & Strategy == strategies[4], "", " No match"),
      match = ifelse(Predictor == predictors[3] & Strategy == strategies[2], "", match),
      match = ifelse(Predictor == predictors[4] & Strategy == strategies[3], "", match),
      match = ifelse(Predictor == predictors[5] & Strategy == strategies[6], "", match),
      match = ifelse(Predictor == predictors[6] & Strategy == strategies[7], "", match),
      match = ifelse(Predictor == predictors[7] & Strategy == strategies[9], "", match),
      match = ifelse(Predictor == predictors[8] & Strategy == strategies[8], "", match),
      match = ifelse(Predictor == predictors[9] & Strategy == strategies[10], "", match),
      match = ifelse(Predictor == predictors[11] & Strategy == strategies[5], "", match),
      # combine country and match for plotting
      Country = paste0(Country, match),
      # as factors
      Predictor = factor(Predictor, levels = predictors),
      Strategy  = factor(Strategy, levels = strategies)
    ) %>%
    ggplot(aes(x = med, xmin = low, xmax = upp,
               y = fct_rev(Predictor), colour = Country)) +
    geom_vline(xintercept = 0, linewidth = 0.1) +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.55)) +
    facet_wrap(. ~ Strategy, nrow = 2) +
    labs(x = "Posterior slope", y = NULL) +
    scale_color_manual(
      values = c(
        "UK"          = "#F8766D",
        "UK No match" = "#FCD5D3",
        "US"          = "#00BFC4",
        "US No match" = "#B2EBED"
        ),
      breaks = c("UK", "US")
      ) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/modelResults/noTPP/allSliders.pdf",
         width = 7.5, height = 4)
  return(out)
}

# summary plot sliders (with tpp)
plotAllSlidersWithTPP <- function(postList) {
  # vectors
  countries <- c("UK", "US")
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish",
                  "Third-party")
  predictors <- 
    paste0(
      c(
        "Punish harm others",
        "Seeking higher bonus",
        "Avoiding lower bonus",
        "All same bonus",
        "Stop cheating",
        "Show disapproval",
        "Random choice",
        "Punish if did not harm",
        "Didn't want to punish",
        "Didn't want to pay",
        "Punish harm me"
      ),
      " - Slider ",
      formatC(1:11, width = 2, format = "d", flag = "0")
    )
  # collect slopes
  slopes <- tibble()
  for (c in 1:2) {
    for (s in 1:11) {
      for (i in 1:11) {
        post <- postList[[i]]$beta[,c,s]
        slopes <-
          bind_rows(
            slopes,
            tibble(
              Country = countries[c],
              Strategy = strategies[s],
              Predictor = predictors[i],
              med = median(post),
              low = quantile(post, 0.025),
              upp = quantile(post, 0.975)
            )
          )
      }
    }
  }
  # plot
  out <-
    slopes %>%
    mutate(
      # determine whether slider and strategy match, set alpha level for plot
      match = ifelse(Predictor == predictors[2] & Strategy == strategies[4], "", " No match"),
      match = ifelse(Predictor == predictors[3] & Strategy == strategies[2], "", match),
      match = ifelse(Predictor == predictors[4] & Strategy == strategies[3], "", match),
      match = ifelse(Predictor == predictors[5] & Strategy == strategies[6], "", match),
      match = ifelse(Predictor == predictors[6] & Strategy == strategies[7], "", match),
      match = ifelse(Predictor == predictors[7] & Strategy == strategies[9], "", match),
      match = ifelse(Predictor == predictors[8] & Strategy == strategies[8], "", match),
      match = ifelse(Predictor == predictors[9] & Strategy == strategies[10], "", match),
      match = ifelse(Predictor == predictors[11] & Strategy == strategies[5], "", match),
      # combine country and match for plotting
      Country = paste0(Country, match),
      # as factors
      Predictor = factor(Predictor, levels = predictors),
      Strategy  = factor(Strategy, levels = strategies)
    ) %>%
    ggplot(aes(x = med, xmin = low, xmax = upp,
               y = fct_rev(Predictor), colour = Country)) +
    geom_vline(xintercept = 0, linewidth = 0.1) +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.55)) +
    facet_wrap(. ~ Strategy, nrow = 3) +
    labs(x = "Posterior slope", y = NULL) +
    scale_color_manual(
      values = c(
        "UK"          = "#F8766D",
        "UK No match" = "#FCD5D3",
        "US"          = "#00BFC4",
        "US No match" = "#B2EBED"
      ),
      breaks = c("UK", "US")
    ) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/modelResults/withTPP/allSliders.pdf",
         width = 7, height = 5)
  return(out)
}

# summary plot personality and svo angle (no tpp)
plotAllPersNoTPP <- function(postList) {
  # vectors
  countries <- c("UK", "US")
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish")
  predictors <- c("Agreeableness", "Conscientiousness", "Extraversion",
                  "Honesty-humility", "Neuroticism", "Openness to experience",
                  "SVO angle")
  # collect slopes
  slopes <- tibble()
  for (c in 1:2) {
    for (s in 1:10) {
      for (i in 1:7) {
        post <- postList[[i]]$beta[,c,s]
        slopes <-
          bind_rows(
            slopes,
            tibble(
              Country = countries[c],
              Strategy = strategies[s],
              Predictor = predictors[i],
              med = median(post),
              low = quantile(post, 0.025),
              upp = quantile(post, 0.975)
            )
          )
      }
    }
  }
  # plot
  out <-
    slopes %>%
    mutate(
      # as factors
      Predictor = factor(Predictor, levels = predictors),
      Strategy  = factor(Strategy, levels = strategies)
    ) %>%
    ggplot(aes(x = med, xmin = low, xmax = upp,
               y = fct_rev(Predictor), colour = Country)) +
    geom_vline(xintercept = 0, linewidth = 0.1) +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.55)) +
    facet_wrap(. ~ Strategy, nrow = 2) +
    labs(x = "Posterior slope", y = NULL) +
    scale_color_manual(
      values = c(
        "UK" = "#F8766D",
        "US" = "#00BFC4"
      ),
      breaks = c("UK", "US")
    ) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/modelResults/noTPP/allPers.pdf",
         width = 7.5, height = 3.5)
  return(out)
}

# summary plot personality and svo angle (with tpp)
plotAllPersWithTPP <- function(postList) {
  # vectors
  countries <- c("UK", "US")
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish", "Third-party")
  predictors <- c("Agreeableness", "Conscientiousness", "Extraversion",
                  "Honesty-humility", "Neuroticism", "Openness to experience",
                  "SVO angle")
  # collect slopes
  slopes <- tibble()
  for (c in 1:2) {
    for (s in 1:11) {
      for (i in 1:7) {
        post <- postList[[i]]$beta[,c,s]
        slopes <-
          bind_rows(
            slopes,
            tibble(
              Country = countries[c],
              Strategy = strategies[s],
              Predictor = predictors[i],
              med = median(post),
              low = quantile(post, 0.025),
              upp = quantile(post, 0.975)
            )
          )
      }
    }
  }
  # plot
  out <-
    slopes %>%
    mutate(
      # as factors
      Predictor = factor(Predictor, levels = predictors),
      Strategy  = factor(Strategy, levels = strategies)
    ) %>%
    ggplot(aes(x = med, xmin = low, xmax = upp,
               y = fct_rev(Predictor), colour = Country)) +
    geom_vline(xintercept = 0, linewidth = 0.1) +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.55)) +
    facet_wrap(. ~ Strategy, nrow = 3) +
    labs(x = "Posterior slope", y = NULL) +
    scale_color_manual(
      values = c(
        "UK" = "#F8766D",
        "US" = "#00BFC4"
      ),
      breaks = c("UK", "US")
    ) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/modelResults/withTPP/allPers.pdf",
         width = 7, height = 4.5)
  return(out)
}

# summary plot politics and religion (no tpp)
plotAllPolRelNoTPP <- function(postList) {
  # vectors
  countries <- c("UK", "US")
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish")
  predictors <- c("Left-right political ideology", "Right Wing Authoritarianism",
                  "Social Dominance Orientation", "Bring up a peg",
                  "Bring down a peg", "Religiosity", "God controls events")
  # collect slopes
  slopes <- tibble()
  for (c in 1:2) {
    for (s in 1:10) {
      for (i in 1:7) {
        # extract sdo/rwa
        if (i == 2) {
          post <- postList[[2]]$beta1[,c,s]
        } else if (i == 3) {
          post <- postList[[2]]$beta2[,c,s]
        } else {
          post <- postList[[ifelse(i >= 3, i - 1, i)]]$beta[,c,s]
        }
        slopes <-
          bind_rows(
            slopes,
            tibble(
              Country = countries[c],
              Strategy = strategies[s],
              Predictor = predictors[i],
              med = median(post),
              low = quantile(post, 0.025),
              upp = quantile(post, 0.975)
            )
          )
      }
    }
  }
  # plot
  out <-
    slopes %>%
    mutate(
      # as factors
      Predictor = factor(Predictor, levels = predictors),
      Strategy  = factor(Strategy, levels = strategies)
    ) %>%
    ggplot(aes(x = med, xmin = low, xmax = upp,
               y = fct_rev(Predictor), colour = Country)) +
    geom_vline(xintercept = 0, linewidth = 0.1) +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.55)) +
    facet_wrap(. ~ Strategy, nrow = 2) +
    labs(x = "Posterior slope", y = NULL) +
    scale_color_manual(
      values = c(
        "UK" = "#F8766D",
        "US" = "#00BFC4"
      ),
      breaks = c("UK", "US")
    ) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/modelResults/noTPP/allPolRel.pdf",
         width = 7.5, height = 3.5)
  return(out)
}

# summary plot politics and religion (with tpp)
plotAllPolRelWithTPP <- function(postList) {
  # vectors
  countries <- c("UK", "US")
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish",
                  "Third-party")
  predictors <- c("Left-right political ideology", "Right Wing Authoritarianism",
                  "Social Dominance Orientation", "Bring up a peg",
                  "Bring down a peg", "Religiosity", "God controls events")
  # collect slopes
  slopes <- tibble()
  for (c in 1:2) {
    for (s in 1:11) {
      for (i in 1:7) {
        # extract sdo/rwa
        if (i == 2) {
          post <- postList[[2]]$beta1[,c,s]
        } else if (i == 3) {
          post <- postList[[2]]$beta2[,c,s]
        } else {
          post <- postList[[ifelse(i >= 3, i - 1, i)]]$beta[,c,s]
        }
        slopes <-
          bind_rows(
            slopes,
            tibble(
              Country = countries[c],
              Strategy = strategies[s],
              Predictor = predictors[i],
              med = median(post),
              low = quantile(post, 0.025),
              upp = quantile(post, 0.975)
            )
          )
      }
    }
  }
  # plot
  out <-
    slopes %>%
    mutate(
      # as factors
      Predictor = factor(Predictor, levels = predictors),
      Strategy  = factor(Strategy, levels = strategies)
    ) %>%
    ggplot(aes(x = med, xmin = low, xmax = upp,
               y = fct_rev(Predictor), colour = Country)) +
    geom_vline(xintercept = 0, linewidth = 0.1) +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.55)) +
    facet_wrap(. ~ Strategy, nrow = 3) +
    labs(x = "Posterior slope", y = NULL) +
    scale_color_manual(
      values = c(
        "UK" = "#F8766D",
        "US" = "#00BFC4"
      ),
      breaks = c("UK", "US")
    ) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/modelResults/withTPP/allPolRel.pdf",
         width = 7.5, height = 4.5)
  return(out)
}

# summary plot demographics (no tpp)
plotAllDemsNoTPP <- function(postList) {
  # vectors
  countries <- c("UK", "US")
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish")
  predictors <- c("Age", "Socio-economic status", "Gender (male)", "Student (yes)")
  # collect slopes
  slopes <- tibble()
  for (c in 1:2) {
    for (s in 1:10) {
      for (i in 1:4) {
        if (i %in% 1:2) {
          post <- postList[[i]]$beta[,c,s]
        } else {
          post <- postList[[i]]$alpha[,c,2,s] - postList[[i]]$alpha[,c,1,s]
        }
        slopes <-
          bind_rows(
            slopes,
            tibble(
              Country = countries[c],
              Strategy = strategies[s],
              Predictor = predictors[i],
              med = median(post),
              low = quantile(post, 0.025),
              upp = quantile(post, 0.975)
            )
          )
      }
    }
  }
  # plot
  out <-
    slopes %>%
    mutate(
      # as factors
      Predictor = factor(Predictor, levels = predictors),
      Strategy  = factor(Strategy, levels = strategies)
    ) %>%
    ggplot(aes(x = med, xmin = low, xmax = upp,
               y = fct_rev(Predictor), colour = Country)) +
    geom_vline(xintercept = 0, linewidth = 0.1) +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.55)) +
    facet_wrap(. ~ Strategy, nrow = 2) +
    labs(x = "Posterior slope", y = NULL) +
    scale_color_manual(
      values = c(
        "UK" = "#F8766D",
        "US" = "#00BFC4"
      ),
      breaks = c("UK", "US")
    ) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/modelResults/noTPP/allDems.pdf",
         width = 7.5, height = 3)
  return(out)
}

# summary plot demographics (with tpp)
plotAllDemsWithTPP <- function(postList) {
  # vectors
  countries <- c("UK", "US")
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish", "Third-party")
  predictors <- c("Age", "Socio-economic status", "Gender (male)", "Student (yes)")
  # collect slopes
  slopes <- tibble()
  for (c in 1:2) {
    for (s in 1:11) {
      for (i in 1:4) {
        if (i %in% 1:2) {
          post <- postList[[i]]$beta[,c,s]
        } else {
          post <- postList[[i]]$alpha[,c,2,s] - postList[[i]]$alpha[,c,1,s]
        }
        slopes <-
          bind_rows(
            slopes,
            tibble(
              Country = countries[c],
              Strategy = strategies[s],
              Predictor = predictors[i],
              med = median(post),
              low = quantile(post, 0.025),
              upp = quantile(post, 0.975)
            )
          )
      }
    }
  }
  # plot
  out <-
    slopes %>%
    mutate(
      # as factors
      Predictor = factor(Predictor, levels = predictors),
      Strategy  = factor(Strategy, levels = strategies)
    ) %>%
    ggplot(aes(x = med, xmin = low, xmax = upp,
               y = fct_rev(Predictor), colour = Country)) +
    geom_vline(xintercept = 0, linewidth = 0.1) +
    geom_pointrange(size = 0.1, position = position_dodge(width = 0.55)) +
    facet_wrap(. ~ Strategy, nrow = 3) +
    labs(x = "Posterior slope", y = NULL) +
    scale_color_manual(
      values = c(
        "UK" = "#F8766D",
        "US" = "#00BFC4"
      ),
      breaks = c("UK", "US")
    ) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/modelResults/withTPP/allDems.pdf",
         width = 7.5, height = 4)
  return(out)
}
