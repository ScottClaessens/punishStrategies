# custom functions

# plot model results
plotModel1 <- function(post, file) {
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Never punish")
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
    mutate(strategy = factor(strategy, 
                             levels = c("Deterrent", "Norm-enforcing", "Retributive", 
                                        "Avoid DI", "Egalitarian", "Seek AI",
                                        "Competitive", "Antisocial", 
                                        "Never punish", "Random choice"))) %>%
    ggplot(aes(x = p, y = fct_rev(strategy), colour = Country)) +
    #geom_density_ridges(rel_min_height = 0.02, colour = "white", scale = 1) +
    stat_pointinterval(position = position_dodge(width = 0.5)) +
    scale_x_continuous(name = "Probability of using punishment strategy",
                       limits = c(0, 0.55), breaks = seq(0, 0.5, by = 0.1)) +
    ylab(NULL) +
    theme_classic()
  # save
  ggsave(out, filename = file, height = 5, width = 7)
  return(out)
}

# plot effect of predictor on strategy usage
plotModel2 <- function(d, post, pred, xlab, xBreaks, file) {
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Never punish")
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
              strategy = factor(strategies, levels = c("Deterrent", "Norm-enforcing",
                                                       "Retributive", "Avoid DI", 
                                                       "Egalitarian", "Seek AI",
                                                       "Competitive", "Antisocial", 
                                                       "Never punish", "Random choice")),
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
plotModel4 <- function(d, post, pred, xlab, file) {
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
                  "Antisocial", "Random choice", "Never punish")
  postPred$strategy <- strategies[postPred$strategy]
  postPred$strategy <- factor(postPred$strategy, levels = c("Deterrent", "Norm-enforcing",
                                                            "Retributive", "Avoid DI", 
                                                            "Egalitarian", "Seek AI",
                                                            "Competitive", "Antisocial", 
                                                            "Never punish", "Random choice"))
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

# summary plot sliders 
plotAllSliders <- function(postList, file) {
  # vectors
  countries <- c("UK", "US")
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Never punish")
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
      )
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
    # don't include this slider in figure
    filter(Predictor != "Didn't want to pay") %>%
    mutate(
      # determine whether slider and strategy match, set alpha level for plot
      match = ifelse(Predictor == predictors[2] & Strategy == strategies[1], "", " No match"),
      match = ifelse(Predictor == predictors[2] & Strategy == strategies[4], "", match),
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
      Predictor = factor(Predictor, levels = predictors[-10]),
      Strategy  = factor(Strategy, levels = c("Deterrent", "Norm-enforcing",
                                              "Retributive", "Avoid DI", 
                                              "Egalitarian", "Seek AI",
                                              "Competitive", "Antisocial", 
                                              "Never punish", "Random choice"))
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
    theme(axis.text = element_text(size = 7))
  # save
  ggsave(out, filename = file, width = 7.5, height = 4)
  return(out)
}

# summary plot personality and svo angle
plotAllPers <- function(postList, file) {
  # vectors
  countries <- c("UK", "US")
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Never punish")
  predictors <- c("Agreeableness", "Conscientiousness", "Extraversion",
                  "Honesty-humility", "Neuroticism", "Openness to experience",
                  "Social Value Orientation")
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
      Strategy  = factor(Strategy, levels = c("Deterrent", "Norm-enforcing",
                                              "Retributive", "Avoid DI", 
                                              "Egalitarian", "Seek AI",
                                              "Competitive", "Antisocial", 
                                              "Never punish", "Random choice"))
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
    theme(axis.text = element_text(size = 7))
  # save
  ggsave(out, filename = file, width = 7.5, height = 3.5)
  return(out)
}

# summary plot politics and religion
plotAllPolRel <- function(postList, file) {
  # vectors
  countries <- c("UK", "US")
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Never punish")
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
      Strategy  = factor(Strategy, levels = c("Deterrent", "Norm-enforcing",
                                              "Retributive", "Avoid DI", 
                                              "Egalitarian", "Seek AI",
                                              "Competitive", "Antisocial", 
                                              "Never punish", "Random choice"))
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
    theme(axis.text = element_text(size = 7))
  # save
  ggsave(out, filename = file, width = 7.5, height = 3.5)
  return(out)
}

# summary plot demographics
plotAllDems <- function(postList, file) {
  # vectors
  countries <- c("UK", "US")
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Never punish")
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
      Strategy  = factor(Strategy, levels = c("Deterrent", "Norm-enforcing",
                                              "Retributive", "Avoid DI", 
                                              "Egalitarian", "Seek AI",
                                              "Competitive", "Antisocial", 
                                              "Never punish", "Random choice"))
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
    theme(axis.text = element_text(size = 7))
  # save
  ggsave(out, filename = file, width = 7.5, height = 3)
  return(out)
}

# create trace plot
plotTraceMCMC <- function(m1.2) {
  # plot
  out <-
    tidy_draws(m1.2, alpha) %>%
    pivot_longer(cols = starts_with("alpha"),
                 names_to = "parameter",
                 values_to = "value") %>%
    mutate(.chain = factor(.chain)) %>%
    ggplot(aes(x = .iteration, y = value, colour = .chain)) +
    geom_line(alpha = 0.3) +
    facet_wrap(~ factor(parameter, levels = unique(parameter)), 
               scales = "free", ncol = 4) +
    labs(x = "Iteration", y = NULL) +
    theme_classic() +
    theme(strip.background = element_blank(),
          legend.position = "none")
  # save plot
  ggsave(out, filename = "figures/modelResults/trace.pdf", width = 7, height = 6)
  return(out)
}