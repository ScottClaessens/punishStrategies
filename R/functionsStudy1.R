# custom functions

# load study 1 data
loadData1 <- function(fileStudy1) {
  # load data
  read_csv(fileStudy1) %>%
    # create variables
    mutate(
      # standardised variables for prediction
      AgeStd = as.numeric(scale(Age)),
      SelfRate1Std = as.numeric(scale(SelfRate_1)),
      SelfRate2Std = as.numeric(scale(SelfRate_2)),
      SelfRate3Std = as.numeric(scale(SelfRate_3)),
      SelfRate4Std = as.numeric(scale(SelfRate_4)),
      SelfRate5Std = as.numeric(scale(SelfRate_5)),
      SelfRate6Std = as.numeric(scale(SelfRate_6)),
      SelfRate7Std = as.numeric(scale(SelfRate_7)),
      SelfRate8Std = as.numeric(scale(SelfRate_8)),
      SelfRate9Std = as.numeric(scale(SelfRate_9)),
      SelfRate10Std = as.numeric(scale(SelfRate_10)),
      SelfRate11Std = as.numeric(scale(SelfRate_11)),
      # numeric punishment vars
      pun1_1 = ifelse(str_starts(NoDI1_Take,    "Yes"), 1, 0),
      pun1_2 = ifelse(str_starts(NoDI1_Nothing, "Yes"), 1, 0),
      pun2_1 = ifelse(str_starts(NoDI2_Take,    "Yes"), 1, 0),
      pun2_2 = ifelse(str_starts(NoDI2_Nothing, "Yes"), 1, 0),
      pun3_1 = ifelse(str_starts(NoDI3_Take,    "Yes"), 1, 0),
      pun3_2 = ifelse(str_starts(NoDI3_Nothing, "Yes"), 1, 0),
      pun4_1 = ifelse(str_starts(NoDI4_Take,    "Yes"), 1, 0),
      pun4_2 = ifelse(str_starts(NoDI4_Nothing, "Yes"), 1, 0),
      pun5_1 = ifelse(str_starts(DI_Take,       "Yes"), 1, 0),
      pun5_2 = ifelse(str_starts(DI_Nothing,    "Yes"), 1, 0),
      pun6_1 = ifelse(str_starts(`3PP_Take`,    "Yes"), 1, 0),
      pun6_2 = ifelse(str_starts(`3PP_Nothing`, "Yes"), 1, 0),
      # comprehension failures
      fail1 = !is.na(NoDI1_TryAgain) & !str_detect(NoDI1_TryAgain, fixed("0.40")),
      fail2 = !is.na(NoDI2_TryAgain) & !str_detect(NoDI2_TryAgain, fixed("0.40")),
      fail3 = !is.na(NoDI3_TryAgain) & !str_detect(NoDI3_TryAgain, fixed("0.40")),
      fail4 = !is.na(NoDI4_TryAgain) & (str_detect(NoDI4_TryAgain, fixed("0.50")) | str_detect(NoDI4_TryAgain, fixed("0.20"))),
      fail5 = !is.na(DI_TryAgain)    & (str_detect(DI_TryAgain, fixed("0.50")) | str_detect(DI_TryAgain, fixed("0.70")) | str_detect(DI_TryAgain, fixed("0.80"))),
      fail6 = !is.na(`3PP_TryAgain`) & (str_detect(`3PP_TryAgain`, fixed("0.00")) | str_detect(`3PP_TryAgain`, fixed("0.70")) | str_detect(`3PP_TryAgain`, fixed("1.30"))),
      # raw strategy classification
      strategy = ifelse(pun1_1 == 1 & pun1_2 == 1 & pun2_1 == 1 & pun2_2 == 1 & 
                          pun3_1 == 1 & pun3_2 == 1 & pun4_1 == 0 & pun4_2 == 0 &
                          pun5_1 == 1 & pun5_2 == 1 & pun6_1 == 1 & pun6_2 == 1,
                        "Competitive", "N/A"),
      strategy = ifelse(pun1_1 == 0 & pun1_2 == 0 & pun2_1 == 0 & pun2_2 == 0 & 
                          pun3_1 == 0 & pun3_2 == 0 & pun4_1 == 0 & pun4_2 == 0 &
                          pun5_1 == 1 & pun5_2 == 0 & pun6_1 == 0 & pun6_2 == 0,
                        "Avoid disadvantageous inequity", strategy),
      strategy = ifelse(pun1_1 == 0 & pun1_2 == 0 & pun2_1 == 0 & pun2_2 == 0 & 
                          pun3_1 == 0 & pun3_2 == 0 & pun4_1 == 0 & pun4_2 == 0 &
                          pun5_1 == 1 & pun5_2 == 0 & pun6_1 == 1 & pun6_2 == 0,
                        "Egalitarian", strategy),
      strategy = ifelse(pun1_1 == 0 & pun1_2 == 0 & pun2_1 == 1 & pun2_2 == 0 & 
                          pun3_1 == 1 & pun3_2 == 0 & pun4_1 == 0 & pun4_2 == 0 &
                          pun5_1 == 0 & pun5_2 == 0 & pun6_1 == 0 & pun6_2 == 0,
                        "Seek advantageous inequity", strategy),
      strategy = ifelse(pun1_1 == 1 & pun1_2 == 0 & pun2_1 == 1 & pun2_2 == 0 & 
                          pun3_1 == 1 & pun3_2 == 0 & pun4_1 == 1 & pun4_2 == 0 &
                          pun5_1 == 1 & pun5_2 == 0 & pun6_1 == 0 & pun6_2 == 0,
                        "Retributive", strategy),
      strategy = ifelse(pun1_1 == 1 & pun1_2 == 0 & pun2_1 == 1 & pun2_2 == 0 & 
                          pun3_1 == 0 & pun3_2 == 0 & pun4_1 == 1 & pun4_2 == 0 &
                          pun5_1 == 1 & pun5_2 == 0 & pun6_1 == 0 & pun6_2 == 0,
                        "Deterrent", strategy),
      strategy = ifelse(pun1_1 == 1 & pun1_2 == 0 & pun2_1 == 1 & pun2_2 == 0 & 
                          pun3_1 == 0 & pun3_2 == 0 & pun4_1 == 1 & pun4_2 == 0 &
                          pun5_1 == 1 & pun5_2 == 0 & pun6_1 == 1 & pun6_2 == 0,
                        "Norm-enforcing", strategy),
      strategy = ifelse(pun1_1 == 0 & pun1_2 == 1 & pun2_1 == 0 & pun2_2 == 1 & 
                          pun3_1 == 0 & pun3_2 == 1 & pun4_1 == 0 & pun4_2 == 1 &
                          pun5_1 == 0 & pun5_2 == 1 & pun6_1 == 0 & pun6_2 == 1,
                        "Exclusively antisocial punishment", strategy),
      strategy = ifelse(pun1_1 == 0 & pun1_2 == 0 & pun2_1 == 0 & pun2_2 == 0 & 
                          pun3_1 == 0 & pun3_2 == 0 & pun4_1 == 0 & pun4_2 == 0 &
                          pun5_1 == 0 & pun5_2 == 0 & pun6_1 == 0 & pun6_2 == 0,
                        "Anti-punish", strategy),
      strategy = factor(strategy, levels = c("Competitive", "Avoid disadvantageous inequity",
                                             "Egalitarian", "Seek advantageous inequity",
                                             "Retributive", "Deterrent", "Norm-enforcing",
                                             "Exclusively antisocial punishment",
                                             "Anti-punish", "N/A"))
    )

}

# plot sample demographics
plotSampleStudy1 <- function(d1) {
  # age
  pA <- 
    ggplot(d1, aes(x = Age)) +
    geom_histogram(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic()
  # gender
  pB <-
    ggplot(d1, aes(x = Gender)) +
    geom_bar(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic()
  # ethnicity
  pC <-
    ggplot(d1, aes(x = Ethnicity)) +
    geom_bar(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic()
  # student
  pD <-
    ggplot(d1, aes(x = Student)) +
    geom_bar(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic()
  # employment
  pE <-
    d1 %>%
    mutate(
      Employment = ifelse(str_starts(Employment, fixed("Not")), "Not in paid work", Employment),
      Employment = ifelse(str_starts(Employment, fixed("Un")), "Unemployed", Employment),
      Employment = ifelse(str_starts(Employment, fixed("Due")), "Starting new job", Employment)
    ) %>%
    ggplot(aes(x = Employment)) +
    geom_bar(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 7))
  # put together and save
  out <- 
    plot_grid(
      plot_grid(pA, pC, nrow = 1), 
      plot_grid(pD, pB, pE, nrow = 1), 
      nrow = 2
    )
  ggsave(out, filename = "figures/study1/sample.pdf", width = 7, height = 5)
  return(out)
}

# plot wordcloud for open-ended question
plotWordcloud <- function(d1) {
  # from: https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
  # clean responses
  d1$Explain <- gsub("@\\S*", "", d1$Explain) 
  d1$Explain <- gsub("amp", "", d1$Explain) 
  d1$Explain <- gsub("[\r\n]", "", d1$Explain)
  d1$Explain <- gsub("[[:punct:]]", "", d1$Explain)
  # create document term matrix
  words <- 
    d1 %>%
    select(Explain) %>%
    unnest_tokens(word, Explain) %>%
    count(word, sort = TRUE) %>%
    # remove common words
    filter(!(word %in% c("to","i","the","my","as","was","not",
                         "of","and","if","it","in","be","a","me",
                         "from","for","that","would","did","didnt",
                         "so","they","make","with","had","wanted",
                         "want","or","much","but","have","on","no",
                         "by","do","at","am","is","2","010","its",
                         "there","your","dont","them","also","when",
                         "any","their","see","than","then","what",
                         "an","well","3")))
  # create wordcloud and save to file
  set.seed(2113)
  png(file = "figures/study1/wordcloud.png", width = 400, height = 400)
  wordcloud(
    words = words$word,
    freq = words$n,
    min.freq = 10,
    colors = brewer.pal(8, "Dark2")
  )
  dev.off()
}

# plot punishment rates
plotPunDecisions <- function(d1) {
  # games vector
  games <- c(
    "No Disadvantageous\nInequity 1",
    "No Disadvantageous\nInequity 2",
    "No Disadvantageous\nInequity 3 (Computer)",
    "No Disadvantageous\nInequity 4 (1:1 Fee Fine)",
    "Disadvantageous\nInequity",
    "Third-Party"
  )
  # plot
  out <-
    d1 %>%
    # from wide to long
    pivot_longer(
      cols = starts_with("pun"),
      names_to = "Decision",
      values_to = "Punishment"
    ) %>%
    # prepare for plot
    separate(Decision, into = c("Game", "Other")) %>%
    transmute(
      # game labels
      Game = ifelse(Game == "pun1", games[1], Game),
      Game = ifelse(Game == "pun2", games[2], Game),
      Game = ifelse(Game == "pun3", games[3], Game),
      Game = ifelse(Game == "pun4", games[4], Game),
      Game = ifelse(Game == "pun5", games[5], Game),
      Game = ifelse(Game == "pun6", games[6], Game),
      Game = factor(Game, levels = games),
      # other labels
      Other = factor(ifelse(Other == 1, "Take", "Do nothing"), levels = c("Take", "Do nothing")),
      # punishment
      Punishment = Punishment
    ) %>%
    # summarise
    group_by(Game, Other) %>%
    summarise(Punishment = mean(Punishment), .groups = "drop") %>%
    # plot
    ggplot(aes(x = Game, y = Punishment, fill = Other)) +
    geom_col(position = "dodge") +
    scale_y_continuous(name = "Proportion of punishment", limits = c(0, 1),
                       expand = c(0, 0)) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 50, hjust = 1, size = 9),
      axis.title.x = element_blank(),
      legend.title = element_blank()
    )
  # save
  ggsave(out, filename = "figures/study1/propPunish.pdf", width = 6, height = 5)
  return(out)
}

# plot slider ratings
plotSliderRatings1 <- function(d1) {
  # named vector for converting to plot labels
  labels <- c(
    "SelfRate_1"  = "I wanted to punish people who harmed others",
    "SelfRate_2"  = "I wanted to have a higher final bonus than others",
    "SelfRate_3"  = "I wanted to avoid having a lower final bonus than others",
    "SelfRate_4"  = "I wanted all players to have the same final bonus",
    "SelfRate_5"  = "I wanted to stop others from cheating",
    "SelfRate_6"  = "I wanted to show that I disapproved of others' actions",
    "SelfRate_7"  = "I made decisions at random",
    "SelfRate_8"  = "I wanted to punish people who DID NOT harm me or others",
    "SelfRate_9"  = "I didn't want to reduce anyone's bonus, no matter what they did",
    "SelfRate_10" = "I didn't want to PAY to reduce anyone's bonus but I would have done so if it were free",
    "SelfRate_11" = "I wanted to punish people if they harmed me but not if they harmed others"
  )
  # plot
  out <-
    d1 %>%
    # from wide to long
    pivot_longer(
      cols = starts_with("SelfRate_"),
      names_to = "SelfRateSlider",
      values_to = "SelfRate"
    ) %>%
    # edit slider labels
    mutate(SelfRateSlider = factor(labels[SelfRateSlider], levels = labels)) %>%
    # plot
    ggplot(aes(x = SelfRate, y = fct_rev(SelfRateSlider))) +
    geom_boxplot(outlier.shape = NA) +
    labs(x = "Rating", y = NULL) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/study1/sliders1.pdf", width = 7, height = 3)
  return(out)
}

# plot slider ratings
plotSliderRatings2 <- function(d1) {
  # named vector for converting to plot labels
  labels <- c(
    "SelfRate_1"  = "I wanted to punish people who harmed others",
    "SelfRate_2"  = "I wanted to have a higher final bonus than others",
    "SelfRate_3"  = "I wanted to avoid having a lower final bonus than others",
    "SelfRate_4"  = "I wanted all players to have the same final bonus",
    "SelfRate_5"  = "I wanted to stop others from cheating",
    "SelfRate_6"  = "I wanted to show that I disapproved of others' actions",
    "SelfRate_7"  = "I made decisions at random",
    "SelfRate_8"  = "I wanted to punish people who DID NOT harm me or others",
    "SelfRate_9"  = "I didn't want to reduce anyone's bonus, no matter what they did",
    "SelfRate_10" = "I didn't want to PAY to reduce anyone's bonus but I would have done so if it were free",
    "SelfRate_11" = "I wanted to punish people if they harmed me but not if they harmed others"
  )
  # plot
  out <-
    d1 %>%
    # create average
    mutate(avgSelfRate = rowMeans(select(d1, starts_with("SelfRate_")))) %>%
    # from wide to long
    pivot_longer(
      cols = starts_with("SelfRate_"),
      names_to = "SelfRateSlider",
      values_to = "SelfRate"
    ) %>%
    # edit slider labels and create deviations from mean
    mutate(
      SelfRateSlider = factor(labels[SelfRateSlider], levels = labels),
      SelfRate = SelfRate - avgSelfRate
      ) %>%
    # plot
    ggplot(aes(x = SelfRate, y = fct_rev(SelfRateSlider))) +
    geom_boxplot(outlier.shape = NA) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Deviation from average\nslider rating", y = NULL) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/study1/sliders2.pdf", width = 7, height = 3)
  return(out)
}

# table of comprehension rates
makeCompTable <- function(d1) {
  # games vector
  games <- c(
    "No Disadvantageous Inequity 1",
    "No Disadvantageous Inequity 2",
    "No Disadvantageous Inequity 3 (Computer)",
    "No Disadvantageous Inequity 4 (1:1 Fee Fine)",
    "Disadvantageous Inequity",
    "Third-Party"
  )
  # make table
  d1 %>%
    # from wide to long
    pivot_longer(
      cols = starts_with("fail"),
      names_to = "Game",
      values_to = "Comprehension"
    ) %>%
    # mutate game labels
    transmute(
      Game = ifelse(Game == "fail1", games[1], Game),
      Game = ifelse(Game == "fail2", games[2], Game),
      Game = ifelse(Game == "fail3", games[3], Game),
      Game = ifelse(Game == "fail4", games[4], Game),
      Game = ifelse(Game == "fail5", games[5], Game),
      Game = ifelse(Game == "fail6", games[6], Game),
      Game = factor(Game, levels = games),
      Comprehension = Comprehension
    ) %>%
    # summarise
    group_by(Game) %>%
    summarise(`Comprehension Rate` = round(mean(!Comprehension), 2))
}

# table of raw strategy counts
makeStrategyCountTable <- function(d1) {
  d1 %>%
    rename(Strategy = strategy) %>%
    group_by(Strategy) %>%
    summarise(N = n(), Proportion = round(n() / nrow(.), 3))
}

# table of behaviour patterns
makePatternsTable <- function(d1) {
  d1 %>%
    # concatenate patterns of behaviour into single string
    unite(
      col = "Pattern",
      starts_with("pun"),
      sep = ""
    ) %>%
    # get five most common strings
    group_by(Pattern) %>%
    summarise(N = n(), Proportion = round(n() / nrow(.), 2)) %>%
    arrange(desc(N)) %>%
    slice(1:5) %>%
    # explain the five most common strings
    mutate(
      Explanation = c(
        "Anti-punish strategy (exact)",
        "Avoid DI strategy (exact)",
        "Egalitarian strategy (exact)",
        "Only punish in third-party game",
        "Punish in No DI 2 and DI games"
      )
    ) %>%
    # organise columns for table
    select(Pattern, Explanation, N, Proportion)
}

# plot model results
plotModel1 <- function(post) {
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice")
  # calculate probabilities
  P <- post$alpha
  for (i in 1:nrow(P)) P[i,] <- softmax(P[i,])
  # plot
  out <- 
    tibble(
      p = c(P[,1], P[,2], P[,3], P[,4], P[,5], 
            P[,6], P[,7], P[,8], P[,9]),
      strategy = rep(strategies, each = length(P[,1]))
    ) %>%
    mutate(strategy = factor(strategy, levels = strategies)) %>%
    ggplot(aes(x = p, y = fct_rev(strategy))) +
    geom_density_ridges(rel_min_height = 0.02, colour = "white", scale = 1) +
    stat_pointinterval() +
    labs(x = "Probability of using punishment strategy", y = NULL) +
    theme_classic()
  # save
  ggsave(out, filename = "figures/study1/model1.pdf", height = 5, width = 6)
  return(out)
}

# plot effect of predictor on strategy usage
plotModelPred <- function(d, post, stdPred, unstdPred, xlab, xBreaks, file) {
  # mean and sd for original unstd predictor
  predMean <- d %>% pull(unstdPred) %>% mean(na.rm = TRUE)
  predSD <- d %>% pull(unstdPred) %>% sd(na.rm = TRUE)
  # predictor sequence
  predSeq <- seq(
    d %>% pull(stdPred) %>% min(na.rm = TRUE), 
    d %>% pull(stdPred) %>% max(na.rm = TRUE), 
    length.out = 100
    )
  # posterior predictions
  postPred <- tibble()
  # median and 95% CIs for plotting
  for (i in 1:length(predSeq)) {
    # on logit scale
    p <- post$alpha + post$beta*predSeq[i]
    # on probability scale
    for (j in 1:nrow(p)) p[j,] <- softmax(p[j,])
    # add to post
    postPred <- bind_rows(postPred, tibble(predictor = predSeq[i], 
                                           strategy = 1:10, 
                                           med = apply(p, 2, median),
                                           lower = apply(p, 2, quantile, 0.025),
                                           upper = apply(p, 2, quantile, 0.975)))
  }
  # strategy vector
  strategies <- c("Competitive", "Avoid DI", "Egalitarian", "Seek AI",
                  "Retributive", "Deterrent", "Norm-enforcing", 
                  "Antisocial", "Random choice", "Anti-punish")
  postPred$strategy <- strategies[postPred$strategy]
  postPred$strategy <- factor(postPred$strategy, levels = strategies)
  # data for geom_text (slopes)
  dataText <-
    tibble(
      strategy = factor(strategies, levels = strategies),
      label = paste0(
        "b = ", format(round(apply(post$beta, 2, median), 2), nsmall = 2),
        ", 95% CI [", format(round(apply(post$beta, 2, quantile, 0.025), 2), nsmall = 2),
        " ", format(round(apply(post$beta, 2, quantile, 0.975), 2), nsmall = 2), "]"
        )
      )
  # plot
  out <-
    ggplot(data = postPred) +
    geom_ribbon(aes(x = predictor, ymin = lower, ymax = upper), fill = "grey") +
    geom_line(aes(y = med, x = predictor)) +
    geom_text(data = dataText, aes(x = -Inf, y = -Inf, label = label), 
              hjust = -0.15, vjust = -27.5, size = 1.93) +
    facet_wrap(. ~ strategy, nrow = 2) +
    scale_y_continuous(name = "Probability of using strategy", limits = c(0, 1)) +
    scale_x_continuous(name = xlab, labels = function(x) round((x * predSD) + predMean, 0),
                       breaks = (xBreaks - predMean) / predSD) +
    theme_classic()
  # save plot
  ggsave(out, filename = file, height = 4, width = 7)
  return(out)
}
