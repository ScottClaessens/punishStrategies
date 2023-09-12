# custom functions

# plot sample demographics
plotSampleStudy <- function(d, country = "") {
  # subset to country
  d <- filter(d, Country == country)
  # age
  pA <- 
    ggplot(d, aes(x = Age)) +
    geom_histogram(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic()
  # gender
  pB <-
    ggplot(d, aes(x = Gender)) +
    geom_bar(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic()
  # ethnicity
  pC <-
    ggplot(d, aes(x = Ethnicity)) +
    geom_bar(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic()
  # student
  pD <-
    ggplot(d, aes(x = Student)) +
    geom_bar(fill = "lightsteelblue2") +
    scale_y_continuous(name = "Count") +
    theme_classic()
  # employment
  pE <-
    d %>%
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
  # title
  title <- ggdraw() + draw_label(paste0("Sample from ", country), hjust = 1.2)
  # put together and save
  out <- 
    plot_grid(
      title,
      plot_grid(pA, pC, nrow = 1), 
      plot_grid(pD, pB, pE, nrow = 1), 
      nrow = 3,
      rel_heights = c(0.15, 1, 1)
    )
  # save
  file <- 
    paste0(
      "figures/dataSummaries/",
      ifelse(
        country == "United Kingdom",
        "sample_UK.pdf",
        "sample_US.pdf"
        )
      )
  ggsave(out, filename = file, width = 7, height = 5)
  return(out)
}

# plot wordcloud for open-ended question
plotWordcloud <- function(d, country = "") {
  # edit row 1970 to reduce repetition in response for wordcloud
  d$Explain[1970] <- "money"
  # subset to country
  d <- filter(d, Country == country)
  # from: https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
  # clean responses
  d$Explain <- gsub("@\\S*", "", d$Explain) 
  d$Explain <- gsub("amp", "", d$Explain) 
  d$Explain <- gsub("[\r\n]", "", d$Explain)
  d$Explain <- gsub("[[:punct:]]", "", d$Explain)
  # create document term matrix
  words <- 
    d %>%
    dplyr::select(Explain) %>%
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
  filename <- 
    paste0(
      "figures/dataSummaries/",
      ifelse(
        country == "United Kingdom",
        "wordcloud_UK.png",
        "wordcloud_US.png"
      )
    )
  png(file = filename, width = 400, height = 400)
  wordcloud(
    words = words$word,
    freq = words$n,
    min.freq = 10,
    colors = brewer.pal(8, "Dark2")
  )
  dev.off()
}

# plot punishment rates
plotPunDecisions <- function(d) {
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
    d %>%
    # from wide to long
    pivot_longer(
      cols = starts_with("pun"),
      names_to = "Decision",
      values_to = "Punishment"
    ) %>%
    # prepare for plot
    separate(Decision, into = c("Game", "Other")) %>%
    group_by(Country) %>%
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
    group_by(Country, Game, Other) %>%
    summarise(Punishment = mean(Punishment), .groups = "drop") %>%
    # plot
    ggplot(aes(x = Game, y = Punishment, fill = Other)) +
    geom_col(position = "dodge") +
    scale_y_continuous(name = "Proportion of punishment", limits = c(0, 1),
                       expand = c(0, 0)) +
    facet_wrap(. ~ Country) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 50, hjust = 1, size = 8),
      axis.title.x = element_blank(),
      legend.title = element_blank()
    )
  # save
  ggsave(out, filename = "figures/dataSummaries/propPunish.pdf", width = 8, height = 5)
  return(out)
}

# plot slider ratings
plotSliderRatings1 <- function(d) {
  # named vector for converting to plot labels
  labels <- c(
    "SelfRate1"  = "I wanted to punish\npeople who harmed others",
    "SelfRate2"  = "I wanted to have a higher\nfinal bonus than others",
    "SelfRate3"  = "I wanted to avoid having a\nlower final bonus than others",
    "SelfRate4"  = "I wanted all players to\nhave the same final bonus",
    "SelfRate5"  = "I wanted to stop\nothers from cheating",
    "SelfRate6"  = "I wanted to show that I\ndisapproved of others' actions",
    "SelfRate7"  = "I made decisions\nat random",
    "SelfRate8"  = "I wanted to punish people who\nDID NOT harm me or others",
    "SelfRate9"  = "I didn't want to reduce anyone's\nbonus, no matter what they did",
    "SelfRate10" = "I didn't want to PAY to reduce anyone's bonus\nbut I would have done so if it were free",
    "SelfRate11" = "I wanted to punish people if they harmed me\nbut not if they harmed others"
  )
  # plot
  out <-
    d %>%
    # from wide to long
    pivot_longer(
      cols = starts_with("SelfRate"),
      names_to = "SelfRateSlider",
      values_to = "SelfRate"
    ) %>%
    # edit slider labels
    mutate(SelfRateSlider = factor(labels[SelfRateSlider], levels = labels)) %>%
    # plot
    ggplot(aes(x = SelfRate, y = fct_rev(SelfRateSlider))) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap(. ~ Country) +
    labs(x = "Rating", y = NULL) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/dataSummaries/sliders1.pdf", width = 7, height = 4)
  return(out)
}

# plot slider ratings
plotSliderRatings2 <- function(d) {
  # named vector for converting to plot labels
  labels <- c(
    "SelfRate1"  = "I wanted to punish\npeople who harmed others",
    "SelfRate2"  = "I wanted to have a higher\nfinal bonus than others",
    "SelfRate3"  = "I wanted to avoid having a\nlower final bonus than others",
    "SelfRate4"  = "I wanted all players to\nhave the same final bonus",
    "SelfRate5"  = "I wanted to stop\nothers from cheating",
    "SelfRate6"  = "I wanted to show that I\ndisapproved of others' actions",
    "SelfRate7"  = "I made decisions\nat random",
    "SelfRate8"  = "I wanted to punish people who\nDID NOT harm me or others",
    "SelfRate9"  = "I didn't want to reduce anyone's\nbonus, no matter what they did",
    "SelfRate10" = "I didn't want to PAY to reduce anyone's bonus\nbut I would have done so if it were free",
    "SelfRate11" = "I wanted to punish people if they harmed me\nbut not if they harmed others"
  )
  # plot
  out <-
    d %>%
    # create average
    mutate(avgSelfRate = rowMeans(dplyr::select(d, starts_with("SelfRate")))) %>%
    # from wide to long
    pivot_longer(
      cols = starts_with("SelfRate"),
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
    facet_wrap(. ~ Country) +
    labs(x = "Deviation from average slider rating", y = NULL) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 7))
  # save
  ggsave(out, filename = "figures/dataSummaries/sliders2.pdf", width = 7, height = 4)
  return(out)
}

# plot correlations between survey measures
plotSurveyCorrelations <- function(d) {
  out <-
    d %>%
    transmute(
      PolIdeology = PolSlider,
      SDO = SDO,
      RWA = RWA,
      Religiosity = Religiosity,
      OpennessToExperience = Open,
      Conscientiousness = Consc,
      Extraversion = Extra,
      Agreeableness = Agree,
      Neuroticism = Neur,
      HonestyHumility = Honest,
      SVOAngle = SVOangle
      ) %>%
    cor(
      method = "spearman",
      use = "pairwise"
      ) %>%
    ggcorrplot(
      type = "lower", 
      legend.title = "Correlation",
      lab = TRUE,
      lab_size = 2.5
      )
  ggsave(out, filename = "figures/dataSummaries/surveyCors.pdf", width = 7, height = 7)
  return(out)
}

# table of comprehension rates
makeCompTable <- function(d) {
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
  d %>%
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
      Comprehension = Comprehension,
      Country = Country
    ) %>%
    # summarise
    group_by(Game, Country) %>%
    summarise(Comp = round(mean(!Comprehension), 2), .groups = "drop") %>%
    # pivot wider
    pivot_wider(names_from = Country, values_from = Comp)
}

# table of raw strategy counts
makeStrategyCountTable <- function(d) {
  d %>%
    rename(Strategy = strategy) %>%
    group_by(Strategy) %>%
    summarise(
      N = n(),
      Proportion = round(n() / nrow(d), 3),
      .groups = "drop"
      ) %>%
    arrange(desc(Proportion))
}

# table for raw strategy counts with mistakes
makeStrategyCountTableWithMistakes <- function(d, nMistakes = 0) {
  d %>%
    rowwise() %>%
    mutate(
      # get hamming distance from all strategies
      `subs_Competitive`    = sum(across(pun1_1:pun6_2) != c(1,1,1,1,1,1,0,0,1,1,1,1)),
      `subs_Avoid DI`       = sum(across(pun1_1:pun6_2) != c(0,0,0,0,0,0,0,0,1,0,0,0)),
      `subs_Egalitarian`    = sum(across(pun1_1:pun6_2) != c(0,0,0,0,0,0,0,0,1,0,1,0)),
      `subs_Seek AI`        = sum(across(pun1_1:pun6_2) != c(0,0,1,0,1,0,0,0,0,0,0,0)),
      `subs_Retributive`    = sum(across(pun1_1:pun6_2) != c(1,0,1,0,1,0,1,0,1,0,0,0)),
      `subs_Deterrent`      = sum(across(pun1_1:pun6_2) != c(1,0,1,0,0,0,1,0,1,0,0,0)),
      `subs_Norm-enforcing` = sum(across(pun1_1:pun6_2) != c(1,0,1,0,0,0,1,0,1,0,1,0)),
      `subs_Antisocial`     = sum(across(pun1_1:pun6_2) != c(0,1,0,1,0,1,0,1,0,1,0,1)),
      `subs_Anti-punish`    = sum(across(pun1_1:pun6_2) != c(0,0,0,0,0,0,0,0,0,0,0,0))
    ) %>%
    # count the strategies
    pivot_longer(
      cols = starts_with("subs"),
      names_to = c(".value","Strategy"),
      names_sep = "_"
    ) %>%
    dplyr::select(ID, Strategy, subs) %>%
    # keep only instances where nMistakes or less were made
    filter(subs <= nMistakes) %>%
    group_by(Strategy) %>%
    summarise(
      N = n(),
      Proportion = round(n() / nrow(d), 3),
      .groups = "drop"
    ) %>%
    arrange(desc(Proportion))
}

# table of behaviour patterns
makePatternsTable <- function(d) {
  # vector of explanations for common strategies
  explanations <- c(
    "000000000000" = "*Anti-punish strategy (exact)",
    "000000001010" = "*Egalitarian strategy (exact)",
    "000000001000" = "*Avoid DI strategy (exact)",
    "000000000010" = "Punish when take in 3PP game",
    "001000001010" = "Punish when take in No DI 2, DI, and 3PP games",
    "001000001000" = "Punish when take in No DI 2 and DI games",
    "101000101010" = "*Norm-enforcing strategy (exact)",
    "101010101010" = "Punish when take in all games",
    "111111111111" = "Punish when take AND nothing in all games",
    "101000001010" = "Punish when take in No DI 1, No DI 2, DI, and 3PP games",
    "101000101000" = "*Deterrent strategy (exact)",
    "000000100000" = "Punish when take in No DI 4 game",
    "101000001000" = "Punish when take in No DI 1, No DI 2, and DI games",
    "100000000000" = "Punish when take in No DI 1 game",
    "001000000000" = "Punish when take in No DI 2 game",
    "100000001000" = "Punish when take in No DI 1 and DI games",
    "101010101000" = "*Retributive strategy (exact)",
    "001000101010" = "Punish when take in No DI 2, No DI 4, DI, and 3PP games",
    "000000101000" = "Punish when take in DI and 3PP games",
    "000000101010" = "Punish when take in No DI 4, DI, and 3PP games",
    "101010001010" = "Punish when take in all games except No DI 4",
    "001010101000" = "Punish when take in No DI 2, No DI 3, No DI 4, and DI games",
    "100000001010" = "Punish when take in No DI 1, DI, and 3PP games",
    "000010001010" = "Punish when take in No DI 3, DI, and 3PP games",
    "001000101000" = "Punish when take in No DI 2, No DI 4, and DI games"
  )
  d %>%
    # concatenate patterns of behaviour into single string
    unite(
      col = "Pattern",
      starts_with("pun"),
      sep = ""
    ) %>%
    # get most common strings
    group_by(Pattern) %>%
    summarise(
      N = n(),
      Proportion = round(n() / nrow(.), 3)
      ) %>%
    arrange(desc(N)) %>%
    slice(1:25) %>%
    # explain the most common strings
    mutate(Explanation = explanations[Pattern]) %>%
    # organise columns for table
    dplyr::select(Pattern, Explanation, N, Proportion)
}

# plot splits graph of behavioural patterns
plotSplitsGraph <- function(d) {
  d <-
    tar_read(d) %>%
    # subset to behavioural decisions
    dplyr::select(starts_with("pun"), strategy) %>%
    # unite behavioural decisions
    unite(
      col = "pattern",
      pun1_1:pun6_2,
      sep = "",
      remove = FALSE
    ) %>%
    # reduce to unique behavioural patterns with counts
    group_by_at(vars(starts_with("pun"))) %>%
    summarise(
      count = n(),
      pattern = unique(pattern),
      strategy = unique(as.character(strategy)),
      .groups = "drop"
    ) %>%
    # add tpp strategy
    mutate(strategy = ifelse(pattern == "000000000010", "Third-party", strategy)) %>%
    # keep only behavioural patterns with certain number of counts
    filter(count >= 2)
  # n is number of unique patterns of behaviour
  n <- nrow(d)
  # calculate Hamming distance matrix for behavioural patterns
  # (number of substitutions to get from one to another)
  # e.g. 000000000000 -> 101000000000 = Hamming distance of 2
  dist <- matrix(NA, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      # how many elements of the behavioural pattern are different?
      dist[i,j] <- sum(d[i,1:12] != d[j,1:12])
    }
  }
  rownames(dist) <- colnames(dist) <- 1:n
  # create neighbour net with phangorn package
  net <- neighborNet(dist)
  # plot splits graph
  p <- 
    ggsplitnet(
      net,
      colour = "grey90",
      linewidth = 0.05
    )
  # labels for strategies
  labelStrategies <- 
    c(
      "Anti-punish"                    = "Anti-punish\n(000000000000)",
      "Seek advantageous inequity"     = "Seek AI\n(001010000000)",
      "Egalitarian"                    = "Egalitarian\n(000000001010)",
      "Avoid disadvantageous inequity" = "Avoid DI\n(000000001000)",
      "Retributive"                    = "Retributive\n(101010101000)",
      "Deterrent"                      = "Deterrent\n(101000101000)",
      "Norm-enforcing"                 = "Norm-enforcing\n(101000101010)",
      "Competitive"                    = "Competitive\n(111111001111)",
      "Third-party"                    = "Third-party\n(000000000010)"
    )
  # additional data for plot (counts, labels)
  pd <- 
    tibble(
      tip = as.character(1:n),
      Count = d$count,
      strategy = ifelse(d$strategy == "N/A", "", labelStrategies[as.character(d$strategy)]),
      exact = strategy != ""
    ) %>%
    left_join(p$data, by = c("tip" = "label"))
  # final plot
  out <-
    p +
    # add strategies as points
    geom_point(
      data = pd,
      aes(
        x = x,
        y = y,
        size = Count,
        colour = exact
      )
    ) +
    # add labels
    geom_text_repel(
      data = pd,
      aes(label = strategy),
      colour = "red",
      size = 3.5,
      seed = 1
    ) +
    scale_colour_discrete(type = c("black","red")) +
    scale_size_continuous(breaks = c(10, 100, 500)) +
    guides(colour = "none") +
    theme(legend.position = c(0.9, 0.15))
  # save plot
  ggsave(out, filename = "figures/dataSummaries/splitsGraph.pdf", width = 7, height = 7)
  return(out)
}
