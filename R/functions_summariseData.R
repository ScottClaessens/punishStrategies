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
    "Game A\n(AI)",
    "Game B\n(Equal)",
    "Game C\n(Computer)",
    "Game D\n(1:1 Fee Fine)",
    "Game E\n(DI)",
    "Game F\n(Third-Party)"
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
      Other = factor(ifelse(Other == 1, "Steal", "No steal"), levels = c("Steal", "No steal")),
      # punishment
      Punishment = Punishment
    ) %>%
    # summarise
    group_by(Country, Game, Other) %>%
    summarise(Punishment = mean(Punishment, na.rm = TRUE), .groups = "drop") %>%
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
      `Political ideology` = PolSlider,
      SDO = SDO,
      RWA = RWA,
      Religiosity = Religiosity,
      `Openness to experience` = Open,
      Conscientiousness = Consc,
      Extraversion = Extra,
      Agreeableness = Agree,
      Neuroticism = Neur,
      `Honesty-humility` = Honest,
      `SVO angle` = SVOangle
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

# plot splits graph of behavioural patterns
plotSplitsGraph <- function(d) {
  d <-
    d %>%
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
    #mutate(strategy = ifelse(pattern == "000000000010", "Third-party", strategy)) %>%
    # add antisocial strategy
    bind_rows(
      tibble(pun1_1 = 0, pun1_2 = 1, pun2_1 = 0, pun2_2 = 1, pun3_1 = 0, pun3_2 = 1,
             pun4_1 = 0, pun4_2 = 1, pun5_1 = 0, pun5_2 = 1, pun6_1 = 0, pun6_2 = 1,
             count = 0, pattern = "010101010101", strategy = "Antisocial")
    ) %>%
    # keep only other behavioural patterns with certain number of counts
    filter(strategy != "N/A" | (strategy == "N/A" & count >= 3))
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
      colour = "grey70",
      linewidth = 0.05
    )
  # labels for strategies
  labelStrategies <- 
    c(
      "Never punish"   = "Never punish\n(000000000000)",
      "Seek AI"        = "Seek AI\n(001010000000)",
      "Egalitarian"    = "Egalitarian\n(000000001010)",
      "Avoid DI"       = "Avoid DI\n(000000001000)",
      "Retributive"    = "Retributive\n(101010101000)",
      "Deterrent"      = "Deterrent\n(101000101000)",
      "Norm-enforcing" = "Norm-enforcing\n(101000101010)",
      "Competitive"    = "Competitive\n(111111001111)",
      "Antisocial"     = "Antisocial\n(010101010101)"
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
      size = 3,
      seed = 1,
      point.padding = unit(2, "cm")
    ) +
    scale_colour_discrete(type = c("black","red")) +
    scale_size_continuous(breaks = c(10, 100, 500)) +
    guides(colour = "none") +
    theme(legend.position = c(0.9, 0.15))
  # save plot
  ggsave(out, filename = "figures/dataSummaries/splitsGraph.pdf", width = 7, height = 7)
  return(out)
}

# table of comprehension rates
makeCompTable <- function(d) {
  # games vector
  games <- c(
    "Game A (AI)",
    "Game B (Equal)",
    "Game C (Computer)",
    "Game D (1:1 Fee-Fine)",
    "Game E (DI)",
    "Game F (Third-Party)"
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
  out <-
    d %>%
    rename(Strategy = strategy) %>%
    group_by(Strategy, Country) %>%
    summarise(N = n(), .groups = "drop") %>%
    mutate(
      Prop = N / ifelse(Country == "United Kingdom", 
                        nrow(d[d$Country == "United Kingdom",]),
                        nrow(d[d$Country == "United States",]))
      ) %>%
    complete(Strategy, Country, fill = list(N = 0, Prop = 0)) %>%
    mutate(Prop = format(round(Prop, digits = 3), nsmall = 3)) %>%
    pivot_wider(names_from = Country, values_from = c(N, Prop)) %>%
    dplyr::select(c(1,2,4,3,5))
  colnames(out)[2:5] <- letters[1:4]
  return(out)
}

## table for raw strategy counts with mistakes
#makeStrategyCountTableWithMistakes <- function(d, nMistakes = 0) {
#  d %>%
#    rowwise() %>%
#    mutate(
#      # get hamming distance from all strategies
#      `subs_Competitive`    = sum(across(pun1_1:pun6_2) != c(1,1,1,1,1,1,0,0,1,1,1,1)),
#      `subs_Avoid DI`       = sum(across(pun1_1:pun6_2) != c(0,0,0,0,0,0,0,0,1,0,0,0)),
#      `subs_Egalitarian`    = sum(across(pun1_1:pun6_2) != c(0,0,0,0,0,0,0,0,1,0,1,0)),
#      `subs_Seek AI`        = sum(across(pun1_1:pun6_2) != c(0,0,1,0,1,0,0,0,0,0,0,0)),
#      `subs_Retributive`    = sum(across(pun1_1:pun6_2) != c(1,0,1,0,1,0,1,0,1,0,0,0)),
#      `subs_Deterrent`      = sum(across(pun1_1:pun6_2) != c(1,0,1,0,0,0,1,0,1,0,0,0)),
#      `subs_Norm-enforcing` = sum(across(pun1_1:pun6_2) != c(1,0,1,0,0,0,1,0,1,0,1,0)),
#      `subs_Antisocial`     = sum(across(pun1_1:pun6_2) != c(0,1,0,1,0,1,0,1,0,1,0,1)),
#      `subs_Never punish`   = sum(across(pun1_1:pun6_2) != c(0,0,0,0,0,0,0,0,0,0,0,0))
#    ) %>%
#    # count the strategies
#    pivot_longer(
#      cols = starts_with("subs"),
#      names_to = c(".value","Strategy"),
#      names_sep = "_"
#    ) %>%
#    mutate(Strategy = factor(Strategy, levels = c("Deterrent", "Norm-enforcing",
#                                                  "Retributive", "Avoid DI",
#                                                  "Egalitarian", "Seek AI",
#                                                  "Competitive", "Never punish"))) %>%
#    dplyr::select(ID, Strategy, Country, subs) %>%
#    # keep only instances where nMistakes or less were made
#    filter(subs <= nMistakes) %>%
#    group_by(Strategy, Country) %>%
#    summarise(N = n(), .groups = "drop") %>%
#    mutate(
#      Proportion = N / ifelse(Country == "United Kingdom", 
#                              nrow(d[d$Country == "United Kingdom",]),
#                              nrow(d[d$Country == "United States",]))
#    ) %>%
#    complete(Strategy, Country, fill = list(N = 0, Proportion = 0)) %>%
#    mutate(Proportion = format(round(Proportion, digits = 3), nsmall = 3)) %>%
#    pivot_wider(names_from = Country, values_from = c(N, Proportion)) %>%
#    dplyr::select(c(1,2,4,3,5))
#}

# table of behaviour patterns
makePatternsTable <- function(d) {
  # vector of explanations for common strategies
  explanations <- c(
    "000000000000" = "Never punish strategy (exact)",
    "000000001000" = "Avoid DI strategy (exact)",
    "000000001010" = "Egalitarian strategy (exact)",
    "000000000010" = "Punish when steal in Game F",
    "001000001000" = "Punish when steal in Games B and E",
    "101000001010" = "Punish when steal in Games A, B, E, and F",
    "100000000000" = "Punish when steal in Game A",
    "000000100000" = "Punish when steal in Game D",
    "001000001010" = "Punish when steal in Games B, E, and F",
    "101000101000" = "Deterrent strategy (exact)",
    "101010101010" = "Punish when steal in all games",
    "101000101010" = "Norm-enforcing strategy (exact)",
    "001000000000" = "Punish when steal in Game B",
    "001010101000" = "Punish when steal in Games B, C, D, and E",
    "100000001000" = "Punish when steal in Games A and E",
    "101000001000" = "Punish when steal in Games A, B, and E",
    "101010101000" = "Retributive strategy (exact)",
    "111111111111" = "Always punish",
    "000000101000" = "Punish when steal in Games D and E",
    "000000101010" = "Punish when steal in Games D, E, and F",
    "101010001010" = "Punish when steal in all games except Game D",
    "001000101000" = "Punish when steal in Games B, D, and E",
    "001000101010" = "Punish when steal in Games B, D, E, and F",
    "101000000000" = "Punish when steal in Games A and B",
    "101010001000" = "Punish when steal in Games A, B, C, and E"
  )
  out <-
    d %>%
    # concatenate patterns of behaviour into single string
    unite(
      col = "Pattern",
      starts_with("pun"),
      sep = ""
    ) %>%
    # get most common strings
    group_by(Pattern, Country) %>%
    summarise(N = n(), .groups = "drop") %>%
    mutate(Prop = N / ifelse(Country == "United Kingdom",
                             nrow(d[d$Country == "United Kingdom",]),
                             nrow(d[d$Country == "United States",]))) %>%
    # round proportions
    mutate(Prop = format(round(Prop, digits = 3), nsmall = 3)) %>%
    # pivot wider
    pivot_wider(
      names_from = Country,
      values_from = c(N, Prop),
      values_fill = list(N = 0, Prop = "0.000")
      ) %>%
    # arrange table
    arrange(desc(`N_United Kingdom`)) %>%
    # top 25 rows
    slice(1:25) %>%
    # explain the most common strings
    mutate(Explanation = explanations[Pattern]) %>%
    # rearrange cols
    dplyr::select(c(1,6,2,4,3,5))
  colnames(out)[3:6] <- letters[1:4]
  return(out)
}

# make table of strategies and behavioural predictions
makeStrategyTable <- function() {
  tibble(
    Function = c(
      "Deterrent", "Norm-enforcing", "Retributive", 
      "Avoid DI", "Egalitarian", "Seek AI", "Competitive",
      "Antisocial", "Never punish"
    ),
    `Behavioural strategy` = c(
      "Punish to deter another who has harmed you from harming you again in the future",
      "Punish to enforce a shared anti-harm norm and encourage future norm compliance, even amongst third parties",
      "Punish if doing so harms another who has harmed you",
      "Punish if doing so avoids disadvantageous inequity for self",
      "Punish if doing so makes payoffs for all more equal",
      "Punish if doing so produces advantageous inequity for self",
      "Punish if doing so improves your relative position",
      "Punish exclusively those who do not cause harm",
      "Never punish others"
    ),
    a = c(
      "\U2713",
      "\U2713",
      "\U2713",
      "x",
      "x",
      "x",
      "\U2713",
      "x",
      "x"
    ),
    b = c(
      "x",
      "x",
      "x",
      "x",
      "x",
      "x",
      "\U2713",
      "\U2713",
      "x"
    ),
    c = c(
      "\U2713",
      "\U2713",
      "\U2713",
      "x",
      "x",
      "\U2713",
      "\U2713",
      "x",
      "x"
    ),
    d = c(
      "x",
      "x",
      "x",
      "x",
      "x",
      "x",
      "\U2713",
      "\U2713",
      "x"
    ),
    e = c(
      "x",
      "x",
      "\U2713",
      "x",
      "x",
      "\U2713",
      "\U2713",
      "x",
      "x"
    ),
    f = c(
      "x",
      "x",
      "x",
      "x",
      "x",
      "x",
      "\U2713",
      "\U2713",
      "x"
    ),
    g = c(
      "\U2713",
      "\U2713",
      "\U2713",
      "x",
      "x",
      "x",
      "x",
      "x",
      "x"
    ),
    h = c(
      "x",
      "x",
      "x",
      "x",
      "x",
      "x",
      "x",
      "\U2713",
      "x"
    ),
    i = c(
      "\U2713",
      "\U2713",
      "\U2713",
      "\U2713",
      "\U2713",
      "x",
      "\U2713",
      "x",
      "x"
    ),
    j = c(
      "x",
      "x",
      "x",
      "x",
      "x",
      "x",
      "\U2713",
      "\U2713",
      "x"
    ),
    k = c(
      "x",
      "\U2713",
      "x",
      "x",
      "\U2713",
      "x",
      "\U2713",
      "x",
      "x"
    ),
    l = c(
      "x",
      "x",
      "x",
      "x",
      "x",
      "x",
      "\U2713",
      "\U2713",
      "x"
    ),
    .name_repair = "minimal"
  )
}

# make table of wordings for self-report sliders
makeSliderWordingTable <- function() {
  tibble(
    Slider = 1:11,
    Wording = c(
      "I wanted to punish people who harmed others",
      "I wanted to have a higher final bonus than others",
      "I wanted to avoid having a lower final bonus than others",
      "I wanted all players to have the same final bonus",
      "I wanted to stop others from cheating",
      "I wanted to show that I disapproved of others' actions",
      "I made decisions at random",
      "I wanted to punish people who DID NOT harm me or others",
      "I didn't want to reduce anyone's bonus, no matter what they did",
      "I didn't want to PAY to reduce anyone's bonus but I would have done so if it were free",
      "I wanted to punish people if they harmed me but not if they harmed others"
    )
  )
}

# make table of wordings to survey questions
makeSurveyWordingTable <- function() {
  tibble(
    Measure = c(
      "Demographics", rep(" ", times = 2),
      "Big 6 Extraversion", rep(" ", times = 3),
      "Big 6 Agreeableness", rep(" ", times = 3),
      "Big 6 Conscientiousness", rep(" ", times = 3),
      "Big 6 Neuroticism", rep(" ", times = 3),
      "Big 6 Openness to experience", rep(" ", times = 3),
      "Big 6 Honesty-humility", rep(" ", times = 3),
      "Social Value Orientation",
      "Left-right political ideology",
      "Social Dominance Orientation", rep(" ", times = 7),
      "Right Wing Authoritarianism", rep(" ", times = 5),
      "Views on social inequality", " ",
      "Religious views", " "
    ),
    Wording = c(
      # Demographics
      "What is your highest level of education?",
      paste0("Where would you place yourself on this ladder? Please indicate ",
             "which number on the rung best represents where you stand at this ",
             "time in your life, relative to other people in your country"),
      paste0("Please could you tell us roughly how many years have you lived ",
             "in your current country of residence?"),
      # Extraversion
      "I am the life of the party",
      "I don't talk a lot (reversed)",
      "I keep in the background (reversed)",
      "I talk to a lot of different people at parties",
      # Agreeableness
      "I sympathise with others' feelings",
      "I am not interested in other people's problems (reversed)",
      "I feel others' emotions",
      "I am not really interested in others (reversed)",
      # Conscientiousness
      "I get chores done right away",
      "I like order",
      "I make a mess of things (reversed)",
      "I often forget to put things back in their proper place (reversed)",
      # Neuroticism
      "I have frequent mood swings",
      "I am relaxed most of the time (reversed)",
      "I get upset easily",
      "I seldom feel blue (reversed)",
      # Openness to experience
      "I have a vivid imagination",
      "I have difficulty understanding abstract ideas",
      "I do not have a good imagination (reversed)",
      "I am not interested in abstract ideas (reversed)",
      # Honesty-humility
      "I feel entitled to more of everything (reversed)",
      "I deserve more things in life (reversed)",
      "I would like to be seen driving around in a very expensive car (reversed)",
      "I would get a lot of pleasure from owning expensive luxury goods (reversed)",
      # SVO
      "Please indicate how you would like to distribute money between yourself and the other player",
      # Left-right political ideology
      paste0("Political views are often organised on a single scale from left ",
             "to right. For example, in the United States, the Democratic Party ",
             "is described as more to the left and the Republican Party is described ",
             "as more to the right. If you had to place your political views on ",
             "this left-right scale, generally speaking, where would you put yourself?"),
      # SDO
      "An ideal society requires some groups to be on top and others to be on the bottom",
      "Some groups of people are simply inferior to other groups",
      "No one group should dominate in society (reversed)",
      "Groups at the bottom are just as deserving as groups at the top (reversed)",
      "Group equality should not be our primary goal",
      "It is unjust to try to make groups equal",
      "We should do what we can to equalize conditions for different groups (reversed)",
      "We should work to give all groups an equal chance to succeed (reversed)",
      # RWA
      "It's great that many young people today are prepared to defy authority (reversed)",
      "What our country needs most is discipline, with everyone following our leaders in unity",
      "God's laws about abortion, pornography, and marriage must be strictly followed before it is too late",
      "There is nothing wrong with premarital sexual intercourse (reversed)",
      "Our society does NOT need tougher government and stricter laws (reversed)",
      paste0("The facts on crime and the recent public disorders show we have ",
             "to crack down harder on troublemakers, if we are going to preserve ",
             "law and order"),
      # Views on social inequality
      "I would like to bring the people above me on the ladder down a peg or two",
      "I would like to bring the people below me on the ladder up a peg or two",
      # Religious views
      "How religious are you?",
      paste0("It is likely that God, or some other type of spiritual non-human ",
             "entity, controls the events in the world")
    ),
    Scale = c(
      rep(" ", times = 3),
      rep("1-7", times = 24),
      "9 choices",
      "0-100 slider",
      rep("1-7", times = 8),
      rep("1-9", times = 6),
      rep("1-7", times = 2),
      "1-5",
      "1-7"
      )
  )
}
