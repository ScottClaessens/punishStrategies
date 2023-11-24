# custom functions

# fit stan model without predictor
fitModel1 <- function(d, compiledModel1, error = 0, 
                      iter = 2000, warmup = 1000, chains = 4, cores = 4) {
  # list for stan
  dataList <-
    list(
      N = nrow(d),
      Nc = length(unique(d$Country)),
      countryID = ifelse(d$Country == "United Kingdom", 1L, 2L),
      pun1_1 = ifelse(!is.na(d$NoDI1_Take),    d$pun1_1, -999),
      pun1_2 = ifelse(!is.na(d$NoDI1_Nothing), d$pun1_2, -999),
      pun2_1 = ifelse(!is.na(d$NoDI2_Take),    d$pun2_1, -999),
      pun2_2 = ifelse(!is.na(d$NoDI2_Nothing), d$pun2_2, -999),
      pun3_1 = ifelse(!is.na(d$NoDI3_Take),    d$pun3_1, -999),
      pun3_2 = ifelse(!is.na(d$NoDI3_Nothing), d$pun3_2, -999),
      pun4_1 = ifelse(!is.na(d$NoDI4_Take),    d$pun4_1, -999),
      pun4_2 = ifelse(!is.na(d$NoDI4_Nothing), d$pun4_2, -999),
      pun5_1 = ifelse(!is.na(d$DI_Take),       d$pun5_1, -999),
      pun5_2 = ifelse(!is.na(d$DI_Nothing),    d$pun5_2, -999),
      pun6_1 = ifelse(!is.na(d$`3PP_Take`),    d$pun6_1, -999),
      pun6_2 = ifelse(!is.na(d$`3PP_Nothing`), d$pun6_2, -999),
      error = error # assumed error rate
    )
  # fit model
  out <- 
    sampling(
      compiledModel1, 
      data = dataList, 
      iter = iter,
      warmup = warmup,
      chains = chains,
      cores = cores,
      seed = 2113
      )
  return(out)
}

# fit stan model with predictor
fitModel2 <- function(d, compiledModel2, predictor, error = 0, 
                      iter = 2000, warmup = 1000, chains = 4, cores = 4) {
  # listwise deletion for predictor
  d <- d[as.vector(!is.na(d[,predictor])),]
  # list for stan
  dataList <-
    list(
      N = nrow(d),
      Nc = length(unique(d$Country)),
      countryID = ifelse(d$Country == "United Kingdom", 1L, 2L),
      # standardise predictor for model fitting
      pred = d %>% pull(predictor) %>% scale() %>% as.numeric(),
      pun1_1 = ifelse(!is.na(d$NoDI1_Take),    d$pun1_1, -999),
      pun1_2 = ifelse(!is.na(d$NoDI1_Nothing), d$pun1_2, -999),
      pun2_1 = ifelse(!is.na(d$NoDI2_Take),    d$pun2_1, -999),
      pun2_2 = ifelse(!is.na(d$NoDI2_Nothing), d$pun2_2, -999),
      pun3_1 = ifelse(!is.na(d$NoDI3_Take),    d$pun3_1, -999),
      pun3_2 = ifelse(!is.na(d$NoDI3_Nothing), d$pun3_2, -999),
      pun4_1 = ifelse(!is.na(d$NoDI4_Take),    d$pun4_1, -999),
      pun4_2 = ifelse(!is.na(d$NoDI4_Nothing), d$pun4_2, -999),
      pun5_1 = ifelse(!is.na(d$DI_Take),       d$pun5_1, -999),
      pun5_2 = ifelse(!is.na(d$DI_Nothing),    d$pun5_2, -999),
      pun6_1 = ifelse(!is.na(d$`3PP_Take`),    d$pun6_1, -999),
      pun6_2 = ifelse(!is.na(d$`3PP_Nothing`), d$pun6_2, -999),
      error = error # assumed error rate
    )
  # fit model
  out <- 
    sampling(
      compiledModel2,
      data = dataList,
      iter = iter,
      warmup = warmup,
      chains = chains,
      cores = cores,
      seed = 2113
      )
  return(out)
}

# fit stan model with two predictors
fitModel3 <- function(d, compiledModel3, predictor1, predictor2, error = 0, 
                      iter = 2000, warmup = 1000, chains = 4, cores = 4) {
  # listwise deletion for predictors
  d <- d[as.vector(!is.na(d[,predictor1])),]
  d <- d[as.vector(!is.na(d[,predictor2])),]
  # list for stan
  dataList <-
    list(
      N = nrow(d),
      Nc = length(unique(d$Country)),
      countryID = ifelse(d$Country == "United Kingdom", 1L, 2L),
      # standardise predictor for model fitting
      pred1 = d %>% pull(predictor1) %>% scale() %>% as.numeric(),
      pred2 = d %>% pull(predictor2) %>% scale() %>% as.numeric(),
      pun1_1 = ifelse(!is.na(d$NoDI1_Take),    d$pun1_1, -999),
      pun1_2 = ifelse(!is.na(d$NoDI1_Nothing), d$pun1_2, -999),
      pun2_1 = ifelse(!is.na(d$NoDI2_Take),    d$pun2_1, -999),
      pun2_2 = ifelse(!is.na(d$NoDI2_Nothing), d$pun2_2, -999),
      pun3_1 = ifelse(!is.na(d$NoDI3_Take),    d$pun3_1, -999),
      pun3_2 = ifelse(!is.na(d$NoDI3_Nothing), d$pun3_2, -999),
      pun4_1 = ifelse(!is.na(d$NoDI4_Take),    d$pun4_1, -999),
      pun4_2 = ifelse(!is.na(d$NoDI4_Nothing), d$pun4_2, -999),
      pun5_1 = ifelse(!is.na(d$DI_Take),       d$pun5_1, -999),
      pun5_2 = ifelse(!is.na(d$DI_Nothing),    d$pun5_2, -999),
      pun6_1 = ifelse(!is.na(d$`3PP_Take`),    d$pun6_1, -999),
      pun6_2 = ifelse(!is.na(d$`3PP_Nothing`), d$pun6_2, -999),
      error = error # assumed error rate
    )
  # fit model
  out <- 
    sampling(
      compiledModel3,
      data = dataList,
      iter = iter,
      warmup = warmup,
      chains = chains,
      cores = cores,
      seed = 2113
    )
  return(out)
}

# fit stan model with categorical predictor
fitModel4 <- function(d, compiledModel4, predictor, error = 0,
                      iter = 2000, warmup = 1000, chains = 4, cores = 4) {
  # listwise deletion for predictor
  d <- d[as.vector(!is.na(d[,predictor])),]
  # categorical predictor as numeric
  d[[predictor]] <- as.numeric(factor(d[[predictor]]))
  # list for stan
  dataList <-
    list(
      N = nrow(d),
      Nc = length(unique(d$Country)),
      countryID = ifelse(d$Country == "United Kingdom", 1L, 2L),
      J = length(unique(d[[predictor]])),
      pred = d %>% pull(predictor),
      pun1_1 = ifelse(!is.na(d$NoDI1_Take),    d$pun1_1, -999),
      pun1_2 = ifelse(!is.na(d$NoDI1_Nothing), d$pun1_2, -999),
      pun2_1 = ifelse(!is.na(d$NoDI2_Take),    d$pun2_1, -999),
      pun2_2 = ifelse(!is.na(d$NoDI2_Nothing), d$pun2_2, -999),
      pun3_1 = ifelse(!is.na(d$NoDI3_Take),    d$pun3_1, -999),
      pun3_2 = ifelse(!is.na(d$NoDI3_Nothing), d$pun3_2, -999),
      pun4_1 = ifelse(!is.na(d$NoDI4_Take),    d$pun4_1, -999),
      pun4_2 = ifelse(!is.na(d$NoDI4_Nothing), d$pun4_2, -999),
      pun5_1 = ifelse(!is.na(d$DI_Take),       d$pun5_1, -999),
      pun5_2 = ifelse(!is.na(d$DI_Nothing),    d$pun5_2, -999),
      pun6_1 = ifelse(!is.na(d$`3PP_Take`),    d$pun6_1, -999),
      pun6_2 = ifelse(!is.na(d$`3PP_Nothing`), d$pun6_2, -999),
      error = error # assumed error rate
    )
  # fit model
  out <- 
    sampling(
      compiledModel4,
      data = dataList,
      iter = iter,
      warmup = warmup,
      chains = chains,
      cores = cores,
      seed = 2113
      )
  return(out)
}

# fit multilevel model for raw data comparisons
# take vs. not take
fitMLM1 <- function(d) {
  d %>%
    pivot_longer(
      starts_with("pun"),
      names_to = "decision",
      values_to = "pun"
      ) %>%
    mutate(otherTook = ifelse(str_ends(decision, "_1"), 1, 0)) %>%
    glmer(
      formula = pun ~ 1 + otherTook + (1 | ID) + (1 | decision),
      data = .,
      family = binomial
    )
}

# fit multilevel model for raw data comparisons
# target behaviour produced DI
fitMLM2 <- function(d) {
  d %>%
    pivot_longer(
      starts_with("pun"),
      names_to = "decision",
      values_to = "pun"
    ) %>%
    mutate(DI = ifelse(decision %in% c("pun5_1", "pun6_1"), 1, 0)) %>%
    glmer(
      formula = pun ~ 1 + DI + (1 | ID) + (1 | decision),
      data = .,
      family = binomial
    )
}
