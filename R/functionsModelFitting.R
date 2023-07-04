# custom functions

# fit stan model without predictor
fitModel1 <- function(d, compiledModel1, error = 0, 
                      iter = 2000,  warmup = 1000, chains = 4, cores = 4) {
  # list for stan
  dataList <-
    list(
      N = nrow(d),
      pun1_1 = d$pun1_1,
      pun1_2 = d$pun1_2,
      pun2_1 = d$pun2_1,
      pun2_2 = d$pun2_2,
      pun3_1 = d$pun3_1,
      pun3_2 = d$pun3_2,
      pun4_1 = d$pun4_1,
      pun4_2 = d$pun4_2,
      pun5_1 = d$pun5_1,
      pun5_2 = d$pun5_2,
      pun6_1 = d$pun6_1,
      pun6_2 = d$pun6_2,
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
      # standardise predictor for model fitting
      pred = d %>% pull(predictor) %>% scale() %>% as.numeric(),
      pun1_1 = d$pun1_1,
      pun1_2 = d$pun1_2,
      pun2_1 = d$pun2_1,
      pun2_2 = d$pun2_2,
      pun3_1 = d$pun3_1,
      pun3_2 = d$pun3_2,
      pun4_1 = d$pun4_1,
      pun4_2 = d$pun4_2,
      pun5_1 = d$pun5_1,
      pun5_2 = d$pun5_2,
      pun6_1 = d$pun6_1,
      pun6_2 = d$pun6_2,
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

# fit stan model with categorical predictor
fitModel3 <- function(d, compiledModel3, predictor, error = 0,
                      iter = 2000, warmup = 1000, chains = 4, cores = 4) {
  # listwise deletion for predictor
  d <- d[as.vector(!is.na(d[,predictor])),]
  # categorical predictor as numeric
  d[[predictor]] <- as.numeric(factor(d[[predictor]]))
  # list for stan
  dataList <-
    list(
      N = nrow(d),
      J = length(unique(d[[predictor]])),
      pred = d %>% pull(predictor),
      pun1_1 = d$pun1_1,
      pun1_2 = d$pun1_2,
      pun2_1 = d$pun2_1,
      pun2_2 = d$pun2_2,
      pun3_1 = d$pun3_1,
      pun3_2 = d$pun3_2,
      pun4_1 = d$pun4_1,
      pun4_2 = d$pun4_2,
      pun5_1 = d$pun5_1,
      pun5_2 = d$pun5_2,
      pun6_1 = d$pun6_1,
      pun6_2 = d$pun6_2,
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
