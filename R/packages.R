options(tidyverse.quiet = TRUE)
library(crew)
library(targets)
library(tarchetypes)
library(tidyverse)
tar_option_set(
  packages = c("ape", "brms", "cowplot", "ggcorrplot", "ggrepel",
               "ggridges", "knitr", "lme4", "loo", "ltm",
               "papaja", "phangorn", "rethinking", "rstan",
               "tanggle", "tidybayes", "tidytext", "tidyverse",
               "wordcloud")#,
  #controller = crew_controller_local(workers = 2)
)
