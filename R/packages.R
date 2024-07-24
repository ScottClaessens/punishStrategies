options(tidyverse.quiet = TRUE)
library(crew.cluster)
library(targets)
library(tarchetypes)
library(tidyverse)
tar_option_set(
  packages = c("ape", "brms", "cowplot", "ggcorrplot", "ggrepel",
               "ggridges", "knitr", "lme4", "loo", "ltm",
               "papaja", "phangorn", "rethinking", "rstan",
               "tanggle", "tidybayes", "tidytext", "tidyverse",
               "wordcloud")#,
  #controller = crew_controller_slurm(
  #  name = "punishStrategies",
  #  workers = 1,
  #  script_lines = "module load R",
  #  slurm_time_minutes = 1440,
  #  slurm_memory_gigabytes_per_cpu = 60,
  #  slurm_cpus_per_task = 4,
  #  garbage_collection = TRUE
  #),
  #memory = "transient",
  #garbage_collection = TRUE
)
