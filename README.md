# Why do people punish? Evidence for a range of strategic concerns

Using behavioural economic games to tease apart the strategies underlying punishment in humans.

## Getting Started

### Installing

To run this code, you will need to [install R](https://www.r-project.org/) and the following R packages:

```
install.packages(c("ape", "cowplot", "ggcorrplot", "ggrepel", 
                   "ggridges", "knitr", "lme4", "ltm", "papaja", 
                   "phangorn", "rstan", "tanggle", "targets", 
                   "tarchetypes", "tidybayes", "tidytext",
                   "tidyverse", "wordcloud"))
```

You will also need to install the `rethinking` package. See here for more details: [https://github.com/rmcelreath/rethinking](https://github.com/rmcelreath/rethinking)

### Executing code

1. Set the working directory to this code repository `setwd("myPath/punishStrategies")`
2. Load the `targets` package with `library(targets)`
3. To run the analysis pipeline, run `tar_make()`
4. To load individual targets into your environment, run `tar_load(fitSimModel)` etc.

## Help

Any issues, please email scott.claessens@gmail.com.

## Authors

Scott Claessens, scott.claessens@gmail.com
