#### Preamble ####
# Purpose: To create the graph and linear model from the simulated dataset.
# Author: Raghav Bhatia 
# Date: 4 April 2024
# Contact: raghav.bhatia@mail.utoronto.ca
# License: MIT
# Pre-requisites: Have the simulated dataset.


#### Workspace setup ####
library(boot)
library(broom.mixed)
library(collapse)
library(janitor)
library(knitr)
library(marginaleffects)
library(modelsummary)
library(rstanarm)
library(tidybayes)
library(tidyverse)
library(arrow)

#### Read data ####
voting_data <- read_parquet("data/cleaned_data/voter_preferences")

### Model ####

set.seed(100)

# This glm regressing voting outcome on predictors such as gender, highest education,
# income group, and age group 

voted_for_model <-
  stan_glm(
    voted ~ gender + highest_education + Income_group + age_group,
    data = voting_data,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = 
      normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 100
  )


### Graph ###

modelplot(voted_for_model, conf_level = 0.9) +
  labs(x = "90 per cent credibility interval")


#### Save model ####

saveRDS(
  voted_for_model,
  file = "models/voted_for_model.rds"
)


