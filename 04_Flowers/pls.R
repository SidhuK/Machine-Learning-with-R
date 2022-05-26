library(tidymodels)
library(plsmod)
library(AppliedPredictiveModeling)
data(permeability)

df <- cbind(fingerprints, permeability)
df <- as_tibble(df)

set.seed(123)
perm_split <- initial_split(df)
perm_train <- training(perm_split)
perm_test <- testing(perm_split)

set.seed(234)
perm_folds <-  vfold_cv(perm_train, v=10)

perm_rec <- recipe(permeability ~ ., data = perm_train) %>%
  step_nzv(all_predictors()) %>%
  step_center(all_numeric(), -all_outcomes()) %>% 
  step_scale(all_numeric(), -all_outcomes())

pls_spec <- pls(num_comp = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("mixOmics")

comp_grid <- tibble(num_comp = seq(from = 1, to = 20, by = 5))

doParallel::registerDoParallel()

workflow() %>% 
  add_recipe(perm_rec) %>%
  add_model(pls_spec) %>%
  tune_grid(resamples = perm_folds, grid = comp_grid)

