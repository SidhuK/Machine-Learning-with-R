library(tidymodels)
library(tidyverse)
library(plsmod)

data(ChemicalManufacturingProcess, package = "AppliedPredictiveModeling") 
split <- initial_split(ChemicalManufacturingProcess, prop = 0.7)
train <- training(split)
test <- testing(split)

chem_rec <- recipe(Yield ~ ., data = train) %>% 
  step_knnimpute(all_predictors()) %>% 
  step_BoxCox(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  step_corr(all_predictors())

pls_spec <- pls(num_comp = 4) %>%    ## can tune instead to find the optimal number
  set_mode("regression") |> 
  set_engine("")

wf <- workflow() %>%
  add_recipe(chem_rec) %>%
  add_model(pls_spec)


pls_fit <- fit(wf, train)

## tidy the fitted model
tidy_pls <- pls_fit %>%
  pull_workflow_fit()
tidy()

tidy_pls

tidy_pls %>%
  filter(term != "Y") %>%
  group_by(component) %>%
  slice_max(abs(value), n = 10) %>%
  ungroup() %>%
  ggplot(aes(value, fct_reorder(term, value), fill = factor(component))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, scales = "free_y") +
  labs(y = NULL)
