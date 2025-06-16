library(tidymodels)
library(tidyverse)
library(mirt)
library(janitor)
library(ranger)
library(vip)

############################TRAIN MODEL###########################
train_cleaned <- read_csv("data/parsed/00-train_d_parsed.csv") |> 
  mutate(resp = as.factor(resp))

rec <- recipe(resp ~ ., data = train_cleaned) |>
  update_role(id, trial_num, new_role = "ID") |>
  step_impute_mean(all_numeric_predictors()) |>
  step_zv(all_predictors()) |>
  step_unknown(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors())

rf_mod <- rand_forest(mtry = 4, trees = 500, min_n = 5) |>
  set_engine("ranger", importance = "impurity", probability = TRUE) |>
  set_mode("classification")

wf <- workflow() |>
  add_model(rf_mod) |>
  add_recipe(rec)

rf_fit <- wf |> fit(data = train_cleaned)
#################################################################
test_cleaned <- read_csv("data/parsed/01-test_d_parsed.csv")

pred_probs <- predict(rf_fit, new_data = test_cleaned, type = "prob")$.pred_1
rmse_prob <- sqrt(mean((pred_probs - test_cleaned$resp)^2, na.rm = TRUE))
print(rmse_prob)

rf_fit |> 
  extract_fit_parsnip() |> 
  vip(num_features = 20)


