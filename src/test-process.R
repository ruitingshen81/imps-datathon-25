library(tidymodels)
library(tidyverse)
library(mirt)
library(janitor)
library(ranger)


test_d <- read_csv("data/imps2025_test.csv")
theta <- read_csv("data/parsed/theta.csv")

test_cleaned <- test_d |>
  mutate(
    congruency = case_when(
      stim_shape == "heart" & stim_side == "left" ~ "congruent",
      stim_shape == "heart" & stim_side == "right" ~ "congruent",
      stim_shape == "flower" & stim_side == "right" ~ "incongruent",
      stim_shape == "flower" & stim_side == "left" ~ "incongruent",
      TRUE ~ "unknown"
    ),
    item_type = case_when(
      block == "hearts test" & stim_shape == "heart" ~ "heart_only",
      block == "mixed test" & stim_shape == "heart" ~ "heart_mixed",
      block == "mixed test" & stim_shape == "flower" ~ "flower_mixed",
      TRUE ~ "other"
    ),
    log_rt = log(rt + 1),
    time_bin = ntile(trial_num, 3)
  )

test_covariates <- test_d |> 
  group_by(id) |> 
  summarise(
    cov_age = first(na.omit(cov_age)),
    cov_female = first(na.omit(cov_female)),
    cov_grade = first(na.omit(cov_grade)),
    .groups = "drop"
  )


test_cleaned_final <- test_cleaned |>
  group_by(id, trial_num) |>
  slice(1) |>
  ungroup() |>
  select(id, trial_num, resp, stim_shape, stim_side, block, congruency, item_type, log_rt, time_bin) |>
  left_join(train_covariates, by = "id") |>
  left_join(theta, by = "id") |>
  filter(!is.na(resp) & !is.na(F1)) |>
  rename(ability_est = F1) |>
  mutate(resp = as.factor(resp))

write_csv(test_cleaned_final,"data/parsed/01-test_d_parsed.csv")


