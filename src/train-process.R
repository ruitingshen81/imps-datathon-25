library(tidyverse)
library(janitor)
library(mirt)

train_d <- read_csv("data/imps2025_train.csv")


# Here I tried to derived 3 features: congruency, item type, log rt,
# and trial positions
train_cleaned <- train_d |>
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

#######################IRT FEATURES######################
irt_input <- train_cleaned |>
  filter(!is.na(resp)) |>
  group_by(id, trial_num) |>
  slice(1) |>
  ungroup() |>
  mutate(trial_num = paste0("item_", trial_num)) |>
  select(id, trial_num, resp) |>
  pivot_wider(names_from = trial_num, values_from = resp) |>
  arrange(id)

irt_data <- irt_input |> 
  select(-id) |>
  mutate(across(everything(), as.numeric))
m_2pl <- mirt(irt_data, 1, itemtype = "2PL", method = "EM")
theta <- fscores(m_2pl) |> as_tibble() |> mutate(id = irt_input$id)
##########################################################

############################Demographics##################
train_covariates <- train_d |> 
  group_by(id) |> 
  summarise(
    cov_age = first(na.omit(cov_age)),
    cov_female = first(na.omit(cov_female)),
    cov_grade = first(na.omit(cov_grade)),
    .groups = "drop"
  )
######################################################

train_cleaned_final <- train_cleaned |>
  group_by(id, trial_num) |>
  slice(1) |>
  ungroup() |>
  select(id, trial_num, resp, stim_shape, stim_side, block, congruency, item_type, log_rt, time_bin) |>
  left_join(train_covariates, by = "id") |>
  left_join(theta, by = "id") |>
  filter(!is.na(resp) & !is.na(F1)) |>
  rename(ability_est = F1) |>
  mutate(resp = as.factor(resp))

write_csv(train_cleaned_final,"data/parsed/00-train_d_parsed.csv")
write_csv(theta,"data/parsed/theta.csv")


