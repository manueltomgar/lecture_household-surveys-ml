# Project       : Lecture on using statistical learning models with household survey microdata
# Last update   : 09/06/2023
# Author        : Manuel Tomas (manuel.tomas@bc3research.org)
# Institution   : Basque Centre for Climate Change (BC3)

# [] Objective ----

# Fit and compare machine learning models for two tasks using the HBS dataset:
# 1. Regression: predict household home energy expenditure.
# 2. Classification: predict whether the household is energy poor under the 10% rule.
#
# Models covered:
# - Random Forest
# - XGBoost
#
# Key modelling concepts:
# - Train/test split
# - Cross-validation
# - Hyperparameter tuning
# - Model selection
# - Predictor importance
# - Final evaluation on a held-out test set

# [] Preliminaries ----

rm(list = ls(all = TRUE))
Sys.setenv(LANG = "en")
start_time <- Sys.time()

packages_loaded <- installed.packages()
packages_needed <- c(
  "dplyr", "tidyr", "ggplot2", "readr", "here", "scales",
  "tidymodels", "ranger", "xgboost", "vip", "doParallel"
)

for (p in packages_needed) {
  if (!p %in% row.names(packages_loaded)) install.packages(p)
  eval(bquote(library(.(p))))
}

tidymodels_prefer()
set.seed(123)

path <- here()
setwd(path)

if (!dir.exists(file.path(path, "outputs"))) dir.create(file.path(path, "outputs"))
if (!dir.exists(file.path(path, "outputs", "models"))) dir.create(file.path(path, "outputs", "models"))
if (!dir.exists(file.path(path, "outputs", "figures"))) dir.create(file.path(path, "outputs", "figures"))
if (!dir.exists(file.path(path, "outputs", "tables"))) dir.create(file.path(path, "outputs", "tables"))

# [] Load dataset ----

ml_file <- file.path(
  path,
  "outputs",
  "indicators",
  "hbs_energy_ml_with_indicators_2023.csv"
)

hbs <- read_csv(ml_file, show_col_types = FALSE)

# [] Prepare modelling data ----

model_data <- hbs %>%
  mutate(
    annual_net_income_eur = if_else(
      is.na(annual_net_income_eur),
      monthly_net_income_eur * 12,
      annual_net_income_eur
    ),
    
    log_home_energy_spend = log1p(home_energy_spend_eur),
    
    energy_poor_10pct = factor(
      if_else(
        energy_poor_10pct == 1,
        "energy_poor",
        "not_energy_poor"
      ),
      levels = c("energy_poor", "not_energy_poor")
    ),
    
    region_code = as.factor(region_code),
    municipality_size_code = as.factor(municipality_size_code),
    heating_energy_source = as.factor(heating_energy_source),
    hot_water_energy_source = as.factor(hot_water_energy_source)
  ) %>%
  select(
    home_energy_spend_eur,
    log_home_energy_spend,
    energy_poor_10pct,
    monthly_net_income_eur,
    annual_net_income_eur,
    total_consumption_spend_eur,
    household_size,
    children_under18,
    employed_members,
    dwelling_area_m2,
    region_code,
    municipality_size_code,
    heating_energy_source,
    hot_water_energy_source
  ) %>%
  filter(
    !is.na(home_energy_spend_eur),
    !is.na(energy_poor_10pct)
  )

# Keep a simple predictor set for teaching.
predictors <- c(
  "monthly_net_income_eur",
  "total_consumption_spend_eur",
  "household_size",
  "children_under18",
  "employed_members",
  "dwelling_area_m2",
  "region_code",
  "municipality_size_code",
  "heating_energy_source",
  "hot_water_energy_source"
)

# [] Train/test split ----

# The final test set is kept untouched until the end.
# Cross-validation and tuning are done only inside the training set.

set.seed(123)

data_split <- initial_split(
  model_data,
  prop = 0.75,
  strata = energy_poor_10pct
)

train_data <- training(data_split)
test_data  <- testing(data_split)

# [] Cross-validation folds ----

# Minimal CV setup for teaching.
# Using 3 folds makes the exercise faster than 5-fold CV.

set.seed(123)

folds_regression <- vfold_cv(
  train_data,
  v = 3,
  strata = home_energy_spend_eur
)

set.seed(123)

folds_classification <- vfold_cv(
  train_data,
  v = 3,
  strata = energy_poor_10pct
)

# [] Recipes ----

recipe_regression <- recipe(
  home_energy_spend_eur ~ monthly_net_income_eur + total_consumption_spend_eur +
    household_size + children_under18 + employed_members + dwelling_area_m2 +
    region_code + municipality_size_code + heating_energy_source + hot_water_energy_source,
  data = train_data
) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

recipe_classification <- recipe(
  energy_poor_10pct ~ monthly_net_income_eur + total_consumption_spend_eur +
    household_size + children_under18 + employed_members + dwelling_area_m2 +
    region_code + municipality_size_code + heating_energy_source + hot_water_energy_source,
  data = train_data
) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# [] Model specifications ----

# Random Forest:

rf_regression_spec <- rand_forest(
  trees = 300,
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine(
    "ranger",
    importance = "permutation",
    num.threads = parallel::detectCores()
  ) %>%
  set_mode("regression")

rf_classification_spec <- rand_forest(
  trees = 300,
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine(
    "ranger",
    importance = "permutation",
    probability = TRUE,
    num.threads = parallel::detectCores()
  ) %>%
  set_mode("classification")

# XGBoost:

xgb_regression_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_classification_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# [] Workflows ----

rf_regression_wf <- workflow() %>%
  add_model(rf_regression_spec) %>%
  add_recipe(recipe_regression)

xgb_regression_wf <- workflow() %>%
  add_model(xgb_regression_spec) %>%
  add_recipe(recipe_regression)

rf_classification_wf <- workflow() %>%
  add_model(rf_classification_spec) %>%
  add_recipe(recipe_classification)

xgb_classification_wf <- workflow() %>%
  add_model(xgb_classification_spec) %>%
  add_recipe(recipe_classification)

# [] Minimal hyperparameter grids ----

# Minimal Random Forest grid:
# - mtry controls how many predictors are tried at each split.
# - min_n controls the minimum number of observations in terminal nodes.
#
# 4 combinations only.

rf_grid <- tibble(
  mtry  = c(3, 6, 3, 6),
  min_n = c(5, 5, 25, 25)
)

# Minimal XGBoost grid:
# We vary only two key parameters:
# - tree_depth: complexity of interactions.
# - learn_rate: speed of learning.
#
# Other parameters are fixed to simple teaching values.
# 4 combinations only.

xgb_grid <- tibble(
  trees = c(300, 300, 500, 500),
  tree_depth = c(2, 4, 2, 4),
  learn_rate = c(0.03, 0.03, 0.10, 0.10),
  loss_reduction = c(0, 0, 0, 0),
  sample_size = c(0.80, 0.80, 0.80, 0.80),
  mtry = c(6, 6, 6, 6),
  min_n = c(10, 10, 10, 10)
)

# [] Tune models with cross-validation ----

# Parallel processing is optional but useful for tree models.

cores <- max(1, parallel::detectCores() - 1)
cl <- parallel::makePSOCKcluster(cores)
doParallel::registerDoParallel(cl)

# Make sure the cluster is stopped even if an error occurs later.

on.exit({
  try(parallel::stopCluster(cl), silent = TRUE)
}, add = TRUE)

control <- control_grid(
  save_pred = TRUE,
  save_workflow = TRUE,
  verbose = TRUE
)

regression_metrics <- metric_set(
  rmse,
  mae,
  rsq
)

classification_metrics <- metric_set(
  roc_auc,
  pr_auc,
  accuracy,
  sens,
  spec
)

set.seed(123)

rf_regression_tuned <- tune_grid(
  rf_regression_wf,
  resamples = folds_regression,
  grid = rf_grid,
  metrics = regression_metrics,
  control = control
)

set.seed(123)

xgb_regression_tuned <- tune_grid(
  xgb_regression_wf,
  resamples = folds_regression,
  grid = xgb_grid,
  metrics = regression_metrics,
  control = control
)

set.seed(123)

rf_classification_tuned <- tune_grid(
  rf_classification_wf,
  resamples = folds_classification,
  grid = rf_grid,
  metrics = classification_metrics,
  control = control
)

set.seed(123)

xgb_classification_tuned <- tune_grid(
  xgb_classification_wf,
  resamples = folds_classification,
  grid = xgb_grid,
  metrics = classification_metrics,
  control = control
)

parallel::stopCluster(cl)

# [] Compare cross-validation performance ----

cv_regression_results <- bind_rows(
  collect_metrics(rf_regression_tuned) %>%
    mutate(model = "Random Forest"),
  
  collect_metrics(xgb_regression_tuned) %>%
    mutate(model = "XGBoost")
)

cv_classification_results <- bind_rows(
  collect_metrics(rf_classification_tuned) %>%
    mutate(model = "Random Forest"),
  
  collect_metrics(xgb_classification_tuned) %>%
    mutate(model = "XGBoost")
)

write_csv(
  cv_regression_results,
  file.path(path, "outputs", "tables", "ml_cv_regression_results.csv")
)

write_csv(
  cv_classification_results,
  file.path(path, "outputs", "tables", "ml_cv_classification_results.csv")
)

# Select best model by the primary metric.

best_rf_regression <- select_best(
  rf_regression_tuned,
  metric = "rmse"
)

best_xgb_regression <- select_best(
  xgb_regression_tuned,
  metric = "rmse"
)

best_rf_classification <- select_best(
  rf_classification_tuned,
  metric = "roc_auc"
)

best_xgb_classification <- select_best(
  xgb_classification_tuned,
  metric = "roc_auc"
)

# [] Final fit on training data and evaluation on test data ----

final_rf_regression <- finalize_workflow(
  rf_regression_wf,
  best_rf_regression
) %>%
  last_fit(
    split = data_split,
    metrics = regression_metrics
  )

final_xgb_regression <- finalize_workflow(
  xgb_regression_wf,
  best_xgb_regression
) %>%
  last_fit(
    split = data_split,
    metrics = regression_metrics
  )

final_rf_classification <- finalize_workflow(
  rf_classification_wf,
  best_rf_classification
) %>%
  last_fit(
    split = data_split,
    metrics = classification_metrics
  )

final_xgb_classification <- finalize_workflow(
  xgb_classification_wf,
  best_xgb_classification
) %>%
  last_fit(
    split = data_split,
    metrics = classification_metrics
  )

test_regression_results <- bind_rows(
  collect_metrics(final_rf_regression) %>%
    mutate(model = "Random Forest"),
  
  collect_metrics(final_xgb_regression) %>%
    mutate(model = "XGBoost")
)

test_classification_results <- bind_rows(
  collect_metrics(final_rf_classification) %>%
    mutate(model = "Random Forest"),
  
  collect_metrics(final_xgb_classification) %>%
    mutate(model = "XGBoost")
)

write_csv(
  test_regression_results,
  file.path(path, "outputs", "tables", "ml_test_regression_results.csv")
)

write_csv(
  test_classification_results,
  file.path(path, "outputs", "tables", "ml_test_classification_results.csv")
)

# [] Choose final models ----

# Regression: choose the model with the smallest test RMSE.

final_regression_choice <- test_regression_results %>%
  filter(.metric == "rmse") %>%
  arrange(.estimate) %>%
  slice(1) %>%
  pull(model)

# Classification: choose the model with the largest test ROC AUC.

final_classification_choice <- test_classification_results %>%
  filter(.metric == "roc_auc") %>%
  arrange(desc(.estimate)) %>%
  slice(1) %>%
  pull(model)

final_regression_fit <- if (final_regression_choice == "Random Forest") {
  final_rf_regression
} else {
  final_xgb_regression
}

final_classification_fit <- if (final_classification_choice == "Random Forest") {
  final_rf_classification
} else {
  final_xgb_classification
}

# Save all final model objects from last_fit.

saveRDS(
  final_rf_regression,
  file.path(path, "outputs", "models", "ml_rf_regression_last_fit.rds")
)

saveRDS(
  final_xgb_regression,
  file.path(path, "outputs", "models", "ml_xgb_regression_last_fit.rds")
)

saveRDS(
  final_rf_classification,
  file.path(path, "outputs", "models", "ml_rf_classification_last_fit.rds")
)

saveRDS(
  final_xgb_classification,
  file.path(path, "outputs", "models", "ml_xgb_classification_last_fit.rds")
)

# [] Save best fitted workflows for prediction ----

# last_fit() stores the fitted workflow in the .workflow column.
# These two objects are directly usable with predict().

best_regression_model <- final_regression_fit$.workflow[[1]]
best_classification_model <- final_classification_fit$.workflow[[1]]

saveRDS(
  best_regression_model,
  file.path(path, "outputs", "models", "best_regression_model.rds")
)

saveRDS(
  best_classification_model,
  file.path(path, "outputs", "models", "best_classification_model.rds")
)

cat("\nBest regression model:", final_regression_choice, "\n")
cat("Saved in: outputs/models/best_regression_model.rds\n")

cat("\nBest classification model:", final_classification_choice, "\n")
cat("Saved in: outputs/models/best_classification_model.rds\n")

# [] Predictor importance ----

# Extract fitted workflows from selected models.

final_regression_wf <- best_regression_model
final_classification_wf <- best_classification_model

final_regression_engine <- final_regression_wf %>%
  extract_fit_parsnip() %>%
  pluck("fit")

final_classification_engine <- final_classification_wf %>%
  extract_fit_parsnip() %>%
  pluck("fit")

p_vip_regression <- vip::vip(final_regression_engine, num_features = 15) +
  labs(
    title = paste("Predictor importance for", final_regression_choice),
    subtitle = "Outcome: home energy expenditure"
  ) +
  theme_minimal()

ggsave(
  file.path(path, "outputs", "figures", "ml_predictor_importance_regression.png"),
  p_vip_regression,
  width = 8.5,
  height = 5,
  dpi = 300
)

p_vip_classification <- vip::vip(final_classification_engine, num_features = 15) +
  labs(
    title = paste("Predictor importance for", final_classification_choice),
    subtitle = "Outcome: energy poverty, 10% rule"
  ) +
  theme_minimal()

ggsave(
  file.path(path, "outputs", "figures", "ml_predictor_importance_classification.png"),
  p_vip_classification,
  width = 8.5,
  height = 5,
  dpi = 300
)

# [] Save compact model summary ----

model_selection_summary <- bind_rows(
  test_regression_results %>%
    mutate(task = "Regression: home energy demand") %>%
    select(task, model, .metric, .estimate),
  
  test_classification_results %>%
    mutate(task = "Classification: energy poverty") %>%
    select(task, model, .metric, .estimate)
)

write_csv(
  model_selection_summary,
  file.path(path, "outputs", "tables", "ml_model_selection_summary.csv")
)

print(model_selection_summary)

# [] Predict on a new HBS dataset and compare observed vs predicted ----

# [] Load best models for prediction ----

best_regression_model <- readRDS(
  file.path(path, "outputs", "models", "best_regression_model.rds")
)

best_classification_model <- readRDS(
  file.path(path, "outputs", "models", "best_classification_model.rds")
)

# [] Load new HBS dataset ----

new_hbs_file <- file.path(
  path,
  "outputs",
  "indicators",
  "hbs_energy_ml_with_indicators_2023.csv"
)

hbs_new_raw <- read_csv(new_hbs_file, show_col_types = FALSE)

# [] Prepare new HBS data with the same structure as the training data ----

hbs_new <- hbs_new_raw %>%
  mutate(
    annual_net_income_eur = if_else(
      is.na(annual_net_income_eur),
      monthly_net_income_eur * 12,
      annual_net_income_eur
    ),
    
    log_home_energy_spend = log1p(home_energy_spend_eur),
    
    energy_poor_10pct = factor(
      if_else(
        energy_poor_10pct == 1,
        "energy_poor",
        "not_energy_poor"
      ),
      levels = c("energy_poor", "not_energy_poor")
    ),
    
    region_code = as.factor(region_code),
    municipality_size_code = as.factor(municipality_size_code),
    heating_energy_source = as.factor(heating_energy_source),
    hot_water_energy_source = as.factor(hot_water_energy_source)
  ) %>%
  select(
    home_energy_spend_eur,
    log_home_energy_spend,
    energy_poor_10pct,
    monthly_net_income_eur,
    annual_net_income_eur,
    total_consumption_spend_eur,
    household_size,
    children_under18,
    employed_members,
    dwelling_area_m2,
    region_code,
    municipality_size_code,
    heating_energy_source,
    hot_water_energy_source
  ) %>%
  filter(
    !is.na(home_energy_spend_eur),
    !is.na(energy_poor_10pct)
  )

# [] Predict home energy expenditure ----

hbs_new_regression_predictions <- hbs_new %>%
  bind_cols(
    predict(
      best_regression_model,
      new_data = hbs_new
    )
  ) %>%
  rename(
    predicted_home_energy_spend_eur = .pred
  )

# [] Predict energy poverty classification ----

hbs_new_classification_predictions <- hbs_new %>%
  bind_cols(
    predict(
      best_classification_model,
      new_data = hbs_new,
      type = "class"
    ),
    predict(
      best_classification_model,
      new_data = hbs_new,
      type = "prob"
    )
  ) %>%
  rename(
    predicted_energy_poverty_class = .pred_class,
    predicted_energy_poverty_risk = .pred_energy_poor
  )

# [] Combine observed and predicted values ----

hbs_new_predictions <- hbs_new %>%
  select(
    home_energy_spend_eur,
    energy_poor_10pct
  ) %>%
  bind_cols(
    hbs_new_regression_predictions %>%
      select(predicted_home_energy_spend_eur),
    
    hbs_new_classification_predictions %>%
      select(
        predicted_energy_poverty_class,
        predicted_energy_poverty_risk
      )
  ) %>%
  mutate(
    regression_error = predicted_home_energy_spend_eur - home_energy_spend_eur,
    absolute_regression_error = abs(regression_error),
    squared_regression_error = regression_error^2,
    correct_classification = energy_poor_10pct == predicted_energy_poverty_class
  )

# [] Regression comparison: observed vs predicted ----

new_regression_metrics <- hbs_new_predictions %>%
  summarise(
    observed_mean_home_energy_spend = mean(home_energy_spend_eur, na.rm = TRUE),
    predicted_mean_home_energy_spend = mean(predicted_home_energy_spend_eur, na.rm = TRUE),
    mean_error = mean(regression_error, na.rm = TRUE),
    mae = mean(absolute_regression_error, na.rm = TRUE),
    rmse = sqrt(mean(squared_regression_error, na.rm = TRUE)),
    correlation_observed_predicted = cor(
      home_energy_spend_eur,
      predicted_home_energy_spend_eur,
      use = "complete.obs"
    )
  )

print(new_regression_metrics)

write_csv(
  new_regression_metrics,
  file.path(path, "outputs", "tables", "new_hbs_regression_comparison.csv")
)

# [] Classification comparison: observed vs predicted ----

new_classification_metrics <- hbs_new_predictions %>%
  summarise(
    observed_energy_poverty_rate = mean(
      energy_poor_10pct == "energy_poor",
      na.rm = TRUE
    ),
    predicted_energy_poverty_rate = mean(
      predicted_energy_poverty_class == "energy_poor",
      na.rm = TRUE
    ),
    mean_predicted_risk = mean(
      predicted_energy_poverty_risk,
      na.rm = TRUE
    ),
    accuracy = mean(
      correct_classification,
      na.rm = TRUE
    )
  )

print(new_classification_metrics)

write_csv(
  new_classification_metrics,
  file.path(path, "outputs", "tables", "new_hbs_classification_comparison.csv")
)

# [] Confusion matrix ----

new_confusion_matrix <- hbs_new_predictions %>%
  conf_mat(
    truth = energy_poor_10pct,
    estimate = predicted_energy_poverty_class
  )

print(new_confusion_matrix)

new_confusion_matrix_table <- new_confusion_matrix$table %>%
  as.data.frame()

write_csv(
  new_confusion_matrix_table,
  file.path(path, "outputs", "tables", "new_hbs_confusion_matrix.csv")
)

# [] Additional classification metrics ----

new_accuracy <- hbs_new_predictions %>%
  accuracy(
    truth = energy_poor_10pct,
    estimate = predicted_energy_poverty_class
  )

new_sensitivity <- hbs_new_predictions %>%
  sens(
    truth = energy_poor_10pct,
    estimate = predicted_energy_poverty_class,
    event_level = "first"
  )

new_specificity <- hbs_new_predictions %>%
  spec(
    truth = energy_poor_10pct,
    estimate = predicted_energy_poverty_class,
    event_level = "first"
  )

new_roc_auc <- hbs_new_predictions %>%
  roc_auc(
    truth = energy_poor_10pct,
    predicted_energy_poverty_risk,
    event_level = "first"
  )

new_pr_auc <- hbs_new_predictions %>%
  pr_auc(
    truth = energy_poor_10pct,
    predicted_energy_poverty_risk,
    event_level = "first"
  )

new_classification_yardstick <- bind_rows(
  new_accuracy,
  new_sensitivity,
  new_specificity,
  new_roc_auc,
  new_pr_auc
)

print(new_classification_yardstick)

write_csv(
  new_classification_yardstick,
  file.path(path, "outputs", "tables", "new_hbs_classification_metrics.csv")
)

# [] Save household-level predictions ----

write_csv(
  hbs_new_predictions,
  file.path(path, "outputs", "tables", "new_hbs_observed_vs_predicted.csv")
)

# [] Plot 1: observed vs predicted home energy expenditure ----

p_new_observed_predicted <- hbs_new_predictions %>%
  ggplot(
    aes(
      x = home_energy_spend_eur,
      y = predicted_home_energy_spend_eur
    )
  ) +
  geom_point(alpha = 0.25) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Observed vs predicted home energy expenditure",
    subtitle = "New HBS dataset",
    x = "Observed home energy expenditure, euros/year",
    y = "Predicted home energy expenditure, euros/year"
  ) +
  theme_minimal()

ggsave(
  file.path(path, "outputs", "figures", "new_hbs_observed_vs_predicted_regression.png"),
  p_new_observed_predicted,
  width = 8.5,
  height = 5,
  dpi = 300
)

# [] Plot 2: distribution of observed and predicted expenditure ----

p_new_distribution <- hbs_new_predictions %>%
  select(
    observed = home_energy_spend_eur,
    predicted = predicted_home_energy_spend_eur
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "series",
    values_to = "home_energy_spend_eur"
  ) %>%
  ggplot(
    aes(
      x = home_energy_spend_eur,
      fill = series
    )
  ) +
  geom_density(alpha = 0.35) +
  labs(
    title = "Distribution of observed and predicted home energy expenditure",
    subtitle = "New HBS dataset",
    x = "Home energy expenditure, euros/year",
    y = "Density",
    fill = NULL
  ) +
  theme_minimal()

ggsave(
  file.path(path, "outputs", "figures", "new_hbs_distribution_observed_predicted.png"),
  p_new_distribution,
  width = 8.5,
  height = 5,
  dpi = 300
)

# [] End ----