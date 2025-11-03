# Setup -----------------------------------------------------------------
# clearing everything
# 534778
rm(list = ls())

# loading in libraries
library(tidyverse)
library(discrim)
library(naivebayes)
library(bonsai)
library(dbarts)
library(tidymodels)
library(embed)
library(lme4)
library(vroom)
library(ranger)
library(themis)
# library(keras)
# library(tensorflow)
# library(kknn)
# library(patchwork)
# library(janitor)

# # Progress handler ------------------------------------------------------
# library(progressr)
# library(purrr)
# handlers(global = T)
# handlers("progress")


# Reading In Data -------------------------------------------------------
# 1050 columns
amazon_train <- vroom("amazon-employee-access-challenge/train.csv") %>%
  mutate(ACTION = factor(ACTION))
amazon_test <- vroom("amazon-employee-access-challenge/test.csv")

# Cleaning Data ---------------------------------------------------------

amazon_cleanup_recipe <- recipe(ACTION ~ .,
                                data = amazon_train) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>%
  step_other(all_factor_predictors(), threshold = 0.0001) %>% # other: <4 obs
  step_upsample() %>%
  step_lencode_mixed(all_factor_predictors(), outcome = vars(ACTION)) %>%
  step_range(all_numeric_predictors(), min = 0, max = 1) #helps with computation
# step_dummy(all_factor_predictors()) %>%
# step_normalize(all_numeric_predictors())
# step_pca(all_predictors(), num_comp = 50)

# # This code is used to ensure that the above recipe worked as intended.
# amazon_train_clean <- bake(prep(amazon_cleanup_recipe),
#      new_data = amazon_train)
# # summary(amazon_train_clean)
# View(amazon_train_clean)

# # EDA -------------------------------------------------------------------
# 
# # getting an introductory view to our data
# DataExplorer::plot_intro(amazon_train_clean)
# 
# # checking for missing values
# DataExplorer::plot_missing(amazon_train_clean)
# # there are none!
# 
# # looking at distribution of yeses and no's
# ggplot(data = amazon_train_clean, mapping = aes(y = ROLE_FAMILY,
#                                                 fill = factor(ACTION))) +
#   geom_bar()
# 
# # looking at number of unique values of each column
# num_unique <- function(x){
#   return(length(unique(x)))
# }
# 
# amazon_train_clean %>%
#   apply(X = ., MARGIN = 2, FUN = num_unique) %>%
#   cbind(names(amazon_train_clean)) %>%
#   as.data.frame(.) %>%
#   ggplot(mapping = aes(x= as.numeric(.),
#                        y = V2)) +
#   geom_col(orientation = "y") +
#   labs(title = "Number of unique values per feature",
#        x = "Unique values",
#        y = "Feature")


# # Logistic Regression Model ---------------------------------------------
# # setting model
# amazon_logistic_regression_model <- logistic_reg() %>%
#   set_engine("glm")
# 
# amazon_workflow <- workflow() %>%
#   add_recipe(amazon_cleanup_recipe) %>%
#   add_model(amazon_logistic_regression_model) %>%
#   fit(data = amazon_train)

# # Penalized logistic regression model -------------------------------------
# amazon_penalized_logistic_model <- logistic_reg(mixture = tune(),
#                                                 penalty = tune()) %>%
#   set_engine("glmnet")
# 
# amazon_preliminary_workflow <- workflow() %>%
#   add_recipe(amazon_cleanup_recipe) %>%
#   add_model(amazon_penalized_logistic_model)

# Random Forests Model --------------------------------------------------

# amazon_random_forest_model <- rand_forest(mtry = tune(),
#                                           min_n = tune(),
#                                           trees = tune()) %>%
#   set_engine("ranger") %>%
#   set_mode("classification")
# 
# amazon_preliminary_workflow <- workflow() %>%
#   add_recipe(amazon_cleanup_recipe) %>%
#   add_model(amazon_random_forest_model)

# # K-Nearest Neighbors Model ---------------------------------------------
# 
# amazon_knn_model <- nearest_neighbor(neighbors = tune()) %>%
#   set_engine("kknn") %>%
#   set_mode("classification")
# 
# amazon_preliminary_workflow <- workflow() %>%
#   add_recipe(amazon_cleanup_recipe) %>%
#   add_model(amazon_knn_model)

# # Naive Bayes Model -----------------------------------------------------
# 
# amazon_naive_bayes_model <- naive_Bayes(Laplace = tune(),
#                                         smoothness = tune()) %>%
#   set_mode("classification") %>%
#   set_engine("naivebayes")
# 
# 
# amazon_preliminary_workflow <- workflow() %>%
#   add_recipe(amazon_cleanup_recipe) %>%
#   add_model(amazon_naive_bayes_model)

# # ANN Model -------------------------------------------------------------
# 
# amazon_ann_model <- mlp(hidden_units = tune(),
#                         epochs = 50) %>%
#   set_engine("keras") %>%
#   set_mode("classification")
# 
# amazon_preliminary_workflow <- workflow() %>%
#   add_recipe(amazon_cleanup_recipe) %>%
#   add_model(amazon_ann_model)

# # SVM Model -------------------------------------------------------------
# amazon_svm_radial_model <- svm_rbf(cost = tune(),
#                                   # degree = tune()
#                                   rbf_sigma = tune()
# ) %>%
#   set_mode("classification") %>%
#   set_engine("kernlab")
# 
# amazon_preliminary_workflow <- workflow() %>%
#   add_recipe(amazon_cleanup_recipe) %>%
#   add_model(amazon_svm_radial_model)

# Making a bart-boosted tree model --------------------------------------

amazon_trees_bart <- parsnip::bart(trees = tune()) %>%
  set_engine("dbarts") %>%
  set_mode("classification")

# preliminary workflow
amazon_preliminary_workflow <- workflow() %>%
  add_recipe(amazon_cleanup_recipe) %>%
  add_model(amazon_trees_bart)

# Cross-validation ------------------------------------------------------
# grid of tuning parameters
tuning_grid <- grid_space_filling(
                                  # cost(),
                                  # rbf_sigma(),
                                  # degree(),
                                  #hidden_units(range = c(5, 55)),
                                  # Laplace(range = c(0, 10)),
                                  # smoothness(range = c(0.1, 3.1)),
                                  # neighbors(),
                                  # mtry(range = c(1, dim(amazon_train)[2] - 1)),
                                  # min_n(),
                                  trees(),
                                  # penalty(),
                                  # mixture(),
                                  # levels = 3,
                                  size = 5)

# splitting data into folds
folds <- vfold_cv(amazon_train,
                  v = 3,
                  repeats = 1)


# Without progress handler
cv_results <- amazon_preliminary_workflow %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(roc_auc))

# # # cross-validation with progress handler
# # with_progress({
# #
# #   p <- progressor(steps = length(folds$splits))
# #
# #   cv_results <- amazon_preliminary_workflow %>%
# #     tune_grid(resamples = folds,
# #               grid = tuning_grid,
# #               metrics = metric_set(roc_auc),
# #               control = control_grid(
# #                 extract = function(x) {
# #                   p(message = "Fold complete")
# #                 }
# #               ))
# # })
#
# # cv_results %>%
# #   collect_metrics() %>%
# #   filter(.metric == "roc_auc") %>%
# #   ggplot(aes(x = hidden_units, y = mean)) +
# #   geom_line()

# pulling off best tuning parameter values
best_tuning_parameters <- cv_results %>%
  select_best(metric = "roc_auc")

best_tuning_parameters

# # using saved tuning parameters
# best_tuning_parameters <- vroom("naiveBayesBestTune.csv")

# Making final workflow -------------------------------------------------
# making final workflow
amazon_workflow <- amazon_preliminary_workflow %>%
  finalize_workflow(best_tuning_parameters) %>%
  fit(data = amazon_train)

# Making Predictions ----------------------------------------------------
amazon_predictions <- predict(amazon_workflow,
                              new_data = amazon_test,
                              type = "prob")

# format the predictions as a submission file
amazon_predictions_formatted <- amazon_predictions %>%
  mutate(id = 1:length(.pred_1)) %>%
  select(id, .pred_1) %>%
  rename(ACTION = .pred_1)

# Writing submission file -----------------------------------------------

vroom_write(x = amazon_predictions_formatted,
            file = "./preds.csv",
            delim = ",")

vroom_write(x = best_tuning_parameters,
            file = "./cv_bart_forests.csv",
            delim = ",")