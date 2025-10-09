# Setup -----------------------------------------------------------------
# clearing everything
rm(list = ls())

# loading in libraries
library(tidyverse)
library(tidymodels)
library(vroom)
# library(patchwork)
# library(janitor)

# Reading In Data -------------------------------------------------------
# 1050 columns
amazon_train <- vroom("amazon-employee-access-challenge/train.csv") %>%
  mutate(ACTION = factor(ACTION))
amazon_test <- vroom("amazon-employee-access-challenge/test.csv")

# Cleaning Data ---------------------------------------------------------
  
amazon_cleanup_recipe <- recipe(ACTION ~ .,
                                data = amazon_train) %>%
  step_mutate_at(all_numeric_predictors(), fn = factor) %>%
  step_other(all_factor_predictors(), threshold = 0.001) %>%
  step_dummy(all_factor_predictors())

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


# Logistic Regression Model ---------------------------------------------
# setting model
amazon_logistic_regression_model <- logistic_reg() %>%
  set_engine("glm")


# Making final workflow -------------------------------------------------
# making final workflow
amazon_workflow <- workflow() %>%
  add_recipe(amazon_cleanup_recipe) %>%
  add_model(amazon_logistic_regression_model) %>%
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
      file = "C:/Users/matth/OneDrive/Documents/GitHub/AmazonEmployeeAccess/preds.csv",
      delim = ",")
