# Setup -----------------------------------------------------------------
# # clearing everything else
# rm(list = ls())

# loading in libraries
library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(janitor)
library(ggmosaic)

# Reading In Data -------------------------------------------------------
# 1050 columns
amazon_train <- vroom("amazon-employee-access-challenge/train.csv")
amazon_test <- vroom("amazon-employee-access-challenge/test.csv")

# Cleaning Data ---------------------------------------------------------

amazon_cleanup_recipe <- recipe(ACTION ~ .,
                                data = amazon_train) %>%
  step_mutate_at(all_predictors(), fn = factor) %>%
  step_other(all_factor_predictors(), threshold = 0.001) #%>%
  # step_dummy(all_factor_predictors())
  
amazon_train_clean <- bake(prep(amazon_cleanup_recipe),
     new_data = amazon_train)
summary(amazon_train_clean)

# EDA -------------------------------------------------------------------

# getting an introductory view to our data
DataExplorer::plot_intro(amazon_train_clean)

# checking for missing values
DataExplorer::plot_missing(amazon_train_clean)
# there are none!

# looking at distribution of yeses and no's
ggplot(data = amazon_train_clean, mapping = aes(y = ROLE_FAMILY,
                                                fill = factor(ACTION))) +
  geom_bar()

num_unique <- function(x){
  return(length(unique(x)))
}

names(amazon_train_clean)
# looking at number of unique values of each column
uniques <- amazon_train_clean %>%
  apply(X = ., MARGIN = 2, FUN = num_unique) %>%
  cbind(names(amazon_train_clean)) %>%
  as.data.frame(.) %>%
  ggplot(mapping = aes(y = .,
                       x = V2)) +
  geom_col()
uniques
