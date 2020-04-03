library(magrittr)
library(tidyverse)
library(withr)

library(caret)
library(randomForest)
library(ranger)

# Load training/testing sets and process so that the random forest methods from the 'ranger' package can be used
  train_set <- readRDS("Data/train_set.rds") %>%
    # Change outcome variable to a factor
    mutate(code = factor(code, levels = c(1,2,3,4),
                         labels = c("Positive", "Neutral", "Negative", "Irrelevant")))
  
  colnames(train_set)[colnames(train_set) == "\U0001f3c0"] <- "U0001f3c0"
  colnames(train_set)[colnames(train_set) == "\U0001f3c6"] <- "U0001f3c6"
  colnames(train_set)[colnames(train_set) == "\U0001f422"] <- "U0001f422"

  
  
  test_set <- readRDS("Data/test_set.rds") %>%
    # Change outcome variable to a factor
    mutate(code = factor(code, levels = c(1,2,3,4),
                         labels = c("Positive", "Neutral", "Negative", "Irrelevant")))
  
  colnames(test_set)[colnames(test_set) == "\U0001f3c0"] <- "U0001f3c0"
  colnames(test_set)[colnames(test_set) == "\U0001f3c6"] <- "U0001f3c6"
  colnames(test_set)[colnames(test_set) == "\U0001f422"] <- "U0001f422"
  
# Establish method for cross-validation tuning
  cv_folds <- createFolds(train_set[['code']],
                          k = 5,
                          returnTrain = TRUE)
  
  tc_settings <- trainControl(method = "cv",
                              number = 5,
                              index = cv_folds,
                              summaryFunction = multiClassSummary,
                              classProbs = TRUE,
                              allowParallel = TRUE,
                              savePredictions = "final")

# Set grid of tuning parameters

  nvars <- ncol(
    model.matrix(object = code ~ . -1 -coder -status_id,
                 data = train_set)
  )
  
  default_mtry <- floor(sqrt(nvars))

  rf_tuning_param_grid <- expand.grid(
    mtry = seq(from = pmax(default_mtry - 5, 1),
               to = pmin(default_mtry + 5, nvars),
               by = 1),
    splitrule = c("gini"),
    min.node.size = seq(10, 30, by = 5)
  )

# Use 5-fold cross validation to select tuning parameter values
  trained_model <- caret::train(
    form = code ~ . -1 -coder -status_id,
    data = train_set,
    method = 'ranger',
    metric = 'Accuracy',
    tuneGrid = rf_tuning_param_grid,
    trControl = tc_settings,
    num.threads = 3
  )
  
  # Get performance metrics
  
  train_set_predictions <- data.frame(
    actual = train_set$code,
    predicted = caret::predict.train(trained_model, newdata = train_set)
  )
  
  train_set_counts <- train_set_predictions %>%
    count(actual, predicted)
    
  # Overall accuracy
  train_set_accuracy <- divide_by(train_set_counts %>% filter(actual == predicted) %>% pull('n') %>% sum(),
                                  train_set_counts %>% pull('n') %>% sum())
  
  # Sensitivities
  train_set_counts %>%
    group_by(actual) %>%
    mutate(sensitivity = n/sum(n)) %>%
    filter(actual == predicted) %>%
    ungroup()
  
# Get test set predictions
  test_set_predictions <- caret::predict.train(trained_model, newdata = test_set)
  
  test_counts <- data.frame(actual = test_set$code, predicted = test_set_predictions) %>%
    count(actual, predicted)
  
  test_counts %>%
    group_by(actual) %>%
    mutate(pct_of_actual = n/sum(n)) %>%
    filter(actual == predicted) %>%
    ungroup()
  
  # Overall accuracy
  divide_by(test_counts %>% filter(actual == predicted) %>% pull('n') %>% sum(),
            test_counts %>% pull('n') %>% sum())
  
  # Sensitivies
  test_counts %>%
    summarize(accuracy = mean(actual == predicted))
  