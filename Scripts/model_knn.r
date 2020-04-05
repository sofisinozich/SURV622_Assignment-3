library(magrittr)
library(tidyverse)
library(caret)
library(class)

# Load data 
test_all <- readRDS("Data/test_set.rds")
train_all <- readRDS("Data/train_set.rds")

  # Change outcome variable to a factor
  train_all %<>%
  mutate(code = factor(code, levels = c(1,2,3,4),
                       labels = c("Positive", "Neutral", "Negative", "Irrelevant")))
  test_all %<>%
    mutate(code = factor(code, levels = c(1,2,3,4),
                         labels = c("Positive", "Neutral", "Negative", "Irrelevant")))

  # Fix problematic column names
    colnames(train_all)[colnames(train_all) == "\U0001f3c0"] <- "U0001f3c0"
    colnames(train_all)[colnames(train_all) == "\U0001f3c6"] <- "U0001f3c6"
    colnames(train_all)[colnames(train_all) == "\U0001f422"] <- "U0001f422"
    
    colnames(test_all)[colnames(test_all) == "\U0001f3c0"] <- "U0001f3c0"
    colnames(test_all)[colnames(test_all) == "\U0001f3c6"] <- "U0001f3c6"
    colnames(test_all)[colnames(test_all) == "\U0001f422"] <- "U0001f422"

  # Leave in only predictors and outcome (removing coder and ID#)
    test <- 
      test_all %>%
      select(-c("coder", "status_id"))
      
      train <- 
        train_all %>%
      select(-c("coder", "status_id"))

# Build model
# Used 25 as square root of number of obs (rule of thumb value)
# Also ran 1 for comparison
  cv_folds <- createFolds(train[['code']],
                          k = 5,
                          returnTrain = TRUE)
  
  tc_settings <- trainControl(method = "cv",
                              number = 5,
                              index = cv_folds,
                              summaryFunction = multiClassSummary,
                              classProbs = TRUE,
                              allowParallel = TRUE,
                              savePredictions = "final")
  
  trained_model <- train(form = code ~ .,
                         data = train,
                         method = "knn",
                         metric = 'Accuracy',
                         trControl = tc_settings,
                         tuneLength = 10)

knn_25 <- knn(train %>% select(-code),
              test %>% select(-code),
              cl = train$code,
              k =25, l = 0, prob=TRUE)
knn_1 <- knn(train %>% select(-code),
             test %>% select(-code),
             cl = train$code,
             k =1, l = 0, prob=TRUE)


#Confusion matrix
cm25 <- table(knn_25,test$code)
cm1 <- table(knn_1, test$code)

#Accuracy
accuracy_25 <-sum(diag(cm25)/(sum(rowSums(cm25))))
accuracy_25

accuracy_1 <-sum(diag(cm1)/(sum(rowSums(cm1))))
accuracy_1

# Get performance metrics

    train_set_predictions <- data.frame(
      actual = train$code,
      predicted = caret::predict.train(trained_model, newdata = train)
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
    test_set_predictions <- caret::predict.train(trained_model, newdata = test)
    
    test_counts <- data.frame(actual = test$code, predicted = test_set_predictions) %>%
      count(actual, predicted)
  
  # Overall accuracy
    test_set_accuracy <- divide_by(test_counts %>% filter(actual == predicted) %>% pull('n') %>% sum(),
                                   test_counts %>% pull('n') %>% sum())
  
  # Sensitivities
    test_counts %>%
      group_by(actual) %>%
      mutate(pct_of_actual = n/sum(n)) %>%
      filter(actual == predicted) %>%
      ungroup()