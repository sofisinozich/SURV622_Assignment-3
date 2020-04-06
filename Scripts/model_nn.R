### Neural networks
library(tidyverse)
library(caret)
library(magrittr)

trainset <- read_rds("Data/train_set.rds")

# trainset %<>% filter(!is.na(code)) %>% select(-status_id,-coder) %>% sample_frac(.8)

trainset %<>% mutate(code = factor(code, levels = c(1,2,3,4),
                                   labels = c("Positive", "Neutral", "Negative", "Irrelevant")))


# Model testing -----------------------------------------------------------

cv3<-trainControl(method = "repeatedcv", 
                  repeats = 3,
                  classProbs = TRUE,
                  allowParallel = TRUE,
                  summaryFunction = multiClassSummary)

cv_folds <- createFolds(trainset[['code']],
                        k = 5,
                        returnTrain = TRUE)

cv <- trainControl(method = "cv",
                   number = 5,
                   index = cv_folds,
                   summaryFunction = multiClassSummary,
                   classProbs = TRUE,
                   allowParallel = TRUE,
                   savePredictions = "final")


model_nn <- train(code ~ .,
                  trainset,
                  method="nnet",
                  type ="classification", 
                  metric = "Accuracy",
                  maximize = TRUE,
                  linout=TRUE,
                  tuneGrid=expand.grid(size=1:3, decay = c(0, .001, .01, .1)),
                  trControl = cv3)


model_nn_pca2 <- train(code ~ .,
                       trainset,
                       method="nnet",
                       type ="classification", 
                       metric = "Accuracy",
                       maximize = TRUE,
                       linout=TRUE,
                       tuneGrid=expand.grid(size=1:3, decay = c(0, .001, .01, .1)),
                       trControl = cv,
                       preProcess = "pca")


limitedtrialset<-trialset[,c(trialset %>% select(-code) %>% colSums >= 20,TRUE)]

model_nn_limited <- train(as.factor(code) ~ .,
                          limitedtrialset,
                          method="nnet",
                          type ="classification", 
                          metric = "Accuracy",
                          maximize = TRUE,
                          linout=TRUE,
                          tuneGrid=expand.grid(size=1:3, decay = c(0, .001, .01, .1)),
                          trControl = cv3)


model_nn_limited_pca <- train(as.factor(code) ~ .,
                              limitedtrialset,
                              method="nnet",
                              type ="classification", 
                              metric = "Accuracy",
                              maximize = TRUE,
                              linout=TRUE,
                              tuneGrid=expand.grid(size=1:3, decay = c(0, .001, .01, .1)),
                              trControl = cv3,
                              preProcess = "pca")

model_nn_limited_lgocv <- train(as.factor(code) ~ .,
                                limitedtrialset,
                                method="nnet",
                                type ="classification", 
                                metric = "Accuracy",
                                maximize = TRUE,
                                linout=TRUE,
                                tuneGrid=expand.grid(size=1:3, decay = c(0, .001, .01, .1)),
                                trControl = trainControl(method="LGOCV"))


# Final model -------------------------------------------------------------

# Best model
model_nn_pca <- train(code ~ .,
                      trainset,
                      method="nnet",
                      type ="classification", 
                      metric = "Accuracy",
                      maximize = TRUE,
                      linout=TRUE,
                      tuneGrid=expand.grid(size=3:5, decay = c(.001, .01, .1)),
                      trControl = cv3,
                      preProcess = "pca")

test_set <- read_rds("Data/test_set.rds") %>%
  # Change outcome variable to a factor
  mutate(code = factor(code, levels = c(1,2,3,4),
                       labels = c("Positive", "Neutral", "Negative", "Irrelevant")))

test_set_predictions <- 
  bind_cols(actual=test_set$code,
            predicted=predict.train(model_nn_pca,newdata=test_set),
            predict.train(model_nn_pca,newdata=test_set, type ="prob")) %>% 
  mutate(correct=actual==predicted)

outcomes <- test_set_predictions %>% group_by(actual,correct) %>% count() %>% 
  group_by(actual) %>% mutate(percent=n/sum(n))

outcomes2 <- test_set_predictions %>% group_by(predicted,actual) %>% count()

# Set up a little table to keep everything
metrics <- tibble(accuracy = 0,
                  precision_pos = 0, precision_neu = 0, precision_neg = 0, precision_irr = 0,
                  recall_pos = 0, recall_neu = 0, recall_neg = 0, recall_irr = 0)

metrics$accuracy<-
  outcomes %>% group_by(correct) %>% summarize(n=sum(n)) %>% mutate(percent=n/sum(n)) %>% extract(2,3) %>% pull

metrics[1,2:5]<-
  outcomes2 %>% mutate(correct = predicted==actual) %>% group_by(predicted) %>% mutate(percent = n/sum(n)) %>% filter(correct == TRUE) %>% select(percent) %>% t %>% extract(2,1:4) %>% as.numeric

metrics[1,6:9] <- outcomes2 %>% group_by(actual) %>% mutate(percent = n/sum(n)) %>% filter(predicted==actual) %>% select(percent) %>% t %>% extract(2,1:4) %>% as.numeric()
