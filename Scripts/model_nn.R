### Neural networks

library(caret)

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

test_set_predictions %>% group_by(actual,correct) %>% count()
test_set_predictions %>% count(correct)

divide_by(test_set_predictions %>% group_by(actual,predicted) %>% count() %>% filter(actual == predicted) %>% pull('n') %>% sum(),
          test_set_predictions %>% group_by(actual,predicted) %>% count() %>% pull('n') %>% sum())

# 56% accuracy in the end