### Neural networks

library(caret)

trialset <- tweet_features %>% filter(!is.na(code)) %>% select(-status_id,-coder) %>% sample_frac(.8)

cv3<-trainControl(method = "repeatedcv", repeats = 3)

model_nn <- train(as.factor(code) ~ .,
                  trialset,
                  method="nnet",
                  type ="classification", 
                  metric = "Accuracy",
                  maximize = TRUE,
                  linout=TRUE,
                  tuneGrid=expand.grid(size=1:3, decay = c(0, .001, .01, .1)),
                  trControl = cv3)


model_nn_pca <- train(as.factor(code) ~ . - status_id,
                  trialset,
                  method="nnet",
                  type ="classification", 
                  metric = "Accuracy",
                  maximize = TRUE,
                  linout=TRUE,
                  tuneGrid=expand.grid(size=1:3, decay = c(0, .001, .01, .1)),
                  trControl = cv3,
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

# Somehow these are all just getting worse...
