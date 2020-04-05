

library(ISLR)
library(caret)
library(foreign)
library(nnet)

names(train_set)
head(train_set)
summary(train_set)

x <- train_set[,2:129]

log_model <- multinom(code ~ . - status_id - coder, data=train_set)
summary(log_model)
z <- summary(log_model)$coefficients/summary(log_model)$standard.errors
z

#predict_1 <- fitted(log_model)
#head(predict_1)

log_pred <- predict(log_model, newdata = test_set, type = "response")
table(test_set$code, log_pred > 0.5)
