library(magrittr)
library(tidyverse)
library(caret)
library(class)

#Load data 
test_all <- readRDS("Data/test_set.rds")
train_all <- readRDS("Data/train_set.rds")

#Leave in only predictors and outcome (removing coder and ID#)
test <- 
  test_all %>%
  select(-c("coder", "status_id"))
  
  train <- 
    train_all %>%
  select(-c("coder", "status_id"))

#Build model
#Used 25 as square root of number of obs (rule of thumb value)
#Also ran 1 for comparison

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
