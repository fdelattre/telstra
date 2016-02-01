# svm
library(caret)

#source('~/datascience/challenges/telstra/base.R')

x <- train.set.mat
y <- as.factor(paste0("X",train.wide$fault_severity))

tr <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = mc_logloss,
  allowParallel = T)

tgr <- expand.grid(
  C = seq(30, 50, by = 5),
  sigma = 1e-5
)

svm_model <- caret::train(x, y, 
                          method = "svmRadial", 
                          metric = "mlogloss",
                          maximize = F,
                          trControl = tr,
                          tuneGrid = tgr)

notify_android(
  event = "SVM Model finished", 
  msg = paste("Minimal CV mlogloss : ", min(svm_model$results$mlogloss)))

pred.svm <- predict(svm_model, train.set.mat, type = "prob")
output.rf <- data.frame(
  id = test$id,
  predict_0 = pred[,1],
  predict_1 = pred[,2],
  predict_2 = pred[,3]
)



