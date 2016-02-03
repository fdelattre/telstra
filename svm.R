# svm

source('~/datascience/challenges/telstra/base.R')
source('~/datascience/challenges/telstra/utils.R')

x <- xtrain
y <- as.factor(paste0("X",ytrain))

tr <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = mc_logloss,
  allowParallel = T)

tgr <- expand.grid(
  C = 1e+6,
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

pred.svm <- predict(svm_model, xtest, type = "prob")

output.svm <- data.frame(
  id = test.id,
  predict_0 = pred.svm[,1],
  predict_1 = pred.svm[,2],
  predict_2 = pred.svm[,3]
)



