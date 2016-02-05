# svm

source('~/datascience/challenges/telstra/base.R')
source('~/datascience/challenges/telstra/utils.R')

x <- xtrain
y <- as.factor(paste0("X",ytrain))

tr <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = mnlogloss,
  allowParallel = T)

tgr <- expand.grid(
  C = seq(5000,10000,1000),
  sigma = 1e-5
)

svm_model <- caret::train(x, y, 
                          method = "svmRadial", 
                          metric = "logLoss",
                          maximize = F,
                          trControl = tr,
                          tuneGrid = tgr)

notify_android(
  event = "SVM Model finished", 
  msg = paste("Minimal CV mlogloss : ", min(svm_model$results$logLoss)))

# Matrice de confusion sur le fold2
xval <- xtrain[folds$Fold1,]
yval <- as.factor(paste0("X",ytrain[folds$Fold1]))
pred.rf.class <- predict(rf_model, xval)
confusionMatrix(yval, pred.rf.class)

pred.svm <- predict(svm_model, xtest, type = "prob")

output.svm <- data.frame(
  id = test.id,
  predict_0 = pred.svm[,1],
  predict_1 = pred.svm[,2],
  predict_2 = pred.svm[,3]
)



