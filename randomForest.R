# svm
library(caret)

genererSubmission <- F

# x <- xtrain[folds$Fold2,]
# y <- as.factor(paste0("X",ytrain[folds$Fold2]))
# xtest <- xtrain[folds$Fold3,]
# ytest <- test.id[folds$Fold3]

x <- xtrain
y <- as.factor(paste0("X",ytrain))

tr <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = mnLogLoss,
  allowParallel = T)

rf_model <- caret::train(x, y, 
                          method = "rf", 
                          metric = "logLoss",
                          maximize = F,
                          trControl = tr,
                          ntree = 800)

notify_android(
  event = "Random Forest Model finished", 
  msg = paste("Minimal CV mlogloss : ", min(rf_model$results$logLoss)))

pred.rf <- predict(rf_model, xtest, type="prob")
output.rf <- data.frame(
  id = test.id,
  predict_0 = pred.rf[,1],
  predict_1 = pred.rf[,2],
  predict_2 = pred.rf[,3]
  )
if (genererSubmission) {
  write.csv(output.rf, paste(sep = "-", format(Sys.time(), "%Y%m%d.%H%M"), "rf-submission.csv"), row.names = F, quote = F)
}
