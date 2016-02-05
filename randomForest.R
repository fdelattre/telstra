# svm
library(caret)

source('~/datascience/challenges/telstra/base.R')
source('~/datascience/challenges/telstra/utils.R')

genererSubmission <- F

# x <- xtrain[folds$Fold2,]
# y <- as.factor(paste0("X",ytrain[folds$Fold2]))

x <- xtrain
y <- as.factor(paste0("X",ytrain))

tr <- trainControl(
  method = "cv", 
  number = 3, 
  classProbs = TRUE, 
  summaryFunction = mc_logloss,
  allowParallel = T)

rf_model <- caret::train(x, y, 
                          method = "rf", 
                          metric = "mlogloss",
                          maximize = F,
                          trControl = tr,
                          strata = c(300, 300, 300),
                          ntree = 800)

notify_android(
  event = "Random Forest Model finished", 
  msg = paste("Minimal CV mlogloss : ", min(rf_model$results$mlogloss)))

# Matrice de confusion sur le fold2
# xval <- xtrain[folds$Fold1,]
# yval <- as.factor(paste0("X",ytrain[folds$Fold1]))
# pred.rf.class <- predict(rf_model, xval)
# confusionMatrix(yval, pred.rf.class)

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
