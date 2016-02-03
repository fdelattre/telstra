# svm
library(caret)

source('~/datascience/challenges/telstra/base.R')
source('~/datascience/challenges/telstra/utils.R')

genererSubmission <- F

x <- xtrain
y <- as.factor(paste0("X",ytrain))

tr <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = mc_logloss,
  allowParallel = T)

tgr <- expand.grid(
  #mtry = seq(40,80, by=10)
  mtry = 260
)

rf_model <- caret::train(x, y, 
                          method = "rf", 
                          metric = "mlogloss",
                          maximize = F,
                          trControl = tr,
                          tuneGrid = tgr )

notify_android(
  event = "Random Forest Model finished", 
  msg = paste("Minimal CV mlogloss : ", min(rf_model$results$mlogloss)))


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
