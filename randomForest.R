# svm
library(caret)

#source('~/datascience/challenges/telstra/base.R')

genererSubmission <- F

x <- train.set.mat
y <- as.factor(paste0("X",train.wide$fault_severity))

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

if (genererSubmission) {
  pred.rf <- predict(rf_model, test.set.mat, type="prob")
  output.rf <- data.frame(
    id = test$id,
    predict_0 = pred[,1],
    predict_1 = pred[,2],
    predict_2 = pred[,3]
  )
  write.csv(output.rf, paste(sep = "-", format(Sys.time(), "%Y%m%d.%H%M"), "rf-submission.csv"), row.names = F, quote = F)
}
