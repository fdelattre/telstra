#source('~/datascience/challenges/telstra/base.R')

library(xgboost)
######################################################################################
# XGBOOST TREE
######################################################################################
verboseXgboost <- F
genererSubmission <- F
cat("Starting xgboost....\n")
dtrain <-
  xgb.DMatrix(data = train.set.mat, label = train.wide$fault_severity)

registerDoMC(cores = 6)

xgparams.tree <- list(
  objective = "multi:softprob",
  num_class = 3,
  colsample_bytree = 0.4,
  eta = 0.05
)

xgboost.first <- xgb.cv(
  data = dtrain,
  params = xgparams.tree,
  nrounds = 1000,
  nfold = 10,
  metrics = "mlogloss",
  verbose = verboseXgboost,
  print.every.n = 200
)

notify_android(
  event = "xgboost cross validation finished",
  msg = paste("Minimum CV mlogloss : ", min(xgboost.first$test.mlogloss.mean))
)
cat("Best CV mlogloss : ", min(xgboost.first$test.mlogloss.mean),"\n")
xgboost.model <- xgboost(
  data = dtrain,
  params = xgparams.tree,
  nrounds = which.min(xgboost.first$test.mlogloss.mean),
  verbose = verboseXgboost
)
imp <- xgb.importance(model = xgboost.model)
bestcols <- names(train.wide)[as.numeric(imp$Feature)]

if (genererSubmission) {
  
  cat("Generating Submission...\n")
  pred.xgboost <- matrix(xgboost::predict(xgboost.model, test.set.mat), ncol = 3, byrow = T)
  
  output.xgboost <- data.frame(
    id = test$id,
    predict_0 = pred.mat[,1],
    predict_1 = pred.mat[,2],
    predict_2 = pred.mat[,3]
  )
  write.csv(output.xgboost, paste(sep = "-", format(Sys.time(), "%Y%m%d.%H%M"), "submission.csv"), row.names = F, quote = F)
}