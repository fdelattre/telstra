source('~/datascience/challenges/telstra/base.R')
source('~/datascience/challenges/telstra/utils.R')

library(xgboost)
######################################################################################
# XGBOOST TREE
######################################################################################
set.seed(123456)
verboseXgboost <- F
genererSubmission <- F
notifyAndroid <- F
cat("Starting xgboost....\n")
dtrain <-
  xgb.DMatrix(data = xtrain, label = ytrain)

registerDoMC(cores = 6)

xgparams.tree <- list(
  objective = "multi:softprob",
  num_class = 3,
  colsample_bytree = 0.3,
  max.depth = 8,
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

cat("Best CV mlogloss : ", min(xgboost.first$test.mlogloss.mean),"\n")
if(notifyAndroid <- F){
  notify_android(
    event = "Xgboost cross validation finished",
    msg = paste("Minimum CV mlogloss : ", min(xgboost.first$test.mlogloss.mean))
  )
}

xgboost.model <- xgboost(
  data = dtrain,
  params = xgparams.tree,
  nrounds = which.min(xgboost.first$test.mlogloss.mean),
  verbose = verboseXgboost
)
imp <- xgb.importance(model = xgboost.model)
bestcols <- names(train.wide)[as.numeric(imp$Feature)+1]

if (genererSubmission) {
  
  cat("Generating Submission...\n")
  pred.xgboost <- matrix(xgboost::predict(xgboost.model, xtest), ncol = 3, byrow = T)
  
  output.xgboost <- data.frame(
    id = test.id,
    predict_0 = pred.xgboost[,1],
    predict_1 = pred.xgboost[,2],
    predict_2 = pred.xgboost[,3]
  )
  write.csv(output.xgboost, paste(sep = "-", format(Sys.time(), "%Y%m%d.%H%M"), "submission.csv"), row.names = F, quote = F)
}