source('~/datascience/challenges/telstra/base.R')
source('~/datascience/challenges/telstra/utils.R')

library(xgboost)

set.seed(123456)
verboseXgboost <- T
genererSubmission <- F
notifyAndroid <- F
CVonly <- F
cat("Starting xgboost....\n")

dtrain <- xgb.DMatrix(data = xtrain, label = ytrain)

registerDoMC(cores = 6)

xgparams.tree <- list(
  objective = "multi:softprob",
  num_class = 3,
  colsample_bytree = 0.2,
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

cat("xVal mlogloss : ", min(xgboost.first$test.mlogloss.mean),"\n")

if(notifyAndroid <- F){
  notify_android(
    event = "Xgboost cross validation finished",
    msg = paste("Minimum xVal mlogloss : ", min(xgboost.first$test.mlogloss.mean))
  )
}

if(!CVonly)
{
  xgboost.model <- xgboost(
    data = dtrain,
    params = xgparams.tree,
    nrounds = which.min(xgboost.first$test.mlogloss.mean),
    verbose = verboseXgboost
  )
  writeLines("Computing importance...")
  imp <- xgb.importance(feature_names = names(xtrain), model = xgboost.model)
  
  
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
}