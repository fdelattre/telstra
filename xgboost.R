library(xgboost)

set.seed(123456)
verboseXgboost <- F
genererSubmission <- F
notifyAndroid <- T
CVonly <- F
importance <- F
cat("Starting xgboost....\n")

# dtrain <- xgb.DMatrix(data = xtrain[folds$Fold1,], label = ytrain[folds$Fold1])
# xtest <- xtrain[folds$Fold3,]
# ytest <- test.id[folds$Fold3]
dtrain <- xgb.DMatrix(data = xtrain, label = ytrain)

registerDoMC(cores = 6)

xgparams.tree <- list(
  objective = "multi:softprob",
  num_class = 3,
  colsample_bytree = 0.5,
  max.depth = 8,
  eta = 0.05
)

xgboost.first <- xgb.cv(
  data = dtrain,
  params = xgparams.tree,
  nrounds = 500,
  nfold = 10,
  metrics = "mlogloss",
  verbose = verboseXgboost,
  print.every.n = 200
)

cat("xVal mlogloss : ", min(xgboost.first$test.mlogloss.mean),"\n")

if(!CVonly)
{
  pred.loop <- matrix(nrow = nrow(xtest)*3, ncol = 10)
  for(index in 1:10)
  {
    set.seed(28021980+index)
    xgboost.model <- xgboost(
      data = dtrain,
      params = xgparams.tree,
      nrounds = which.min(xgboost.first$test.mlogloss.mean),
      verbose = verboseXgboost
    )
    pred.loop[,index] <- xgboost::predict(xgboost.model, xtest)
  }

  pred.xgboost <- matrix(apply(pred.loop, MARGIN = 1, mean), ncol = 3, byrow = T)
  
  output.xgboost <- data.frame(
    id = test.id,
    predict_0 = pred.xgboost[,1],
    predict_1 = pred.xgboost[,2],
    predict_2 = pred.xgboost[,3]
  )

if(importance){
  writeLines("Computing importance...")
  imp <- xgb.importance(feature_names = names(xtrain), model = xgboost.model)
  }
  
  if (genererSubmission) {
    cat("Generating Submission...\n")
    write.csv(output.xgboost, paste(sep = "-", format(Sys.time(), "%Y%m%d.%H%M"), "submission.csv"), row.names = F, quote = F)
  }
}

if(notifyAndroid){
  notify_android(
    event = "Xgboost cross validation finished",
    msg = paste("Minimum xVal mlogloss : ", min(xgboost.first$test.mlogloss.mean))
  )
}