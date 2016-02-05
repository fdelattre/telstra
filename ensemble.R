source('~/datascience/challenges/telstra/utils.R') # utility functions
source('~/datascience/challenges/telstra/base.R') # read files and feature engineering
source('~/datascience/challenges/telstra/xgboost.R') # provides pred.xgboost
source('~/datascience/challenges/telstra/randomForest.R') # provides pred.rf

# on construit un model blend sur fold3


output.ens <- data.frame(
  id = test$id,
  X0 = 0.8*pred.xgboost[,1]+ 0.2*pred.rf[, 1],
  X1 = 0.4*pred.xgboost[,2]+ 0.6*pred.rf[, 2],
  X2 = 0.5*pred.xgboost[,3]+ 0.5*pred.rf[, 3]
)


write.csv(output.ens, paste(sep = "-", format(Sys.time(), "%Y%m%d.%H%M"), "ens-submission.csv"), row.names = F, quote = F)
