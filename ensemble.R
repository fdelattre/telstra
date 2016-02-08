onFold <- F

source('~/datascience/challenges/telstra/utils.R') # utility functions
source('~/datascience/challenges/telstra/base.R') # read files and feature engineering
source('~/datascience/challenges/telstra/xgboost.R') # provides pred.xgboost either on fold or on full test set
source('~/datascience/challenges/telstra/randomForest.R') # provides pred.rf either on fold or on full test set

# on construit un model blend sur fold3

output.ens <- data.frame(
  id = test$id,
  predict_0 = 0.9*pred.xgboost[,1]+ 0.1*pred.rf[, 1],
  predict_1 = 0.9*pred.xgboost[,2]+ 0.1*pred.rf[, 2],
  predict_2 = 0.5*pred.xgboost[,3]+ 0.5*pred.rf[, 3]
)


write.csv(output.ens, paste(sep = "-", format(Sys.time(), "%Y%m%d.%H%M"), "ens-submission.csv"), row.names = F, quote = F)
