# moyenne de pred.xgboost et pred.rf
head(pred.xgboost)
head(pred.rf)

source('~/datascience/challenges/telstra/utils.R') # utility functions
source('~/datascience/challenges/telstra/base.R') # read files and feature engineering
source('~/datascience/challenges/telstra/xgboost.R') # provides pred.xgboost
source('~/datascience/challenges/telstra/randomForest.R') # proivides pred.rf

# on construit un model blend sur fold3


combinaisons <- expand.grid(
  alpha0 = seq(0,1,by=0.1),
  alpha1 = seq(0,1,by=0.1),
  alpha2 = seq(0,1,by=0.1),
  beta0 = seq(0,1,by=0.1),
  beta1 = seq(0,1,by=0.1),
  beta2 = seq(0,1,by=0.1)
)
combinaisons$s0 <- combinaisons$alpha0 + combinaisons$beta0
combinaisons$s1 <- combinaisons$alpha1 + combinaisons$beta1
combinaisons$s2 <- combinaisons$alpha2 + combinaisons$beta2

combs <- combinaisons[which(combinaisons$s0 == 1 & combinaisons$s1 == 1 & combinaisons$s2 == 1),]
View(combs)

computeLogLoss <- function(row){
  alpha0 <- row[1]
  alpha1 <- row[2]
  alpha2 <- row[3]
  beta0 <- row[4]
  beta1 <- row[5]
  beta2 <- row[6]
  output.ens <- data.frame(
    #id = test$id,
    X0 = alpha0*pred.xgboost[,1]+ beta0*pred.rf[, 1],
    X1 = alpha1*pred.xgboost[,2]+ beta1*pred.rf[, 2],
    X2 = alpha2*pred.xgboost[,3]+ beta2*pred.rf[, 3],
    obs = paste0("X",ytrain[folds$Fold3])
  )
  return(mnLogLoss(output.ens, lev = levels(output.ens$obs)))
  
}

loglosses <- base::apply(combs, 1, FUN = computeLogLoss)


bestComb <- combs[which.min(loglosses),]
output.ens <- data.frame(
  #id = test$id,
  X0 = 0.8*pred.xgboost[,1]+ 0.2*pred.rf[, 1],
  X1 = 0.4*pred.xgboost[,2]+ 0.6*pred.rf[, 2],
  X2 = 0.5*pred.xgboost[,3]+ 0.5*pred.rf[, 3],
  obs = paste0("X",ytrain[folds$Fold3])
)
mnLogLoss(output.ens, lev = levels(output.ens$obs))

#write.csv(output.ens, paste(sep = "-", format(Sys.time(), "%Y%m%d.%H%M"), "ens-submission.csv"), row.names = F, quote = F)
