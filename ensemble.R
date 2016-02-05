# moyenne de pred.xgboost et pred.rf
head(pred.xgboost)
head(pred.rf)

output.ens <- data.frame(
  id = test$id,
  predict_0 = 1*pred.xgboost[,1]+ 0*pred.rf[, 1],
  predict_1 = 1*pred.xgboost[,2]+ 0*pred.rf[, 2],
  predict_2 = 0.3*pred.xgboost[,3]+ 0.7*pred.rf[, 3]
)

write.csv(output.ens, paste(sep = "-", format(Sys.time(), "%Y%m%d.%H%M"), "ens-submission.csv"), row.names = F, quote = F)
