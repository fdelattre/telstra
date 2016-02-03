# moyenne de pred.xgboost et pred.rf
head(pred.xgboost)
head(pred.rf)

p0 <- cbind( pred.xgboost[,1],pred.rf[, 1],pred.svm[, 1])
p1 <- cbind( pred.xgboost[,2],pred.rf[, 2],pred.svm[, 2])
p2 <- cbind( pred.xgboost[,3],pred.rf[, 3],pred.svm[, 3])

output.ens <- data.frame(
  id = test$id,
  predict_0 = 0.7*pred.xgboost[,1]+ 0.2*pred.rf[, 1] + 0.1*pred.svm[, 1],
  predict_1 = 0.7*pred.xgboost[,2]+ 0.2*pred.rf[, 2] + 0.1*pred.svm[, 2],
  predict_2 = 0.7*pred.xgboost[,3]+ 0.2*pred.rf[, 3] + 0.1*pred.svm[, 3]
)

output.ens <- data.frame(
  id = test$id,
  predict_0 = apply(p0, 1, max),
  predict_1 = apply(p1, 1, max),
  predict_2 = apply(p2, 1, max)
)
write.csv(output.ens, paste(sep = "-", format(Sys.time(), "%Y%m%d.%H%M"), "ens-submission.csv"), row.names = F, quote = F)
