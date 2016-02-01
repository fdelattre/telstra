# calcul du lasso
library(glmnet)
X <- train.set.mat[,-1]
Y <- ifelse(train.wide$fault_severity == 0, 0, 1)
Y2 <- train.wide$fault_severity

glm_binomial <- glmnet(X, Y, family = "binomial", intercept = F)
glm_multi <- glmnet(X, Y2, family = "multinomial", intercept = F)

cv <- cv.glmnet(X,Y, family = "binomial")
cf <- coef(cv, s = "lambda.1se")
inds<-which(cf!=0)
variables<-row.names(cf)[inds]
variables<-variables[!variables %in% '(Intercept)']
