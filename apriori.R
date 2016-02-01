
# association rules
library(arules)
library(data.table)
library(magrittr)
library(stringr)

makeReadable <- function(x){
  num <- sapply(strsplit(x, " "), `[`, 2)
  c <- str_sub(x, 1, 1)
  return(paste0(c,num))}

makeNumeric <- function(x){
  num <- sapply(strsplit(x, " "), `[`, 2)
  return(as.numeric(num))
}

train <- fread("~/datascience/challenges/telstra/data/train.csv")%>%setkey(id)
test <- fread("~/datascience/challenges/telstra/data/test.csv")%>%setkey(id)
sample_submission <- fread("~/datascience/challenges/telstra/data/sample_submission.csv")%>%setkey(id)
log_feature <- fread("~/datascience/challenges/telstra/data/log_feature.csv")%>%setkey(id)
event_type <- fread("~/datascience/challenges/telstra/data/event_type.csv")%>%setkey(id)
resource_type <- fread("~/datascience/challenges/telstra/data/resource_type.csv")%>%setkey(id)
severity_type <- fread("~/datascience/challenges/telstra/data/severity_type.csv")%>%setkey(id)

log_feature[, log_feature := makeReadable(log_feature)]
event_type[, event_type := makeReadable(event_type)]
resource_type[, resource_type := makeReadable(resource_type)]
severity_type[, severity_type := makeReadable(severity_type)]
train[, ":="(location = makeReadable(location),fault_severity = paste0("fs", fault_severity))]
test[, location := makeReadable(location)]


joined_train <- log_feature[severity_type][resource_type][event_type, allow.cartesian = TRUE][train]
joined_test <- log_feature[severity_type][resource_type][event_type, allow.cartesian = TRUE][test]


trainC0 <- joined_train[fault_severity == "fs0", lapply(.SD, as.factor), .SDcols = c("id", "log_feature","severity_type","resource_type","event_type","fault_severity", "location")]
trainC1 <- joined_train[fault_severity == "fs1", lapply(.SD, as.factor), .SDcols = c("id", "log_feature","severity_type","resource_type","event_type","fault_severity", "location")]
trainC2 <- joined_train[fault_severity == "fs2", lapply(.SD, as.factor), .SDcols = c("id", "log_feature","severity_type","resource_type","event_type","fault_severity", "location")]

#objectif : transformer les jeux de données en transactions :
# ID  Liste
# 1   "location 1" "event_type 2" "event_type 5" "log_feature 123" .... "fault_severity 2"
# 2   "location 65" "event_type 8" "event_type 11" "log_feature 51" .... "fault_severity 0"
lf_list <- log_feature[,.(rep(log_feature, volume)), by = id][, .(list(V1)), by = id]
lf_list <- log_feature[, .(list(log_feature)), by = id]
et_list <- event_type[, .(list(event_type)), by = id]
rt_list <- resource_type[, .(list(resource_type)), by = id]
listed <- lf_list[et_list][rt_list][train]
#listed <- lf_list[train]

trainC0 <- listed[fault_severity == "fs0"][, .(unlist(.SD)), by = id, .SDcols = c("V1", "i.V1", "i.V1.1", "location", "fault_severity")][,V1:=as.factor(V1)]
trainC1 <- listed[fault_severity == "fs1"][, .(unlist(.SD)), by = id, .SDcols = c("V1", "i.V1", "i.V1.1", "location", "fault_severity")][,V1:=as.factor(V1)]
trainC2 <- listed[fault_severity == "fs2"][, .(unlist(.SD)), by = id, .SDcols = c("V1", "i.V1", "i.V1.1", "location", "fault_severity")][,V1:=as.factor(V1)]

# trainC0 <- listed[fault_severity == "fs0"][, .(unlist(.SD)), by = id, .SDcols = c("V1", "location", "fault_severity")][,V1:=as.factor(V1)]
# trainC1 <- listed[fault_severity == "fs1"][, .(unlist(.SD)), by = id, .SDcols = c("V1", "location", "fault_severity")][,V1:=as.factor(V1)]
# trainC2 <- listed[fault_severity == "fs2"][, .(unlist(.SD)), by = id, .SDcols = c("V1", "location", "fault_severity")][,V1:=as.factor(V1)]


C0 <- as(split(trainC0[,V1], trainC0[,id]), "transactions")
C1 <- as(split(trainC1[,V1], trainC1[,id]), "transactions")
C2 <- as(split(trainC2[,V1], trainC2[,id]), "transactions")

rulesC0 <- apriori(C0,
                 appearance = list(rhs="fs0", default="lhs"))

rulesC1 <- apriori(C1,
                   appearance = list(rhs="fs1", default="lhs"))

rulesC2 <- apriori(C2,
                   appearance = list(rhs="fs2", default="lhs"))



# règles identifiées
# on ne retrouve jamais location ni severity type parmi les plus hauts supports

# on garde toutes les event_types (53)
# on garde toutes les resources_types (10)
# on garde les features suivantes (cf algorithme apriori)
features_to_keep <- c("f307","f315","f233","f313","f232","f312","f68","f54","f170","f203","f82","f201","f80","f193","f71" )

locations_to_keep

trainC0_location <- listed[fault_severity == "fs0"][, .(unlist(.SD)), by = id, .SDcols = c("location", "i.V1.1", "fault_severity")][,V1:=as.factor(V1)]
C0_location <- as(split(trainC0_location[,V1], trainC0_location[,id]), "transactions")
rulesC0_location <- apriori(C0_location)

