library(data.table)
library(magrittr)
library(stringr)
library(doMC)
library(caret)

#
remove0cols <- T
writeFiles <- F
computeFeatures <- T
computeManualInteractions <- T
groupCardinalities <- T

log_feature_rarityThreshold <- 50
event_type_rarity_threshold <- 5
resource_type_rarity_threshold <- 600
severity_type_rarity_threshold <- 2000
#

source('~/datascience/challenges/telstra/utils.R')

train <-
  fread("~/datascience/challenges/telstra/data/train.csv") %>% setkey(id)
test <-
  fread("~/datascience/challenges/telstra/data/test.csv") %>% setkey(id)
log_feature <-
  fread("~/datascience/challenges/telstra/data/log_feature.csv") %>% setkey(id)
event_type <-
  fread("~/datascience/challenges/telstra/data/event_type.csv") %>% setkey(id)
resource_type <-
  fread("~/datascience/challenges/telstra/data/resource_type.csv") %>% setkey(id)
severity_type <-
  fread("~/datascience/challenges/telstra/data/severity_type.csv") %>% setkey(id)


log_feature[, ":="(numlf = makeNumeric(log_feature), log_feature = makeReadable(log_feature))]
event_type[, ":="(numet = makeNumeric(event_type), event_type = makeReadable(event_type))]
resource_type[, ":="(numrt = makeNumeric(resource_type), resource_type = makeReadable(resource_type))]
severity_type[, ":="(numst = makeNumeric(severity_type), severity_type = makeReadable(severity_type))]


train[, ":="(numloc = makeNumeric(location),
             location = makeReadable(location))]
test[, ":="(numloc = makeNumeric(location),
            location = makeReadable(location), fault_severity = -1)]

total <- rbind(train, test)%>%setkey("id")

# certaines lof features importantes :

lf203 <- log_feature[,.(lf203 = ifelse("f203"%in%log_feature,1,0)), by=id]
lf312 <- log_feature[,.(lf312 = ifelse("f312"%in%log_feature,1,0)), by=id]
lf232 <- log_feature[,.(lf232 = ifelse("f232"%in%log_feature,1,0)), by=id]
lf170 <- log_feature[,.(lf170 = ifelse("f170"%in%log_feature,1,0)), by=id]
lf313 <- log_feature[,.(lf313 = ifelse("f313"%in%log_feature,1,0)), by=id]
lf233 <- log_feature[,.(lf233 = ifelse("f233"%in%log_feature,1,0)), by=id]
lf315 <- log_feature[,.(lf315 = ifelse("f315"%in%log_feature,1,0)), by=id]
lf82 <- log_feature[,.(lf82 = ifelse("f82"%in%log_feature,1,0)), by=id]
lf71 <- log_feature[,.(lf71 = ifelse("f71"%in%log_feature,1,0)), by=id]
lf193 <- log_feature[,.(lf193 = ifelse("f193"%in%log_feature,1,0)), by=id]
lf80 <- log_feature[,.(lf80 = ifelse("f80"%in%log_feature,1,0)), by=id]

impLf <- lf203[lf312][lf232][lf170][lf313][lf233][lf315][lf82][lf71][lf193][lf80]


lfnums <- log_feature[,.(
  nbfl = .N,
  minlf = min(numlf), maxlf = max(numlf), avglf = mean(numlf), sumlf = sum(numlf),
  minvol = min(volume), maxvol = max(volume), avgvol = mean(volume), sumvol = sum(volume)), by = id]

etnums <- event_type[,.(
  nbet = .N,
  minet = min(numet), maxet = max(numet), avget = mean(numet), sumet = sum(numet)), by = id]

rtnums <- resource_type[,.(
  nbrt = .N,
  minrt = min(numrt), maxrt = max(numrt), avgrt = mean(numrt), sumrt = sum(numrt)), by = id]

t1 <- log_feature[total][,.(
  loc_nid = uniqueN(id),
  loc_nlf = uniqueN(log_feature),
  loc_sumvol = sum(volume),
  loc_avgvol = mean(volume)), keyby=location]

t2 <- resource_type[total][, .(loc_nrt = uniqueN(resource_type)), keyby=location]
t3 <- event_type[total][, .(loc_net = uniqueN(event_type)), keyby=location]
t4 <- severity_type[total][, .(loc_nst = uniqueN(severity_type)), keyby=location]

joined_total <- resource_type[event_type][severity_type][total]
# nombres de combinaisons différentes de resource_type/event_type par location
t5 <- joined_total[,.(etrtcomb = uniqueN(paste(resource_type, event_type, sep = "x"))), keyby = location]
t6 <- joined_total[,.(etstcomb = uniqueN(paste(severity_type, event_type, sep = "x"))), keyby = location]
t7 <- joined_total[,.(rtstcomb = uniqueN(paste(severity_type, resource_type, sep = "x"))), keyby = location]

#t8 <- joined_total[,.(rtetlfcomb = uniqueN(paste(resource_type, event_type, log_feature,sep = "x"))), keyby=location]

location_info_total <- t1[t2][t3][t4]#[t5][t6][t7]#[t8]

total <- total[lfnums][etnums][rtnums]


total <- merge(total, location_info_total, by = "location")
setkeyv(total[,":="(location=NULL)], c("id", "fault_severity"))



##################################################################
rare_lf <- log_feature[total][,.N, by=log_feature][N<=log_feature_rarityThreshold][,log_feature]
log_feature$log_feature[log_feature$log_feature%in%rare_lf] <- "rare_lf"

rare_et <- event_type[total][,.N, by=event_type][N<=event_type_rarity_threshold][,event_type]
event_type$event_type[event_type$event_type%in%rare_et] <- "rare_et"

rare_rt <- resource_type[total][,.N, by=resource_type][N<=resource_type_rarity_threshold][,resource_type]
resource_type$resource_type[resource_type$resource_type%in%rare_rt] <- "rare_rt"

rare_st <- severity_type[total][,.N, by=severity_type][N<=severity_type_rarity_threshold][,severity_type]
severity_type$severity_type[severity_type$severity_type%in%rare_st] <- "rare_st"


rm(list=c("rare_lf", "rare_et", "rare_rt", "rare_st"))

total_lf_volume  <- dcast(
  log_feature[total],
  id + fault_severity ~ log_feature,
  value.var = "volume",
  fun = sum
)

total_et <-
  dcast(event_type[total], id + fault_severity ~ event_type, value.var = "event_type", fun = length)
total_rt <-
  dcast(resource_type[total], id + fault_severity ~ resource_type, value.var = "resource_type", fun = length  )
total_st <-
  dcast(severity_type[total], id + fault_severity ~ severity_type, value.var = "severity_type", fun = length  )

total <- total[total_lf_volume][total_et][total_rt][total_st]

################################################################
train.numeric <- total[fault_severity != -1,-"id", with = FALSE]
test.numeric <- total[fault_severity == -1,-"id", with = FALSE]

xtrain <- as.matrix(train.numeric[,-"fault_severity", with = F])
names(xtrain) <- setdiff(names(train.numeric), "fault_severity")
ytrain <- train.numeric$fault_severity

xtest <-  as.matrix(test.numeric[,-"fault_severity", with = F])
names(xtest) <- setdiff(names(test.numeric), "fault_severity")
test.id <- test$id