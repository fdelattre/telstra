library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)
library(doMC)

#
remove0cols <- T
set.seed(123456)
registerDoMC(cores = 6)
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


log_feature[, log_feature := makeReadable(log_feature)]
event_type[, ":="(numet = makeNumeric(event_type), event_type = makeReadable(event_type))]
resource_type[, resource_type := makeReadable(resource_type)]
severity_type[, severity_type := makeReadable(severity_type)]
train[, ":="(numloc = makeNumeric(location),
             location = makeReadable(location))]
test[, ":="(numloc = makeNumeric(location),
            location = makeReadable(location), fault_severity = -1)]

total <- rbind(train, test)

t1 <- log_feature[total][,.(
  loc_nid = uniqueN(id),
  loc_nlf = uniqueN(log_feature), 
  loc_sumvol = sum(volume),
  loc_avgvol = mean(volume),
  loc_sdvol = ifelse(is.na(sd(volume)), 0, sd(volume)),
  loc_logvol = log(sum(volume)),
  loc_vollog = sum(log(volume+1)),
  loc_sqrtvol = sqrt(sum(volume)),
  loc_volsqrt = sum(sqrt(volume))), keyby=location]

t2 <- resource_type[total][, .(loc_nrt = uniqueN(resource_type)), keyby=location]
t3 <- event_type[total][, .(loc_net = uniqueN(event_type), loc_avg_et = mean(numet)), keyby=location]
t4 <- severity_type[total][, .(loc_nst = uniqueN(severity_type)), keyby=location]

joined_total <- resource_type[event_type][severity_type][total]
# nombres de combinaisons différentes de resource_type/event_type par location
t5 <- joined_total[,etrtcomb := paste(resource_type, event_type, sep = "x")][, .(loc_etrtcomb = uniqueN(etrtcomb)), keyby=location]
t6 <- joined_total[,etstcomb := paste(severity_type, event_type, sep = "x")][, .(loc_etstcomb = uniqueN(etstcomb)), keyby=location]
t7 <- joined_total[,rtstcomb := paste(severity_type, resource_type, sep = "x")][, .(loc_rtstcomb = uniqueN(rtstcomb)), keyby=location]
# t8 <- log_feature[joined_total, allow.cartesian=TRUE][,lfetcomb := paste(log_feature, event_type, sep="x")][,.(loc_lfetcomb = uniqueN(lfetcomb)), keyby=location]
# t9 <- joined_total[,rtetstcomb := paste(severity_type, event_type, resource_type, sep = "x")][, .(loc_rtetstcomb = uniqueN(rtetstcomb)), keyby=location]

location_info_total <- t1[t2][t3][t4][t5][t6][t7]

total <- merge(total, location_info_total, by = "location")[,":="(location=NULL)]
setkeyv(total, c("id", "fault_severity"))

# total_loc <-  dcast(total, id + fault_severity ~ location, value.var = "location", fun = length)

####################################

total_lf_volume  <- dcast(
  log_feature[total],
  id + fault_severity ~ log_feature,
  value.var = list("volume", "log_feature"),
  fun = list(sum, length)
)

total_et <-
  dcast(event_type[total], id + fault_severity ~ event_type, value.var = "event_type", fun = length)
total_rt <-
  dcast(
    resource_type[total], id + fault_severity ~ resource_type, value.var = "resource_type", fun = length
  )
total_st <-
  dcast(
    severity_type[total], id + fault_severity ~ severity_type, value.var = "severity_type", fun = length
  )

total.wide <-
  total[total_lf_volume][total_et][total_rt][total_st]

######################################################################################
#calculs d'intercations entre event_type, resource_types, log_features

total.wide[,":="(
  
  r2e34 = r2 * e34,
  r2e35 = r2 * e35,
  r2e12 = r2 * e12,
  e6r4 = r4 * e6,
  e6r3 = r3 * e6,
  
  f202r8 = r8 * log_feature.1_length_f202,
  f202r2 = r2 * log_feature.1_length_f202,
  
  f202r8vol = r8 * log_feature.1_length_f202 * volume_sum_f202,
  f202r2vol = r2 * log_feature.1_length_f202 * volume_sum_f202,
  
  f202f81 = log_feature.1_length_f202*log_feature.1_length_f81,
  f202f311 = log_feature.1_length_f202*log_feature.1_length_f311,
  f202f81vol = volume_sum_f202 + volume_sum_f81,
  f202f311vol = volume_sum_f202 + volume_sum_f311
)]



######################################################################################

train.wide <- total.wide[fault_severity != -1,-"id", with = FALSE]
test.wide <- total.wide[fault_severity == -1,-"id", with = FALSE]

keepcols <- names(train.wide)
if(remove0cols){
#retrait des colonnes ayant essentiellement des 0
  n <- length(keepcols)
counts <- apply(total.wide, 2, sum)
keepcols <- names(counts[counts > 5])
n2 <- length(keepcols)
cat(n-n2, "columns removed\n")
}

# write files with train and test
write.csv(train.wide, paste(sep = "-", "train.csv"), row.names = F, quote = F)
write.csv(test.wide[,.SD, .SDcols = -"fault_severity"], paste(sep = "-", "test.csv"), row.names = F, quote = F)

train.set.mat <-
  model.matrix(fault_severity ~  .,data = train.wide[,, .SDcols = keepcols])
test.set.mat <-
  model.matrix(fault_severity ~ . ,data = test.wide[,, .SDcols = keepcols])