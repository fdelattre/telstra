library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)
library(doMC)
library(caret)

#
remove0cols <- T
writeFiles <- T
computeFeatures <- T
computeManualInteractions <- T
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


log_feature[, ":="(log_feature = makeReadable(log_feature))]
event_type[, ":="(event_type = makeReadable(event_type))]
resource_type[, ":="(resource_type = makeReadable(resource_type))]
severity_type[, ":="(severity_type = makeReadable(severity_type))]


train[, ":="(numloc = makeNumeric(location),
             location = makeReadable(location))]
test[, ":="(numloc = makeNumeric(location),
            location = makeReadable(location), fault_severity = -1)]

total <- rbind(train, test)%>%setkey("id")

if(computeFeatures){

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
  t3 <- event_type[total][, .(loc_net = uniqueN(event_type)), keyby=location]
  t4 <- severity_type[total][, .(loc_nst = uniqueN(severity_type)), keyby=location]
  
  joined_total <- resource_type[event_type][severity_type][total]
  
  # Volume et nb de log features par event_type
  event_type_info <- log_feature[event_type, allow.cartesian=TRUE][, .(avgvolet = mean(volume), nblfet = uniqueN(log_feature)), keyby = event_type]
  joined_total <- merge(joined_total, event_type_info, by = "event_type")
  t8 <- joined_total[,.(avgvolet = sum(log(avgvolet))), keyby = id]
  
  # Volume et nb de log features par resource_type
  resource_type_info <- log_feature[resource_type][, .(avgvolrt = mean(volume), nblfrt = uniqueN(log_feature)), keyby = resource_type]
  joined_total <- merge(joined_total, resource_type_info, by = "resource_type")
  t9 <- joined_total[,.(avgvolrt = sum(log(avgvolrt))), keyby = id]
  
  # Volume et nb de log features par severity_type
  severity_type_info <- log_feature[severity_type][, .(avgvolst = mean(volume), nblfst = uniqueN(log_feature)), keyby = severity_type]
  joined_total <- merge(joined_total, severity_type_info, by = "severity_type")
  t10 <- joined_total[,.(avgvolst = sum(log(avgvolst))), keyby = id]
  
  # nombres de combinaisons différentes de resource_type/event_type par location
  t5 <- joined_total[,etrtcomb := paste(resource_type, event_type, sep = "x")][, .(loc_etrtcomb = uniqueN(etrtcomb)), keyby=location]
  t6 <- joined_total[,etstcomb := paste(severity_type, event_type, sep = "x")][, .(loc_etstcomb = uniqueN(etstcomb)), keyby=location]
  t7 <- joined_total[,rtstcomb := paste(severity_type, resource_type, sep = "x")][, .(loc_rtstcomb = uniqueN(rtstcomb)), keyby=location]
  
  location_info_total <- t1[t2][t3][t4][t5][t6][t7]
  
  total <- total[t8][t9][t10]
  #[,":="(location=NULL)]
}
setkeyv(total[,":="(location=NULL)], c("id", "fault_severity"))

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
if(computeManualInteractions){
  total.wide[,":="(

    xore15r8 = as.numeric(r8 | e15),
    xore11r6 = as.numeric(r6 | e11),

    f202f81vol = volume_sum_f202 + volume_sum_f81,
    f202f311vol = volume_sum_f202 + volume_sum_f311,
    
    som_vol_feat = volume_sum_f82 + volume_sum_f203 + volume_sum_f71 + volume_sum_f193 + volume_sum_f80,
    som_vol_feat_c0 = volume_sum_f313 + volume_sum_f233 + volume_sum_f315,
    som_vol_feat_c1 = volume_sum_f82 + volume_sum_f203 + volume_sum_f170,
    som_vol_feat_c2 = volume_sum_f71 + volume_sum_f193 + volume_sum_f80,
    
    xor_feat = as.numeric(log_feature.1_length_f82 | log_feature.1_length_f203 | log_feature.1_length_f71 | log_feature.1_length_f193 | log_feature.1_length_f80),
    xor_feat_c0 = as.numeric(log_feature.1_length_f313 | log_feature.1_length_f233 | log_feature.1_length_f315),
    xor_feat_c1 = as.numeric(log_feature.1_length_f82 | log_feature.1_length_f203 | log_feature.1_length_f170),
    xor_feat_c2 = as.numeric(log_feature.1_length_f71 | log_feature.1_length_f193 | log_feature.1_length_f80),
    
    and_feat_c0 = as.numeric(log_feature.1_length_f313 & log_feature.1_length_f233 & log_feature.1_length_f315),
    and_feat_c1 = as.numeric(log_feature.1_length_f82 & log_feature.1_length_f203 & log_feature.1_length_f170),
    and_feat_c2 = as.numeric(log_feature.1_length_f71 & log_feature.1_length_f193 & log_feature.1_length_f80),
    
    xore34r2 = as.numeric(e34 | r2),
    xore35r2 = as.numeric(e35 | r2),
    xore54r8 = as.numeric(e54 | r8),
    xore11r8 = as.numeric(e11 | r8),
    xore10r8 = as.numeric(e10 | r8),
    
    xore36s2 = as.numeric(e36|s2),
    xore36s3 = as.numeric(e36|s3),
    xore36e17 = as.numeric(e36|e17),
    xore36e12 = as.numeric(e36|e12),
    
    xore54e42 = as.numeric(e54|e42),
    testvol = volume_sum_f105 + volume_sum_f106 + volume_sum_f114
    )]
}


######################################################################################

removecols <- names(total.wide)
if(remove0cols){
  
#retrait des colonnes ayant essentiellement des 0
  n <- length(removecols)
  counts <- apply(total.wide, 2, sum)
  cols2remove <- names(counts[counts == 0])
  n2 <- length(cols2remove)
  writeLines(paste(n2, "columns removed", sep = " "))
  removecols <- setdiff(cols2remove, "fault_severity")
  train.wide <- total.wide[fault_severity != -1,-c(removecols, "id"), with = FALSE]
  test.wide <- total.wide[fault_severity == -1,-c(removecols, "id"), with = FALSE]
} else{
  train.wide <- total.wide[fault_severity != -1,-"id", with = FALSE]
  test.wide <- total.wide[fault_severity == -1,-"id", with = FALSE]
}


# write files with train and test
if(writeFiles){
  writeLines("Writing train.csv and test.csv...")
  write.csv(train.wide, paste(sep = "-", "train.csv"), row.names = F, quote = F)
  write.csv(test.wide[,.SD, .SDcols = -"fault_severity"], paste(sep = "-", "test.csv"), row.names = F, quote = F)
  writeLines("...done")}

xtrain <- model.matrix(fault_severity ~  .,data = train.wide)
names(xtrain) <- setdiff(names(train.wide), "fault_severity")
ytrain <- train.wide$fault_severity

xtest <-  model.matrix(fault_severity ~ . ,data = test.wide)
names(xtest) <- setdiff(names(test.wide), "fault_severity")
test.id <- test$id




#clean up
writeLines("Cleaning up...")

#rm(list=setdiff(ls(),c("xtrain", "ytrain", "xtest", "test.id") ))
