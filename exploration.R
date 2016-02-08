library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)

source('utils.R')

train <- fread("~/datascience/challenges/telstra/data/train.csv")%>%setkey(id)
test <- fread("~/datascience/challenges/telstra/data/test.csv")%>%setkey(id)
sample_submission <- fread("~/datascience/challenges/telstra/data/sample_submission.csv")%>%setkey(id)
log_feature <- fread("~/datascience/challenges/telstra/data/log_feature.csv")%>%setkey(id)
event_type <- fread("~/datascience/challenges/telstra/data/event_type.csv")%>%setkey(id)
resource_type <- fread("~/datascience/challenges/telstra/data/resource_type.csv")%>%setkey(id)
severity_type <- fread("~/datascience/challenges/telstra/data/severity_type.csv")%>%setkey(id)

log_feature[, log_feature := makeReadable(log_feature)]
event_type[, ":="(numet = makeNumeric(event_type), event_type = makeReadable(event_type))]
resource_type[, resource_type := makeReadable(resource_type)]
severity_type[, severity_type := makeReadable(severity_type)]
train[, ":="(numloc = makeNumeric(location), location = makeReadable(location),
             fault_present = ifelse(fault_severity == 0,0,1))]
test[, ":="(numloc = makeNumeric(location), location = makeReadable(location), fault_severity = -1, fault_present = -1)]

total <- rbind(train, test)%>%setkey(id)
# comparaison train / test

train_lf <- unique(log_feature[train]$log_feature)
test_lf <- unique(log_feature[test]$log_feature)
train_lf_not_in_test <- train_lf[which(!is.element(train_lf, test_lf))]
test_lf_not_in_train <- test_lf[which(!is.element(test_lf, train_lf))]
log_feature[total][,.N, by=log_feature][order(N, decreasing = T)][N==1]
log_feature[test][,.N, by=log_feature][order(N, decreasing = T)][log_feature%in%test_lf_not_in_train]
log_feature[test][,sum(volume), by=log_feature][order(N, decreasing = T)][log_feature%in%test_lf_not_in_train]

# ==> 331 lf dans le train
# ==> 335 lf dans le test
# ==> 51  lf du jeu de train ne figurent pas dans le jeu de test ==> supprimer ces lf du jeu de train
# ==> 55  lf du jeu de test ne figurent pas dans le jeu de train 
#    ==> les remplacer par les lf présentes similaires en terme de qté ?
#    ==> les remplacer par les lf présentes similaires en terme de volume ?
# ==> 85  lf avec un seul unique id  ==> les regrouper en rare_lf
# ==> 33  lf avec 2 id ==> rares ?
# ==> 28  lf avec 3 id ==> rares ?

train_et <- unique(event_type[train]$event_type)
test_et <- unique(event_type[test]$event_type)
train_et_not_in_test <- train_et[which(!is.element(train_et, test_et))]
test_et_not_in_train <- test_et[which(!is.element(test_et, train_et))]
event_type[train][,.N, by=event_type][order(N, decreasing = T)][N <= 5]
# ==> 4 event_types du jeu de test ne figurent pas dans le jeu de train ==> les remplacer
# ==> 11 event_types avec un count < 5

train_rt <- unique(resource_type[train]$resource_type)
test_rt <- unique(resource_type[test]$resource_type)
train_rt_not_in_test <- train_rt[which(!is.element(train_rt, test_rt))]
test_rt_not_in_train <- test_rt[which(!is.element(test_rt, train_rt))]
resource_type[train][,uniqueN(id), by=resource_type][order(V1, decreasing = T)][,sum(V1)]
# ==> pas de différences entre train et test
# ==> r8 et r2 présents sur plus de 7800 ids
# ==> r5 est plus rare


joined_train <- log_feature[resource_type][event_type, allow.cartesian = TRUE][severity_type][train]
joined_test <- log_feature[resource_type][event_type, allow.cartesian = TRUE][severity_type][test]



ggplot(data = log_feature[train][log_feature%in%significant_log_feature])+
  geom_bar(aes(x = log_feature, fill = factor(fault_present)))

log_feature[train][, uniqueN(id), by = .(log_feature, location)][order(V1, decreasing = T)]
log_feature[train][fault_severity == 0][, uniqueN(id), by = .(log_feature)][order(V1, decreasing = T)]
log_feature[train][fault_severity == 1][, uniqueN(id), by = .(log_feature)][order(V1, decreasing = T)]
log_feature[train][fault_severity == 2][, uniqueN(id), by = .(log_feature)][order(V1, decreasing = T)]
log_feature[train][fault_severity == 2][, uniqueN(id), by = location][order(V1, decreasing = T)]


# Event_types
ggplot(data = event_type[train])+
  geom_bar(aes(x = event_type, fill = factor(fault_severity)))
event_type[train][, uniqueN(id), by = .(event_type, location)][order(V1, decreasing = T)]
most_freq_et_c0 <- event_type[train][fault_severity == 0][, uniqueN(id), by = .(event_type)][order(V1, decreasing = T)][1:10, event_type]
most_freq_et_c1 <- event_type[train][fault_severity == 1][, uniqueN(id), by = .(event_type)][order(V1, decreasing = T)][1:10, event_type]
most_freq_et_c2 <- event_type[train][fault_severity == 2][, uniqueN(id), by = .(event_type)][order(V1, decreasing = T)][1:10, event_type]

# Resources_types
ggplot(data = resource_type[train])+
  geom_bar(aes(x = resource_type, fill = factor(fault_severity)))
resource_type[train][, uniqueN(id), by = .(resource_type, location)][order(V1, decreasing = T)]

ggplot(data = severity_type[train])+
  geom_bar(aes(x = severity_type, fill = factor(fault_present)))

# combinaisons de resources et d'event
res_rt_all <- event_type[resource_type][train][,uniqueN(id), by = .(event_type, resource_type, fault_severity)]
res_rt_c0 <- event_type[resource_type][train][fault_severity == 0][,uniqueN(id), by = .(event_type, resource_type)]
res_rt_c1 <- event_type[resource_type][train][fault_severity == 1][,uniqueN(id), by = .(event_type, resource_type)]
res_rt_c2 <- event_type[resource_type][train][fault_severity == 2][,uniqueN(id), by = .(event_type, resource_type)]

ggplot(data = res_rt_all)+
  geom_tile(aes(x=event_type, y=resource_type, fill=V1))+
  facet_grid(fault_severity~.)

ggplot(data = res_rt_c0)+
  geom_raster(aes(x=event_type, y=resource_type, fill=V1))

ggplot(data = res_rt_c1)+
  geom_raster(aes(x=event_type, y=resource_type, fill=V1))

ggplot(data = res_rt_c2)+
  geom_raster(aes(x=event_type, y=resource_type, fill=V1))
  

# combinaisons de resources types et de features
res_lf_all <- log_feature[resource_type][train][,uniqueN(id), by = .(resource_type, log_feature)]
res_lf_c0 <- log_feature[resource_type][train][fault_severity == 0][,.(c0 =uniqueN(id)), by = .(resource_type, log_feature)]
res_lf_c1 <- log_feature[resource_type][train][fault_severity == 1][,.(c1 =uniqueN(id)), by = .(resource_type, log_feature)]
res_lf_c2 <- log_feature[resource_type][train][fault_severity == 2][,.(c2 =uniqueN(id)), by = .(resource_type, log_feature)]

temp <- merge(merge(res_lf_c0, res_lf_c1, by=c("resource_type", "log_feature"), all = T), res_lf_c2,  by=c("resource_type", "log_feature"), all = T)

ggplot(data = res_lf_c0)+
  geom_raster(aes(x=resource_type, y=log_feature, fill=c0))
ggplot(data = res_lf_c1)+
  geom_raster(aes(x=resource_type, y=log_feature, fill=V1))
ggplot(data = res_lf_c2)+
  geom_raster(aes(x=resource_type, y=log_feature, fill=V1))


# combinaison d'event et de log feature
rt_lf_all <- log_feature[event_type, allow.cartesian=TRUE][train][,uniqueN(id), by = .(event_type, log_feature)]
rt_lf_c0 <- log_feature[event_type, allow.cartesian=TRUE][train][fault_severity == 0][,uniqueN(id), by = .(event_type, log_feature)]
rt_lf_c1 <- log_feature[event_type, allow.cartesian=TRUE][train][fault_severity == 1][,uniqueN(id), by = .(event_type, log_feature)]
rt_lf_c2 <- log_feature[event_type, allow.cartesian=TRUE][train][fault_severity == 2][,uniqueN(id), by = .(event_type, log_feature)]

ggplot(data = rt_lf_c0)+
  geom_raster(aes(x=event_type, y=log_feature, fill=V1))

ggplot(data = rt_lf_c1)+
  geom_raster(aes(x=event_type, y=log_feature, fill=V1))

ggplot(data = rt_lf_c2)+
  geom_raster(aes(x=event_type, y=log_feature, fill=V1))

# quel volume par event_type ?
log_feature[event_type, allow.cartesian=TRUE][train][fault_severity == 0][, .(avgvolet = mean(volume), nblfet = uniqueN(log_feature)), by = event_type]

# combinaison d'event et de severity
et_sv_all <- severity_type[event_type][train][,uniqueN(id), by = .(event_type, severity_type, fault_severity)]

et_sv_c0 <- severity_type[event_type][train][fault_severity == 0][,uniqueN(id), by = .(event_type, severity_type)]
et_sv_c1 <- severity_type[event_type][train][fault_severity == 1][,uniqueN(id), by = .(event_type, severity_type)]
et_sv_c2 <- severity_type[event_type][train][fault_severity == 2][,uniqueN(id), by = .(event_type, severity_type)]

ggplot(data = et_sv_all)+
  geom_tile(aes(x=event_type, y=severity_type, fill=V1))+
  facet_grid(fault_severity~.)

ggplot(data = et_sv_c0)+
  geom_raster(aes(x=event_type, y=severity_type, fill=V1))

ggplot(data = et_sv_c0)+
  geom_raster(aes(x=event_type, y=severity_type, fill=V1))

ggplot(data = et_sv_c0)+
  geom_raster(aes(x=event_type, y=severity_type, fill=V1))

# Combinaison de resource et de severity
rt_sv_all <- severity_type[resource_type][train][,uniqueN(id), by = .(resource_type, severity_type, fault_severity)]

rt_sv_c0 <- severity_type[resource_type][train][fault_severity == 0][,uniqueN(id), by = .(resource_type, severity_type)]
rt_sv_c1 <- severity_type[resource_type][train][fault_severity == 1][,uniqueN(id), by = .(resource_type, severity_type)]
rt_sv_c2 <- severity_type[resource_type][train][fault_severity == 2][,uniqueN(id), by = .(resource_type, severity_type)]

ggplot(data = rt_sv_all)+
  geom_tile(aes(x=resource_type, y=severity_type, fill=V1))+
  facet_grid(fault_severity~.)

ggplot(data = rt_sv_c0)+
  geom_raster(aes(x=resource_type, y=severity_type, fill=V1))

ggplot(data = rt_sv_c0)+
  geom_raster(aes(x=resource_type, y=severity_type, fill=V1))

ggplot(data = rt_sv_c0)+
  geom_raster(aes(x=resource_type, y=severity_type, fill=V1))

# Combinaisons de location et de resource type
rt_loc_all <- resource_type[train][,uniqueN(id), by = .(resource_type, location, fault_severity)]
rt_loc_c0 <- resource_type[train][fault_severity == 0][,uniqueN(id), by = .(resource_type, location, fault_severity)]
rt_loc_c1 <- resource_type[train][fault_severity == 1][,uniqueN(id), by = .(resource_type, location, fault_severity)]
rt_loc_c2 <- resource_type[train][fault_severity == 2][,uniqueN(id), by = .(resource_type, location, fault_severity)]
ggplot(data = rt_loc_all)+
  geom_tile(aes(x=resource_type, y=location, fill=V1))+
  facet_grid(fault_severity~.)

ggplot(data = rt_loc_c0)+
  geom_raster(aes(x=resource_type, y=location, fill=V1))

ggplot(data = rt_loc_c1)+
  geom_raster(aes(x=resource_type, y=location, fill=V1))




# nombre de resources par location
View(resource_type[train][,.(.N, uniqueN(resource_type)), by=.(location, fault_severity)])

# nombre d'event par location
View(event_type[train][,.(.N, uniqueN(event_type)), by=.(location, fault_severity)])

# remplacer location par :
# nombre d'event
# nombre de resource_type
# nombre de log_feature
# sum_volume de log_feature
# avg_volume de log_feature


ggplot(data = event_type[train])+
  geom_bar(aes(x = numet, fill = as.factor(fault_severity)), position = "dodge")

ggplot(data = train)+
  geom_bar(aes(x = numloc, fill = as.factor(fault_severity)))

event_type_info <- log_feature[event_type, allow.cartesian=TRUE][train][, .(avgvolet = mean(volume), nblfet = uniqueN(log_feature)), by = .(event_type, fault_severity)]
event_type_info0 <- log_feature[event_type, allow.cartesian=TRUE][train][fault_severity == 0][, .(avgvolet = mean(volume), nblfet = uniqueN(log_feature)), keyby = event_type]
event_type_info1 <- log_feature[event_type, allow.cartesian=TRUE][train][fault_severity == 1][, .(avgvolet = mean(volume), nblfet = uniqueN(log_feature)), keyby = event_type]
event_type_info2 <- log_feature[event_type, allow.cartesian=TRUE][train][fault_severity == 2][, .(avgvolet = mean(volume), nblfet = uniqueN(log_feature)), keyby = event_type]
joined_total <- merge(joined_total, event_type_info, by = "event_type")
t8 <- joined_total[,.(avgvolet = sum(avgvolet), nblfet = sum(nblfet)), keyby = id]


ggplot(data = event_type_info)+
  geom_boxplot(aes(x = as.factor(fault_severity), y = log(nblfet)))


resource_type_info <- log_feature[resource_type, allow.cartesian=TRUE][train][, .(avgvolet = mean(volume), nblfet = uniqueN(log_feature)), by = .(resource_type, fault_severity)]
resource_type_info0 <- log_feature[resource_type, allow.cartesian=TRUE][train][fault_severity == 0][, .(avgvolet = mean(volume), nblfet = uniqueN(log_feature)), keyby = resource_type]
resource_type_info1 <- log_feature[resource_type, allow.cartesian=TRUE][train][fault_severity == 1][, .(avgvolet = mean(volume), nblfet = uniqueN(log_feature)), keyby = resource_type]
resource_type_info2 <- log_feature[resource_type, allow.cartesian=TRUE][train][fault_severity == 2][, .(avgvolet = mean(volume), nblfet = uniqueN(log_feature)), keyby = resource_type]
joined_total <- merge(joined_total, resource_type_info, by = "resource_type")
t8 <- joined_total[,.(avgvolet = sum(avgvolet), nblfet = sum(nblfet)), keyby = id]


ggplot(data = resource_type_info)+
  geom_boxplot(aes(x = as.factor(fault_severity), y = log(avgvolet)))


ggplot(data = severity_type[train])+
  geom_bar(aes(x = numst, fill = as.factor(fault_severity)))



ggplot(data = log_feature[train])+
  geom_hex(aes(x = numlf, y = numloc, fill = as.factor(fault_severity)))