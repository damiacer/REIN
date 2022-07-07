
#--------------------------------------------------------------------------------  
### DATA
getwd()
setwd("/Users/damianocerasuolo/Desktop/PhD/M2/DATABASES_REIN/csv_data") 
#OLD rdb <- read.csv2("reindatasave.csv", header = TRUE, na.string="NA", sep = "\t")
rdb <- read.csv2("rdb.saved.csv")
names(rdb)
dim(rdb)
#View(rdb)
#--------------------------------------------------------------------------------  
#population : N = 45026, dont PKR+ = 2560
#events of interest : N = (ischemic/hemorrhage/all events)
#median follow-up time

# TABLES
#table1: characteristics of the population (col1=PKR+, col2=PKR-, col3=all patients)
#table 1bis: results of imputation (col1=complete case dataset, col2=imputed data set)

#ALL EVENTS
#table 2: bivariate analysis for all stroke events, all patients
#table3: multivariate analysis for all stroke events, all patients
#table4: bivariate analysis for all stroke events, non diabetic patients
#table5: multivariate analysis for all stroke events, non diabetic patients

#ISCHEMIC EVENTS
#table6: bivariate analysis for  ischemic stroke, all patients
#table7: multivariate analysis for  ischemic stroke, all patients
#table8: bivariate analysis for  ischemic stroke, non diabetic patients
#table9: multivariate analysis for  ischemic stroke, non diabetic patients

#HEMORRHAGIC STROKE
#table10: bivariate analysis for  hemorrhagic stroke, all patients
#table11: multivariate analysis for hemorrhagic stroke, all patients
#table12: bivariate analysis for hemorrhagic stroke, non diabetic patients
#table13: multivariate analysis for hemorrhagic stroke, non diabetic patients
#--------------------------------------------------------------------------------
### PACKAGES FOR SURVIVAL 
#install.packages("survminer")
library("survminer")
#install.packages("survival")
library("survival")
#install.packages("mice")
library("mice")
#install.packages("grid")
library("grid")
#install.packages("VIM")
library("VIM")
#--------------------------------------------------------------------------------
### TIME AND STATUS INFORMATION
rdb$time = rdb$epilogus
rdb$time = as.numeric(as.character(rdb$time))
library("dplyr")

# MAKE IT RUN
rdb$EVENTUM[rdb$t.b.eN == "2" & rdb$EVENT == "1"] <- "event"
rdb$EVENTUM[rdb$t.b.eN == "2" & rdb$EVENT == "0"] <- "no event"
rdb$EVENTUM[rdb$t.b.eN == "1" & rdb$EVENT == "1"] <- "no event"
rdb$EVENTUM[rdb$t.b.eN == "1" & rdb$EVENT == "0"] <- "no event"
rdb$EVENTUM[rdb$t.b.eN == "0" & rdb$EVENT == "0"] <- "no event"
rdb$EVENTUM[rdb$t.b.eN == "0" & rdb$EVENT == "1"] <- "event"
rdb$status = if_else(rdb$EVENTUM == "event", "1", "0")

rdb$status = as.factor(rdb$status)
str(rdb$status)
rdb$status = as.numeric(as.character(rdb$status))
#--------------------------------------------------------------------------------
### IMPUTATION
library(mice)
rdb.toimput = subset(rdb, select = c(cardiovasc, tabac2, dial, apkd01, 
                                     sex, age, diabetes, bmic, status, time))

rdb.toimput$cardiovasc <- as.factor(rdb.toimput$cardiovasc)
rdb.toimput$tabac2 <- as.factor(rdb.toimput$tabac2)
rdb.toimput$apkd01 <- as.factor(rdb.toimput$apkd01)
rdb.toimput$sex <- as.factor(rdb.toimput$sex)
rdb.toimput$age <- as.numeric(as.character(rdb.toimput$age))
#rdb.toimput$diabetes <- as.factor(rdb.toimput$diabetes) # it is necessary to keep the variable
#       "diabetes" as "int" if you wanna use the "is.na" function and exclude efficently 
#       all subjects presenting a diabetes or not having this information 
#       transforming the variable into a factor (AS IT IS NECESSARY IN THE GENERAL ANALYSIS)
#       won't allow to create the "only non diabetic patients" dataset
rdb.toimput$status <- as.factor(rdb.toimput$status)
rdb.toimput$time <- as.numeric(as.character(rdb.toimput$time))
rdb.toimput$bmic <- as.factor(rdb.toimput$bmic)

md.pattern(rdb.toimput)

aggr(rdb.toimput, delimiter = NULL, plot = TRUE)

# The red box plot on the left shows the distribution of var X 
# the blue box plot shows the distribution of the remaining datapoints
# Likewhise for the red box plots at the bottom of the graph
# if our assumption of MCAR data is correct, then we expect the 
# red and blue box plots to be very similar

marginplot(rdb.toimput[c(4,3)])

### DATA IMPUTATION
rdb.imputed <- mice(rdb.toimput, m=20, maxit=10, meth='pmm', seed=210586,
                    print = FALSE)
#--------------------------------------------------------------------------------
### TABLES 1 and 1bis
# table1: characteristics of the population (col 1 = PKR+, col 2 = PKR-, col 3 = all patients)

library(tableone)

dput(names(rdb))

# function to transform in numeric values
n <- function(x){
  result = as.numeric(as.character(x))
}

rdb$PDS = n(rdb$PDS)
rdb$TAIL = n(rdb$TAIL)
rdb$bmi = n(rdb$bmi)
rdb$age = n(rdb$age)
rdb$delai_IRT = n(rdb$delai_IRT)
rdb$delai_DC = n(rdb$delai_DC)
rdb$delai_TX = n(rdb$delai_TX)
rdb$delai_SVR = n(rdb$delai_SVR)
rdb$delai_PDV = n(rdb$delai_PDV)
rdb$delai_DERNOUV2019 = n(rdb$delai_DERNOUV2019)

variables <- c("URGn", "KTTINIn", "EPOINIn",  
               "nephgp", "METHOn", "techn", "MODALn", "VAVn", "traitement", 
               "PDS", "TAIL", "IRCn", "O2n", "ICn", "ICOROn", "IDMn", "RYTHMn", 
               "ANEVn", "AMIn", "AVCAITn", "KCn", "VHBn", "VHCn", "CIRHn", "VIHn", 
               "SIDAn", "HANDn", "AMPn", "PLEGn", "CECITEn", "COMPORTn", "TYPDIABn", 
               "STADICn", "STDAMIn", "STDCIRHn", "TABACn", "bmi", "tabac2", 
               "iresp", "sero", "coro", "foie", "comCV", "comcvcl", "comcvcl2", 
               "sex", "age", "ETAT_DERNOUV2019", "delai_IRT", "delai_DC", "delai_TX", 
               "delai_SVR", "delai_PDV", "delai_DERNOUV2019", "groupes6", "categories18", 
               "groupes6_CA1", "categories18_CA1", "groupes6_CA2", "categories18_CA2", 
               "MOTIF_An", "CPKMEDn", "REFUSn",  
               "DGN_PAL", "DEATH", "hemorragic", 
               "ischemic", "status", "bmic", "apkd01", "cardiovasc")

categorical <- c("URGn", "KTTINIn", "EPOINIn",  
               "nephgp", "METHOn", "techn", "MODALn", "VAVn", "traitement", 
               "IRCn", "O2n", "ICn", "ICOROn", "IDMn", "RYTHMn", 
               "ANEVn", "AMIn", "AVCAITn", "KCn", "VHBn", "VHCn", "CIRHn", "VIHn", 
               "SIDAn", "HANDn", "AMPn", "PLEGn", "CECITEn", "COMPORTn", "TYPDIABn", 
               "STADICn", "STDAMIn", "STDCIRHn", "TABACn", "tabac2", 
               "iresp", "sero", "coro", "foie", "comCV", "comcvcl", "comcvcl2", 
               "sex", "ETAT_DERNOUV2019", "groupes6", "categories18", 
               "groupes6_CA1", "categories18_CA1", "groupes6_CA2", "categories18_CA2", 
               "MOTIF_An", "CPKMEDn", "REFUSn",  
               "DGN_PAL", "DEATH", "hemorragic", 
               "ischemic", "status", "bmic", "apkd01", "cardiovasc")

tab.one <- CreateTableOne(vars = variables, data = rdb, factorVars = categorical)
print(tab.one, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

descr.tab1 = CreateTableOne(vars = variables, factorVars = categorical, data = rdb, test = F,
                            strata = "apkd01")
print(descr.tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

#--------------------------------------------------------------------------------
### TABLE 2
# table 2: bivariate analysis for all stroke events, all patients

# complete model
# = with(rdb.imputed, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 + apkd01 + bmic +
#                                          sex + age + diabetes))

# cardiovascular
cox.t2.cardiovasc = with(rdb.imputed, coxph(Surv(time, status=="1") ~ cardiovasc))
summary(pool(cox.t2.cardiovasc))
est.card <- pool(cox.t2.cardiovasc)
summary(est.card, conf.int = TRUE, exponentiate = TRUE)

# tabac2
cox.t2.tab = with(rdb.imputed, coxph(Surv(time, status=="1") ~ tabac2))
summary(pool(cox.t2.tab))
est.tab <- pool(cox.t2.tab)
summary(est.tab, conf.int = TRUE, exponentiate = TRUE)

# adpkd
cox.t2.adpkd = with(rdb.imputed, coxph(Surv(time, status=="1") ~ apkd01))
summary(pool(cox.t2.adpkd))
est.adpkd <- pool(cox.t2.adpkd)
summary(est.adpkd, conf.int = TRUE, exponentiate = TRUE)

# bmic
cox.t2.bmic = with(rdb.imputed, coxph(Surv(time, status=="1") ~ bmic))
summary(pool(cox.t2.bmic))
est.bmic <- pool(cox.t2.bmic)
summary(est.bmic, conf.int = TRUE, exponentiate = TRUE)

# sex
cox.t2.sex = with(rdb.imputed, coxph(Surv(time, status=="1") ~ sex))
summary(pool(cox.t2.sex))
est.sex <- pool(cox.t2.sex)
summary(est.sex, conf.int = TRUE, exponentiate = TRUE)

# age
cox.t2.age = with(rdb.imputed, coxph(Surv(time, status=="1") ~ age))
summary(pool(cox.t2.age))
est.age <- pool(cox.t2.age)
summary(est.age, conf.int = TRUE, exponentiate = TRUE)

# diabetes
cox.t2.dia = with(rdb.imputed, coxph(Surv(time, status=="1") ~ diabetes))
summary(pool(cox.t2.dia))
est.dia <- pool(cox.t2.dia)
summary(est.dia, conf.int = TRUE, exponentiate = TRUE)

#--------------------------------------------------------------------------------
### TABLE 3
# table 3: multivariate analysis for all stroke events, all patients
cox.t3 = with(rdb.imputed, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 + apkd01 +
                                   bmic + sex + age + diabetes))
summary(pool(cox.t3))
est.t3 <- pool(cox.t3)
summary(est.t3, conf.int = TRUE, exponentiate = TRUE)

#--------------------------------------------------------------------------------
#################################################################################
names(rdb.toimput)
table(rdb.toimput$diabetes)
#--------------------------------------------------------------------------------
# DIABETES VARIABLE 
rdb.toimput$diabetes[is.na(rdb.toimput$diabetes)] <- "2"
#--------------------------------------------------------------------------------
is.na(rdb.toimput$diabetes)
str(rdb.toimput$diabetes)
str(rdb.toimput$bmic)

rdb.toimput.diab = rdb.toimput[(rdb.toimput$diabetes=="0"),] # this will NOT includes the NA
count(rdb.toimput.diab) # 22943
rdbdia.imputed<- mice(rdb.toimput.diab, m=20, maxit=10, meth='pmm', seed=210586,
                           print = FALSE)
#################################################################################
#--------------------------------------------------------------------------------
# TABLE 4
#table4: bivariate analysis for all stroke events, non diabetic patients

# cardiovascular
cox.t4.cardiovasc = with(rdbdia.imputed, coxph(Surv(time, status=="1") ~ cardiovasc))
summary(pool(cox.t4.cardiovasc))
est.card <- pool(cox.t4.cardiovasc)
summary(est.card, conf.int = TRUE, exponentiate = TRUE)

# tabac2
cox.t4.tab = with(rdbdia.imputed, coxph(Surv(time, status=="1") ~ tabac2))
summary(pool(cox.t4.tab))
est.tab <- pool(cox.t4.tab)
summary(est.tab, conf.int = TRUE, exponentiate = TRUE)

# adpkd
cox.t4.adpkd = with(rdbdia.imputed, coxph(Surv(time, status=="1") ~ apkd01))
summary(pool(cox.t4.adpkd))
est.adpkd <- pool(cox.t4.adpkd)
summary(est.adpkd, conf.int = TRUE, exponentiate = TRUE)

# bmic
cox.t4.bmic = with(rdbdia.imputed, coxph(Surv(time, status=="1") ~ bmic))
summary(pool(cox.t4.bmic))
est.bmic <- pool(cox.t4.bmic)
summary(est.bmic, conf.int = TRUE, exponentiate = TRUE)

# sex
cox.t4.sex = with(rdbdia.imputed, coxph(Surv(time, status=="1") ~ sex))
summary(pool(cox.t4.sex))
est.sex <- pool(cox.t4.sex)
summary(est.sex, conf.int = TRUE, exponentiate = TRUE)

# age
cox.t4.age = with(rdbdia.imputed, coxph(Surv(time, status=="1") ~ age))
summary(pool(cox.t4.age))
est.age <- pool(cox.t4.age)
summary(est.age, conf.int = TRUE, exponentiate = TRUE)

#--------------------------------------------------------------------------------

# TABLE 5
#table5 : multivariate analysis for all stroke events, non diabetic patients

cox.t5 = with(rdbdia.imputed, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 + apkd01 +
                                   bmic + sex + age))
summary(pool(cox.t5))
est.t5 <- pool(cox.t5)
summary(est.t5, conf.int = TRUE, exponentiate = TRUE)

#--------------------------------------------------------------------------------
#################################################################################
# SUMMARY OF ISCHEMIC AND HEMORRAGIC EVENTS 
#
#rdb <- rdb %>% 
#  mutate(hemorragic = case_when(
#    DGN_PAL == "I60"  ~ "1", 
#    DGN_PAL == "I61" ~ "1", 
#    DGN_PAL == "I62" ~ "1",
#    DGN_PAL != "I60" | DGN_PAL != "I61" | DGN_PAL != "I62" ~ "0"
#  )) 
# 
#rdb <- rdb %>% 
#  mutate(ischemic = case_when(
#    DGN_PAL == "G45"  ~ "1", 
#    DGN_PAL == "I63" ~ "1",
#    DGN_PAL != "G45" | DGN_PAL != "I63" ~ "0"
#  )) 
#################################################################################
#--------------------------------------------------------------------------------

# TABLES 6-9: ALL ISCHEMIC EVENTS 
# TO CREATE THE DATASET CONTAINING ALL THE ISCHEMIC EVENT plus THE SUBJECTS HAVING
# NO EVENT, YOU NEED TO EXCLUDE THE HEMORRAGIC == 1 
# THE VARIABLE HEMORRAGIC IS CODED AS 0=NO HEMORRAGIC EVENT (BUT ANOTHER TYPE OF EVENT)
# IS STILL POSSIBLE (E.G. ISCHEMIC) AND 1=HEMORRAGIC EVENT
# BY CREATING THE DATABASE BY INCLUDING ONLY HEMORRAGIC==0, YOU RULE OUT ALL THE 
# HEMORRAGIC EVENTS. TO CHECK THE PRESENCE OF THE ISCHEMIC EVENTS, PLEASE 
# TABLE THE VARIABLE status
rdb.toimput.i = subset(rdb, select = c(cardiovasc, tabac2, dial, apkd01, 
                                     sex, age, diabetes, bmic, status, time, hemorragic))

rdb.toimput.i$cardiovasc <- as.factor(rdb.toimput.i$cardiovasc)
rdb.toimput.i$tabac2 <- as.factor(rdb.toimput.i$tabac2)
rdb.toimput.i$apkd01 <- as.factor(rdb.toimput.i$apkd01)
rdb.toimput.i$sex <- as.factor(rdb.toimput.i$sex)
rdb.toimput.i$age <- as.numeric(as.character(rdb.toimput.i$age))
rdb.toimput.i$diabetes <- as.factor(rdb.toimput.i$diabetes) 
rdb.toimput.i$status <- as.factor(rdb.toimput$status)
rdb.toimput.i$time <- as.numeric(as.character(rdb.toimput.i$time))
rdb.toimput.i$bmic <- as.factor(rdb.toimput.i$bmic)
rdb.toimput.i$hemorragic <- as.factor(rdb.toimput.i$hemorragic)

rdb.toimput.i = rdb.toimput.i[(rdb.toimput.i$hemorragic=="0"),] 
count(rdb.toimput.i) # 44612 HEMORRAGIC IS EQUAL TO ZERO (ischemic event OR no event at all)

# eliminate the hemorragic variable from the dataset since it is not useful for the imputation
rdb.toimput.i = subset(rdb.toimput.i, select = -c(hemorragic))

rdbischemic.imputed<- mice(rdb.toimput.i, m=20, maxit=10, meth='pmm', seed=210586,
                      print = FALSE)

#--------------------------------------------------------------------------------
# TABLE 6
#table6: bivariate analysis for  ischemic stroke, all patients

# cardiovascular
cox.t8.cardiovasc = with(rdbischemic.imputed, coxph(Surv(time, status=="1") ~ cardiovasc))
summary(pool(cox.t8.cardiovasc))
est.card <- pool(cox.t8.cardiovasc)
summary(est.card, conf.int = TRUE, exponentiate = TRUE)

# tabac2
cox.t8.tab = with(rdbischemic.imputed, coxph(Surv(time, status=="1") ~ tabac2))
summary(pool(cox.t8.tab))
est.tab <- pool(cox.t8.tab)
summary(est.tab, conf.int = TRUE, exponentiate = TRUE)

# adpkd
cox.t8.adpkd = with(rdbischemic.imputed, coxph(Surv(time, status=="1") ~ apkd01))
summary(pool(cox.t8.adpkd))
est.adpkd <- pool(cox.t8.adpkd)
summary(est.adpkd, conf.int = TRUE, exponentiate = TRUE)

# bmic
cox.t8.bmic = with(rdbischemic.imputed, coxph(Surv(time, status=="1") ~ bmic))
summary(pool(cox.t8.bmic))
est.bmic <- pool(cox.t8.bmic)
summary(est.bmic, conf.int = TRUE, exponentiate = TRUE)

# sex
cox.t8.sex = with(rdbischemic.imputed, coxph(Surv(time, status=="1") ~ sex))
summary(pool(cox.t8.sex))
est.sex <- pool(cox.t8.sex)
summary(est.sex, conf.int = TRUE, exponentiate = TRUE)

# age
cox.t8.age = with(rdbischemic.imputed, coxph(Surv(time, status=="1") ~ age))
summary(pool(cox.t8.age))
est.age <- pool(cox.t8.age)
summary(est.age, conf.int = TRUE, exponentiate = TRUE)

# diabetes
cox.t8.dia = with(rdbischemic.imputed, coxph(Surv(time, status=="1") ~ diabetes))
summary(pool(cox.t8.dia))
est.dia <- pool(cox.t8.dia)
summary(est.dia, conf.int = TRUE, exponentiate = TRUE)

#--------------------------------------------------------------------------------

# TABLE 7
# table7: multivariate analysis for  ischemic stroke, all patients

cox.t7 = with(rdbischemic.imputed, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 + apkd01 +
                                           bmic + sex + age + diabetes))
summary(pool(cox.t7))
est.t7 <- pool(cox.t7)
summary(est.t7, conf.int = TRUE, exponentiate = TRUE)

#--------------------------------------------------------------------------------
# DATASET FOR ISCHEMIC ONLY EVENTS IN NON-DIABETIC POPULATION

rdb.toimput.ind = subset(rdb, select = c(cardiovasc, tabac2, dial, apkd01, 
                                     sex, age, diabetes, bmic, status, time, hemorragic))

rdb.toimput.ind$cardiovasc <- as.factor(rdb.toimput.ind$cardiovasc)
rdb.toimput.ind$tabac2 <- as.factor(rdb.toimput.ind$tabac2)
rdb.toimput.ind$apkd01 <- as.factor(rdb.toimput.ind$apkd01)
rdb.toimput.ind$sex <- as.factor(rdb.toimput.ind$sex)
rdb.toimput.ind$age <- as.numeric(as.character(rdb.toimput.ind$age))
#rdb.toimput.ind$diabetes <- as.factor(rdb.toimput.ind$diabetes) # it is necessary to keep the variable
#       "diabetes" as "int" if you wanna use the "is.na" function and exclude efficently 
#       all subjects presenting a diabetes or not having this information 
#       transforming the variable into a factor (AS IT IS NECESSARY IN THE GENERAL ANALYSIS)
#       won't allow to create the "only non diabetic patients" dataset
rdb.toimput.ind$status <- as.factor(rdb.toimput.ind$status)
rdb.toimput.ind$time <- as.numeric(as.character(rdb.toimput.ind$time))
rdb.toimput.ind$bmic <- as.factor(rdb.toimput.ind$bmic)

table(rdb.toimput.ind$hemorragic)
rdb.toimput.ind = rdb.toimput.i[(rdb.toimput.ind$hemorragic=="0"),] 
count(rdb.toimput.ind) #44612

# DIABETES VARIABLE 
rdb.toimput.ind$diabetes = as.numeric(as.character(rdb.toimput.ind$diabetes))
rdb.toimput.ind$diabetes[is.na(rdb.toimput.ind$diabetes)] <- "2"
table(rdb.toimput.ind$diabetes)

rdb.toimput.inddiab = rdb.toimput.ind[(rdb.toimput.ind$diabetes=="0"),] # this will NOT includes the NA
count(rdb.toimput.inddiab) # 22527
rdbdia.ind.imputed<- mice(rdb.toimput.inddiab, m=20, maxit=10, meth='pmm', seed=210586,
                      print = FALSE)

#--------------------------------------------------------------------------------
# TABLE 8
# table8: bivariate analysis for ischemic stroke, non diabetic patients

# cardiovascular
cox.t8.cardiovasc = with(rdbdia.ind.imputed, coxph(Surv(time, status=="1") ~ cardiovasc))
summary(pool(cox.t8.cardiovasc))
est.card <- pool(cox.t8.cardiovasc)
summary(est.card, conf.int = TRUE, exponentiate = TRUE)

# tabac2
cox.t8.tab = with(rdbdia.ind.imputed, coxph(Surv(time, status=="1") ~ tabac2))
summary(pool(cox.t8.tab))
est.tab <- pool(cox.t8.tab)
summary(est.tab, conf.int = TRUE, exponentiate = TRUE)

# adpkd
cox.t8.adpkd = with(rdbdia.ind.imputed, coxph(Surv(time, status=="1") ~ apkd01))
summary(pool(cox.t8.adpkd))
est.adpkd <- pool(cox.t8.adpkd)
summary(est.adpkd, conf.int = TRUE, exponentiate = TRUE)

# bmic
cox.t8.bmic = with(rdbdia.ind.imputed, coxph(Surv(time, status=="1") ~ bmic))
summary(pool(cox.t8.bmic))
est.bmic <- pool(cox.t8.bmic)
summary(est.bmic, conf.int = TRUE, exponentiate = TRUE)

# sex
cox.t8.sex = with(rdbdia.ind.imputed, coxph(Surv(time, status=="1") ~ sex))
summary(pool(cox.t8.sex))
est.sex <- pool(cox.t8.sex)
summary(est.sex, conf.int = TRUE, exponentiate = TRUE)

# age
cox.t8.age = with(rdbdia.ind.imputed, coxph(Surv(time, status=="1") ~ age))
summary(pool(cox.t8.age))
est.age <- pool(cox.t8.age)
summary(est.age, conf.int = TRUE, exponentiate = TRUE)

#--------------------------------------------------------------------------------
# TABLE 9
# table9: multivariate analysis for ischemic stroke, non diabetic patients

cox.t9 = with(rdbdia.ind.imputed, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 + apkd01 +
                                           bmic + sex + age))
summary(pool(cox.t9))
est.t9 <- pool(cox.t9)
summary(est.t9, conf.int = TRUE, exponentiate = TRUE)

#--------------------------------------------------------------------------------
#HEMORRHAGIC STROKE

# TABLES 10-11: ALL HEMORRAGIC EVENTS 
# SEE TABLES 6-9 FOR EXPLANATION
rdb.toimput.h = subset(rdb, select = c(cardiovasc, tabac2, dial, apkd01, 
                                       sex, age, diabetes, bmic, status, time, ischemic))

rdb.toimput.h$cardiovasc <- as.factor(rdb.toimput.h$cardiovasc)
rdb.toimput.h$tabac2 <- as.factor(rdb.toimput.h$tabac2)
rdb.toimput.h$apkd01 <- as.factor(rdb.toimput.h$apkd01)
rdb.toimput.h$sex <- as.factor(rdb.toimput.h$sex)
rdb.toimput.h$age <- as.numeric(as.character(rdb.toimput.h$age))
rdb.toimput.h$diabetes <- as.factor(rdb.toimput.h$diabetes) 
rdb.toimput.h$status <- as.factor(rdb.toimput$status)
rdb.toimput.h$time <- as.numeric(as.character(rdb.toimput.h$time))
rdb.toimput.h$bmic <- as.factor(rdb.toimput.h$bmic)
rdb.toimput.h$ischemic <- as.factor(rdb.toimput.h$ischemic)

table(rdb.toimput.h$ischemic)
#     0     1 
# 43677  1349 

rdb.toimput.h = rdb.toimput.h[(rdb.toimput.h$ischemic=="0"),] 
count(rdb.toimput.h) # 43677 ISCHEMIC IS EQUAL TO ZERO (hemorragic event OR no event at all)

# eliminate the ischemic variable from the dataset since it is not useful for the imputation
rdb.toimput.h = subset(rdb.toimput.h, select = -c(ischemic))

rdbhemorragic.imputed<- mice(rdb.toimput.h, m=20, maxit=10, meth='pmm', seed=210586,
                           print = FALSE)

#--------------------------------------------------------------------------------
#table 10: bivariate analysis for hemorrhagic stroke, all patients

# cardiovascular
cox.t10.cardiovasc = with(rdbhemorragic.imputed, coxph(Surv(time, status=="1") ~ cardiovasc))
summary(pool(cox.t10.cardiovasc))
est.card <- pool(cox.t10.cardiovasc)
summary(est.card, conf.int = TRUE, exponentiate = TRUE)

# tabac2
cox.t10.tab = with(rdbhemorragic.imputed, coxph(Surv(time, status=="1") ~ tabac2))
summary(pool(cox.t10.tab))
est.tab <- pool(cox.t10.tab)
summary(est.tab, conf.int = TRUE, exponentiate = TRUE)

# adpkd
cox.t10.adpkd = with(rdbhemorragic.imputed, coxph(Surv(time, status=="1") ~ apkd01))
summary(pool(cox.t10.adpkd))
est.adpkd <- pool(cox.t10.adpkd)
summary(est.adpkd, conf.int = TRUE, exponentiate = TRUE)

# bmic
cox.t10.bmic = with(rdbhemorragic.imputed, coxph(Surv(time, status=="1") ~ bmic))
summary(pool(cox.t10.bmic))
est.bmic <- pool(cox.t10.bmic)
summary(est.bmic, conf.int = TRUE, exponentiate = TRUE)

# sex
cox.t10.sex = with(rdbhemorragic.imputed, coxph(Surv(time, status=="1") ~ sex))
summary(pool(cox.t10.sex))
est.sex <- pool(cox.t10.sex)
summary(est.sex, conf.int = TRUE, exponentiate = TRUE)

# age
cox.t10.age = with(rdbhemorragic.imputed, coxph(Surv(time, status=="1") ~ age))
summary(pool(cox.t10.age))
est.age <- pool(cox.t10.age)
summary(est.age, conf.int = TRUE, exponentiate = TRUE)

# diabetes
cox.t10.dia = with(rdbhemorragic.imputed, coxph(Surv(time, status=="1") ~ diabetes))
summary(pool(cox.t10.dia))
est.dia <- pool(cox.t10.dia)
summary(est.dia, conf.int = TRUE, exponentiate = TRUE)


#--------------------------------------------------------------------------------
#table 11: multivariate analysis for hemorrhagic stroke, all patients

cox.t11 = with(rdbhemorragic.imputed, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                              apkd01 +
                                          bmic + sex + age + diabetes))
summary(pool(cox.t11))
est.t11 <- pool(cox.t11)
summary(est.t11, conf.int = TRUE, exponentiate = TRUE)
#--------------------------------------------------------------------------------
# HAEMORRAGIC STROKE NON-DIABETIC POPULATION 

rdb.toimput.h = subset(rdb, select = c(cardiovasc, tabac2, dial, apkd01, 
                                       sex, age, diabetes, bmic, status, time, ischemic))

rdb.toimput.h$cardiovasc <- as.factor(rdb.toimput.h$cardiovasc)
rdb.toimput.h$tabac2 <- as.factor(rdb.toimput.h$tabac2)
rdb.toimput.h$apkd01 <- as.factor(rdb.toimput.h$apkd01)
rdb.toimput.h$sex <- as.factor(rdb.toimput.h$sex)
rdb.toimput.h$age <- as.numeric(as.character(rdb.toimput.h$age))
#rdb.toimput.h$diabetes <- as.factor(rdb.toimput.h$diabetes) 
rdb.toimput.h$status <- as.factor(rdb.toimput$status)
rdb.toimput.h$time <- as.numeric(as.character(rdb.toimput.h$time))
rdb.toimput.h$bmic <- as.factor(rdb.toimput.h$bmic)
rdb.toimput.h$ischemic <- as.factor(rdb.toimput.h$ischemic)

table(rdb.toimput.h$ischemic)
#     0     1 
# 43677  1349 

rdb.toimput.h = rdb.toimput.h[(rdb.toimput.h$ischemic=="0"),] 
count(rdb.toimput.h) # 43677 ISCHEMIC IS EQUAL TO ZERO (hemorragic event OR no event at all)

# eliminate the ischemic variable from the dataset since it is not useful for the imputation
rdb.toimput.h = subset(rdb.toimput.h, select = -c(ischemic))

# THE diabetic VARIABLE
rdb.toimput.h$diabetes = as.numeric(as.character(rdb.toimput.h$diabetes))
rdb.toimput.h$diabetes[is.na(rdb.toimput.h$diabetes)] <- "2"
table(rdb.toimput.h$diabetes)
#     0     1     2 
# 22341 19440  1896 

rdb.toimput.hdia = rdb.toimput.h[(rdb.toimput.h$diabetes==0),] # this will NOT includes the NA
count(rdb.toimput.hdia) # 22341

rdbhaemdia.imputed<- mice(rdb.toimput.hdia, m=20, maxit=10, meth='pmm', seed=210586,
                      print = FALSE)
#--------------------------------------------------------------------------------
#table12: bivariate analysis for haemorrhagic stroke, non diabetic patients

# cardiovascular
cox.t12.cardiovasc = with(rdbhaemdia.imputed, coxph(Surv(time, status=="1") ~ cardiovasc))
summary(pool(cox.t12.cardiovasc))
est.card <- pool(cox.t12.cardiovasc)
summary(est.card, conf.int = TRUE, exponentiate = TRUE)

# tabac2
cox.t12.tab = with(rdbhaemdia.imputed, coxph(Surv(time, status=="1") ~ tabac2))
summary(pool(cox.t12.tab))
est.tab <- pool(cox.t12.tab)
summary(est.tab, conf.int = TRUE, exponentiate = TRUE)

# adpkd
cox.t12.adpkd = with(rdbhaemdia.imputed, coxph(Surv(time, status=="1") ~ apkd01))
summary(pool(cox.t12.adpkd))
est.adpkd <- pool(cox.t12.adpkd)
summary(est.adpkd, conf.int = TRUE, exponentiate = TRUE)

# bmic
cox.t12.bmic = with(rdbhaemdia.imputed, coxph(Surv(time, status=="1") ~ bmic))
summary(pool(cox.t12.bmic))
est.bmic <- pool(cox.t12.bmic)
summary(est.bmic, conf.int = TRUE, exponentiate = TRUE)

# sex
cox.t12.sex = with(rdbhaemdia.imputed, coxph(Surv(time, status=="1") ~ sex))
summary(pool(cox.t12.sex))
est.sex <- pool(cox.t12.sex)
summary(est.sex, conf.int = TRUE, exponentiate = TRUE)

# age
cox.t12.age = with(rdbhaemdia.imputed, coxph(Surv(time, status=="1") ~ age))
summary(pool(cox.t12.age))
est.age <- pool(cox.t12.age)
summary(est.age, conf.int = TRUE, exponentiate = TRUE)


#table13: multivariate analysis for haemorrhagic stroke, non diabetic patients

cox.t13 = with(rdbhaemdia.imputed, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                              apkd01 +
                                              bmic + sex + age))
summary(pool(cox.t13))
est.t13 <- pool(cox.t13)
summary(est.t13, conf.int = TRUE, exponentiate = TRUE)

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# IMPUTATION FOR DESCRIPTIVE ANALYSIS 
# cardivascular is set as: ICn ICOROn IDMn RYTHMn ANEVn

rdb.global = subset(rdb, select = c(#URGn, KTTINIn, EPOINIn,  
                              nephgp, METHOn, #techn, MODALn, VAVn, traitement, 
                              IRCn, O2n, AMIn, AVCAITn, KCn, 
                              VHBn, VHCn, CIRHn, VIHn, 
                              HANDn, AMPn, 
                              bmic, tabac2, 
                              iresp, sero, coro, foie, #comCV, comcvcl, comcvcl2, 
                              sex, age, groupes6,
                              #groupes6_CA1, groupes6_CA2, 
                              #MOTIF_An, CPKMEDn, REFUSn,  
                              #DGN_PAL, 
                              DEATH, hemorragic, 
                              ischemic, status, apkd01, cardiovasc))
count(rdb.global)
count(rdb)

md.pattern(rdb.global)

global.imputed <- mice(rdb.global, m=20, maxit=10, meth='pmm', seed=210586,
                          print = FALSE)
