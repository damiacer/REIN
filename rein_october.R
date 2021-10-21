count(rein)
#View(rein)
names(rein)

#-------------------------------------------------------------------------------

# RENAME COLUMNS
# WARNING: THIS IS NOT NECESSARY ON MAC
rein <- as_tibble(rein)
rein <- rein %>% rename(
  # new name = old name,
  "prttturg" = "ï..URGn")

#-------------------------------------------------------------------------------

# ADD LABELS TO VARIABLES
rein = apply_labels(rein,
                    URGn = "Premier traitement en urgence", #MAC
                    prttturg = "Premier traitement en urgence", #WINDOWS
                    KTTINIn = "1ère séance d'hémodialyse réalisée avec cathéter",
                    EPOINIn = "Traitement par Erythropoietine",
                    liste_longue = "Regroupement détaillé néphropathie",
                    nephgp = "Regroupement en 8 classes néphropathie",
                    METHOn = "Traitement 3 classes",
                    techn = "Méthode de traitement",
                    MODALn = "Modalité de traitement",
                    VAVn = "Voie d'abord vasculaire",
                    traitement = "Traitement (concaténation TECHN et MODAL)",
                    PDS = "Poids",
                    TAIL = "Taille",
                    IRCn = "Insuffisance respiratoire chronique",          
                    O2n = "Oxygénothérapie",   
                    ICn = "Insuffisance cardiaque",
                    ICOROn = "Insuffisance coronarienne",
                    IDMn = "Infarctus du myocarde",
                    RYTHMn = "Troubles du rythme",
                    ANEVn = "Anevrysme de l'aorte abdominale",
                    AMIn = "Artérite des membres inférieurs",
                    AVCAITn = "Variable composite de AVC et AIT",
                    KCn = "Cancer évolutif",
                    VHBn = "Ag HBS positif",
                    VHCn = "PCR VHC positif", 
                    CIRHn = "Cirrhose",
                    VIHn = "VIH",
                    SIDAn = "SIDA",
                    HANDn = "Au moins un handicap",
                    AMPn = "Amputation membres inférieurs",
                    PLEGn = "Paraplégie/Hémiplégie",
                    CECITEn = "Cécité",
                    COMPORTn = "Troubles du comportement",
                    TYPDIABn = "Type de diabète",
                    STADICn = "Stade de l'insuffisance cardiaque",
                    STDAMIn = "Stade de l'artérite des membres inférieurs",
                    STDCIRHn = "Stade de la cirrrhose",
                    TABACn = "Statut tabagique 0-1-2",
                    bmi = "IMC",
                    tabac2 = "Statut tabagique 0-1",   
                    iresp = "Variable composite de O2 et IRC",   
                    sero = "Variable composite de VIH et SIDA",
                    coro = "Variable composite de ICORO et IDM",
                    foie = "VariablSe composite de CIRH, VHB, VHC",
                    comCV = "Nb de comorbidités  cardiovasc sur 6 chez les patients avec TOUTES LES VAR RENSEIGNEES",
                    comcvcl = "Au moins une comorbidité cardiovasculaire",
                    comcvcl2 = "Nb de comorbidités cardiovasculaires en 3 classes sur 6 comorbidités",
                    sex = "Sexe",
                    age = "Age à l'initiation du traitement de suppléance",    
                    ETAT_DERNOUV2019 = "Etat aux dernières nouvelles avant 31/12/2019",
                    delai_IRT = "Délai insuffisance rénale terminale",
                    delai_DC = "Délai décès",
                    delai_TX = "",
                    delai_SVR = "Délai de sevrage (par récupération de la fonc rénale, soit en fin d vie)",     
                    delai_PDV = "Délai de perdue de vue",       
                    delai_DERNOUV2019 = "Délai dernières nouvelles 2019",
                    groupes6 = "Regroupement causes de décès 6 groupes", 
                    categories18 = "Regroupement causes de décès 18 groupes",
                    groupes6_CA1 = "Regroupement causes de décès associée 1 en 6 groupes",
                    categories18_CA1 = "Regroupement causes de décès associée 1 en 18 groupes",
                    groupes6_CA2 = "Regroupement causes de décès associée 2 en 6 groupes",
                    categories18_CA2 = "Regroupement causes de décès associée 2 en 18 groupes",
                    MOTIF_An = "Motif d'arrêt de la dialyse",
                    CPKMEDn = "Fin de traitement pour complication médicale",
                    REFUSn = "Fin de traitement par refus du patient",
                    DDC = "Date de décès",
                    DINSCMED = "Date de la première inscription sur la liste transplantation",
                    DDIRT = "Date de l'insuffisance rénale terminale",
                    DGRF = "Date de greffe",          
                    DSVR = "Date de sevrage (par récupération de la fonc rénale, soit en fin d vie)",
                    DPDV = "Date de perdu de vue",
                    DATE_DERNOUV2019 = "Date de dernières nouvelles 2019",
                    RREC_COD_ANO = "Code anonym"
)

# RECALL LABELS (BY VAR NAME OR VAR POSITION)
var_lab(rein[12])

# MERGING THE DATABASES 
# THE FULL LIST OF DATABASES ARE UNDER DATALISTING
# FOR MORE INFOS FOR THE MERGING FUNCTIONS USED BELOW, VISIT:
## https://www.infoworld.com/article/3454356/how-to-merge-data-in-r-using-r-merge-dplyr-or-datatable.html

# THIS FUNCTION IS AVAILABE UNDER "data.table" OR IN BASE PACKAGE
?merge

#RUN THE TIBBLE BEFORE
names(rein)
names(rein_s) # "rein_s" is the "accrochage" database
names(hosp) 

# COUNT DATASETS
count(rein)
# A tibble: 1 x 1
# 45026
count(rein_s)
# A tibble: 1 x 1
# 45026
count(hosp)
# 18543

# MERGING "ACCROCHAGE" FOR SNDS AND REIN DATA
# CHECK THE "accrochage.R" BEFORE LAUNCHING THIS MERGING ON WINDOWS
rein_m1 <- merge(rein, rein_s, by.x = "RREC_COD_ANO", by.y = "RREC_COD_ANO")
count(rein_m1)
# 45026

is.data.table(rein_m1)

# RENAME COLUMNS FOR "REIN_M1"
# WARNING: THIS IS NOT NECESSARY ON MAC
rein_m1 <- as_tibble(rein_m1)
rein_m1 <- rein_m1 %>% rename(
  # new name = old name,
  "num_enq" = "ï..num_enq")

# MERGING REIN_M1 WITH HOSPITALISATION DATA (MORE LINES PER SUBJECT)
# THIS PROCESS USES THE "id" TO MERGE DATABASES 
# TO KEEP ONLY FULL CORRESPONDANCE USE THE FOLLOWING PROCEDURE
count(hosp)

# RENAME COLUMNS FOR "HOSP"
# WARNING: THIS IS NOT NECESSARY ON MAC
hosp <- as_tibble(hosp)
hosp <- hosp %>% rename(
  # new name = old name,
  "num_enq" = "ï..num_enq")

# CHECK THE "hospdata.R" BEFORE LAUNCHING THIS MERGING ON WINDOWS

rein_m2 <- merge(rein_m1, hosp, by.x = "num_enq", by.y = "num_enq",
                 all.x = TRUE, all.y = FALSE)
count(rein_m2)
# 52724
# NEW DATABASE HAS MULTIPLE LINES FOR THE SAME SUBJECT ACCORDING TO THE 
# View(rein_m2)

#-------------------------------------------------------------------------------

# THE VARIABLE DGN_PAL DEFINES THE DIAGNOSIS 
# SOME SUBJECTS CAN HAVE MORE THAN ONE LINE SINCE THEY HAVE MORE THAN ONE EVENT 
rein_m2$DGN_PALB[rein_m2$DGN_PAL==""] <- "0"
rein_m2$DGN_PALB[rein_m2$DGN_PAL!=""] <- "1"
table(rein_m2$DGN_PALB)
table(rein_m2$DGN_PALB, rein_m2$DGN_PAL)
#is.na(rein_m2$DGN_PALB)

# TEST TO ELIMINATE THE DOUBLED LINES
# dgn_data <- rein_m2[,c("RREC_COD_ANO", "num_enq", "DGN_PAL", "DGN_PALB", "SOR_ANN", "SOR_MOI")]
# View(dgn_data)
dgn_data <- rein_m2
names(dgn_data)
count(dgn_data) # THIS SHOULD BE THE SAME AS "rein_m2"

# VARIABLE TO VERIFY THE EVENT 
table(dgn_data$DGN_PALB)

# CREATE THE BASE WITH ONLY EVENTS 
# IN THIS DATABASE, ONE PATIENT CAN HAVE MORE THAN ONE EVENT 
# AND PATIENTS FROM THE ORIGINAL DATABASES (BUT WHO HAVE NO EVENT), COMPLETELY MISSING
dgn_complete = dgn_data[complete.cases(dgn_data[ , "DGN_PAL"]), ]
count(dgn_complete)
# 18543

#-------------------------------------------------------------------------------

table(dgn_complete$SOR_MOI)
str(dgn_complete$SOR_MOI)

dgn_complete$SOR_MOI[dgn_complete$SOR_MOI=="1"] <- "01"
dgn_complete$SOR_MOI[dgn_complete$SOR_MOI=="2"] <- "02"
dgn_complete$SOR_MOI[dgn_complete$SOR_MOI=="3"] <- "03"
dgn_complete$SOR_MOI[dgn_complete$SOR_MOI=="4"] <- "04"
dgn_complete$SOR_MOI[dgn_complete$SOR_MOI=="5"] <- "05"
dgn_complete$SOR_MOI[dgn_complete$SOR_MOI=="6"] <- "06"
dgn_complete$SOR_MOI[dgn_complete$SOR_MOI=="7"] <- "07"
dgn_complete$SOR_MOI[dgn_complete$SOR_MOI=="8"] <- "08"
dgn_complete$SOR_MOI[dgn_complete$SOR_MOI=="9"] <- "09"
dgn_complete$SOR_MOI[dgn_complete$SOR_MOI=="10"] <- "10"
dgn_complete$SOR_MOI[dgn_complete$SOR_MOI=="11"] <- "11"
dgn_complete$SOR_MOI[dgn_complete$SOR_MOI=="12"] <- "12"

#-------------------------------------------------------------------------------

# CREATE A DAY FOR EVERY ENTRY IN THE DATABASE
# EVERY ENTRY DAY WILL BE SET TO 15
dgn_complete$SOR_JJ <- rep(15, times=18543)

# install.packages("lubridate")
library("lubridate")
# IF NECESSARY (WINDOWS): library(lubridate, warn.conflicts = FALSE)
# MORE ON LUBRIDATE: https://lubridate.tidyverse.org/

# CREATE A NEW VARIABLE FOR DATE
# THE CODE DOES NOT RECOGNIZE THE sep = "" COMMAND. DATE CANNOT BE AUTOMATICALLY CREATED
dgn_complete$eventfulldate <- as.numeric(paste(dgn_complete$SOR_ANN, dgn_complete$SOR_MOI, 
                                               dgn_complete$SOR_JJ, sep = ""))
table(dgn_complete$eventfulldate)
dgn_complete$evdate = ymd(dgn_complete$eventfulldate)
table(dgn_complete$evdate)
str(dgn_complete$evdate)

# is.na(dgn_complete$eventdate)
# library(tidyverse)
# dgn_complete %>% unite("eventdate", "SOR_ANN","SOR_MOI", sep = "")
# table(dgn_complete$eventdate)

#-------------------------------------------------------------------------------

# SELECT FIRST OCCURRENCE BY TIME DATE
# STACK: https://stackoverflow.com/questions/54525745/r-select-first-occurrence-by-time-date-for-multiple-ids
# COMMENT: the following function does not actually create a new database selecting only one occurrence, but
# it helps creating a new dataset with only select() variables and ordered by date.
# the selection is run by dplyr after a counting variable is added to the dataset. 
# the following step could be avoided, though ordering the data makes the upcoming steps run smoother

dgn_one <- dgn_complete %>%
  group_by(num_enq, lubridate::date(evdate)) %>%
  arrange(evdate) %>%
  slice(1) %>%
  ungroup() %>%
  select(evdate, num_enq, DGN_PAL)

is.data.table(dgn_one)
# View(dgn_one)

# ADD A COUNT NUMBER FOR EACH EVENT FOR EACH SUBJECT
# THE FOLLWING SQL FUNCTION WILL CREATE A NEW COULM CALLED "count"
# THE NAME OF THE NEW COLUMN/VARIABLE CAN BE CHANGED IN THE SQL FUNCION

# install.packages("sqldf") # INSTALL ON R WHEN WORKING ON PC
library("sqldf")
dgn_oneclassifier <- sqldf("SELECT a.*, COUNT(*) count
       FROM dgn_one a, dgn_one b 
       WHERE a.num_enq = b.num_enq AND b.ROWID <= a.ROWID 
       GROUP BY a.ROWID"
)

is.data.table(dgn_oneclassifier)
# View(dgn_oneclassifier)
names(dgn_oneclassifier)
count(dgn_oneclassifier)
# 18543

# NEW DATABASE WITH ONLY THE FIRST EVENT PER PERSON
# THE "filter" FUNCTION REQUIRES dplyr OR tidyverse

dgn_line = filter(dgn_oneclassifier, count == 1)
count(dgn_line)

#-------------------------------------------------------------------------------

# SORT DATA BEFORE ERASE DUPLICATES
# sort(dgn_complete$eventfulldate, increasing = TRUE)
# sort.int(dgn_complete$eventdate, decreasing = FALSE, index.return = TRUE)

# DUPLICATE CAN BE IDENTIFIED BY "RREC_COD_ANO" OR "num_enq"
# duplicated(dgn_complete$RREC_COD_ANO)
# FIND THE DUPLICATES
# dgn_complete$RREC_COD_ANO[duplicated(dgn_complete$RREC_COD_ANO)]
# ERASE DUPLICATED LINES ACCORDING TO ONE VALUE 
# dgn_one<-dgn_complete[!duplicated(dgn_complete$RREC_COD_ANO, fromLast = TRUE),]
# count(dgn_one)

# dgn_dup = dgn_complete[duplicated(dgn_complete$RREC_COD_ANO),]
# dgn_duptest = dgn_dup[c(1:250),]
# count(dgn_dup)
# dgn_onetest = dgn_one[c(1:250),]
# THE DOUBLE-LINE DATABASE IS ON THE X
# THE SINGLE-LINE DATABASE IS ON THE Y
# table(dgn_duptest$SOR_ANN, dgn_onetest$SOR_ANN)

################################################################################

# FINAL DATABASE (REIN + HOSP) WITHOUT REPLICATION
# n = 45 026

# MERGE BASES WITH THE HOSPITALIZATION DATA 
# this database contains only one hospital event per person
count(rein_m1)
count(dgn_line)
rein_mone <- merge(rein_m1, dgn_line, by.x = "num_enq", by.y = "num_enq",
                   all.x = TRUE, all.y = FALSE)
count(rein_mone)
# 45026

# NUMBER THE REPLICATED LINES
# COMMENT: THIS IS NOT NECESSARY
#library(dplyr)
# rein_m2c <- rein_m2 %>%
#  group_by(num_enq) %>%
#  mutate(replicate=seq(n))

# COUNT THE NUMBER OF DUPLICATED PATIENTS (LINES) IN THE DATASET
# COMMENT: THIS FUNCTION USES THE data.table PACKAGE, WHICH IS NOT PROPERLY WORKING
# ON MACBOOK BECAUSE OF THE "MP" COMPONENT
#library(data.table)
# rein_m2d <- rein_m2
# setDT(rein_m2d)[, .N, id]
# sum(duplicated(rein_m2d$id))

################################################################################

# DATA ANALYSIS USING "rein_mone"
# THIS DATABASE CONTAINS ALL DATA FROM ALL PATIENTS + THEIR HOSPITALISATION
# IT DOES NOT CONTAIN DRUG DATA
count(rein_mone)
# 45026
names(rein_mone)
table(rein_mone$nephgp)

rein_mone$apkd01[rein_mone$nephgp=="APKD"] <- "1"
rein_mone$apkd01[rein_mone$nephgp=="autre"] <- "0"
rein_mone$apkd01[rein_mone$nephgp=="diabète"] <- "0"  #MACBOOK
rein_mone$apkd01[rein_mone$nephgp=="diabÃ¨te"] <- "0" #PC
rein_mone$apkd01[rein_mone$nephgp=="gnc"] <- "0"
rein_mone$apkd01[rein_mone$nephgp=="HTA"] <- "0"
rein_mone$apkd01[rein_mone$nephgp=="Inconnu"] <- "0"
rein_mone$apkd01[rein_mone$nephgp=="pyelo"] <- "0"
rein_mone$apkd01[rein_mone$nephgp=="vasc"] <- "0"
rein_mone$apkd01[rein_mone$nephgp==""] <- "0"
table(rein_mone$apkd01)

# CREATE A NEW DATABASE ACCORDING TO THE APKD01
apkd<-rein_mone[!(rein_mone$apkd01=="0"),]
count(apkd)
# 2560

# apkd <- select(apdk,-c("apdk01"))

# SAVE THE DATABASE
# more on saving and loading in R: 
# https://www.r-bloggers.com/2019/05/how-to-save-and-load-datasets-in-r-an-overview/

# save(apkd, file = "apkd.RData")
# write.table(apkd, file = "apkd.csv", sep = ",", row.names = F)

# SAVE A .csv FILE WITH TIDYVERSE

install.packages("reader")
library("reader")

write_csv2(apkd, "P:/UBRC_M2/REYES/ANALYSIS/DATABASES/csv_data/apkd_reader.csv")

################################################################################
################################################################################
################################################################################

# CREATE THE VARIABLE EVENT FOR THE NEW DATASET
# WARNING: THE NEW VARIABLE WILL LIST EVENTS BEFORE AND AFTER THE INCLUSION
# EVENTS AFTER THE INCLUSION WILL BE ERASED

m <- as.data.frame(apkd$DGN_PAL)
m[is.na(m)] <- "E0"
table(m)
apkd$grouping <- m
apkd$EVENT[apkd$grouping == "E0"]  <- 0
apkd$EVENT[apkd$grouping != "E0"]  <- 1
tabev <- table(apkd$EVENT)
prop.table(tabev)
apkd$EVENT <- as.character(apkd$EVENT)
str(apkd$EVENT)
table(apkd$EVENT)

d <- as.data.frame(apkd$delai_DC)
d[is.na(d)] <- "0"
apkd$deathbinary <- d
apkd$DEATH[apkd$deathbinary == 0] <- "0"
apkd$DEATH[apkd$deathbinary > 0] <- "1"
tabde <- table(apkd$DEATH)
prop.table(tabde)

apkd$DGRF.d = as.numeric(as.Date(apkd$DGRF, "%d/%m/%Y"))
t <- as.data.frame(apkd$DGRF.d)
t[is.na(t)] <- "0"
apkd$transplantationbinary <- t
apkd$TRANSP[apkd$transplantationbinary == 0] <- "0"
apkd$TRANSP[apkd$transplantationbinary > 0] <- "1"
table(apkd$TRANSP)

#-------------------------------------------------------------------------------

# RECODE THE "apkd$DGN_PAL" VARIABLE  (DIAGNOSIS)

apkd$DGN_PALs[apkd$DGN_PAL=="G450"] = "G45"
apkd$DGN_PALs[apkd$DGN_PAL=="G451"] = "G45"  
apkd$DGN_PALs[apkd$DGN_PAL=="G452"] = "G45"
apkd$DGN_PALs[apkd$DGN_PAL=="G453"] = "G45"
apkd$DGN_PALs[apkd$DGN_PAL=="G454"] = "G45"
apkd$DGN_PALs[apkd$DGN_PAL=="G458"] = "G45"
apkd$DGN_PALs[apkd$DGN_PAL=="G459"] = "G45"
apkd$DGN_PALs[apkd$DGN_PAL=="G464"] = "G46"
apkd$DGN_PALs[apkd$DGN_PAL=="G8101"] = "G46"
apkd$DGN_PALs[apkd$DGN_PAL=="I601"] = "I60"
apkd$DGN_PALs[apkd$DGN_PAL=="I602"] = "I60"
apkd$DGN_PALs[apkd$DGN_PAL=="I605"] = "I60"
apkd$DGN_PALs[apkd$DGN_PAL=="I607"] = "I60"
apkd$DGN_PALs[apkd$DGN_PAL=="I608"] = "I60"
apkd$DGN_PALs[apkd$DGN_PAL=="I609"] = "I60"
apkd$DGN_PALs[apkd$DGN_PAL=="I610"] = "I61"
apkd$DGN_PALs[apkd$DGN_PAL=="I611"] = "I61"
apkd$DGN_PALs[apkd$DGN_PAL=="I612"] = "I61"
apkd$DGN_PALs[apkd$DGN_PAL=="I615"] = "I61"
apkd$DGN_PALs[apkd$DGN_PAL=="I616"] = "I61"
apkd$DGN_PALs[apkd$DGN_PAL=="I618"] = "I61"
apkd$DGN_PALs[apkd$DGN_PAL=="I619"] = "I61" 
apkd$DGN_PALs[apkd$DGN_PAL=="I620"] = "I62"
apkd$DGN_PALs[apkd$DGN_PAL=="I629"] = "I62"
apkd$DGN_PALs[apkd$DGN_PAL=="I630"] = "I63"
apkd$DGN_PALs[apkd$DGN_PAL=="I631"] = "I63"
apkd$DGN_PALs[apkd$DGN_PAL=="I632"] = "I63"
apkd$DGN_PALs[apkd$DGN_PAL=="I633"] = "I63"
apkd$DGN_PALs[apkd$DGN_PAL=="I634"] = "I63"
apkd$DGN_PALs[apkd$DGN_PAL=="I635"] = "I63"
apkd$DGN_PALs[apkd$DGN_PAL=="I638"] = "I63"
apkd$DGN_PALs[apkd$DGN_PAL=="I639"] = "I63"
apkd$DGN_PALs[apkd$DGN_PAL=="I64"] = "I64" 
apkd$DGN_PALs[apkd$DGN_PAL=="I742"] = "I74"
apkd$DGN_PALs[apkd$DGN_PAL=="I743"] = "I74"
apkd$DGN_PALs[apkd$DGN_PAL=="I744"] = "I74"
apkd$DGN_PALs[apkd$DGN_PAL=="I745"] = "I74"
apkd$DGN_PALs[apkd$DGN_PAL=="I748"] = "I74"
apkd$DGN_PALs[apkd$DGN_PAL=="R4701"] = "R471"
apkd$DGN_PALs[apkd$DGN_PAL=="R471"] = "R471"  

DGN_PALSstab = table(apkd$DGN_PALs)
prop.table(DGN_PALSstab)

################################################################################

# TRANSFORM DATE BEFORE THE FIRST INCLUSION (1/1/2015) IN MISSING VALUES 

library("dplyr")
library("lubridate")

apkd$inclusion.d = as.Date(apkd$DDIRT, "%d/%m/%Y")
apkd$transplantation.d = as.Date(apkd$DGRF, "%d/%m/%Y")
apkd$event.d = as.Date(apkd$evdate, "%d/%m/%Y")

#-------------------------------------------------------------------------------

# FILTER EVENTS AFTER INCLUSION
hap.data1 <- apkd %>% 
  select(event.d, num_enq, RREC_COD_ANO, inclusion.d, transplantation.d) %>%
  filter(event.d > inclusion.d)
is.data.frame(hap.data1)
count(hap.data1) # n=261 
# THIS DATASET CONTAINS ONLY SUBJECTS UNDERGOING THE 
# EVENT AFTER THE INCLUSION

#-------------------------------------------------------------------------------

# FILTER TRANSPLANTATION AFTER EVENT
hap.data2 <- apkd %>%
  select(event.d, num_enq, RREC_COD_ANO, inclusion.d, transplantation.d) %>%
  filter(transplantation.d > event.d)
is.data.frame(hap.data2)
count(hap.data2) # n=107
# THIS DATASET CONTAINS ONLY SUBJECTS UNDERGOING THE 
# TRANSPLANTATION AFTER THE EVENT

#-------------------------------------------------------------------------------

# CREATE THE EVENT FROM THE EVENT DATE
table(hap.data1$event.d)
hap.data1$event01 = as.numeric(hap.data1$event.d)
hap.data1$event01 = ifelse(hap.data1$event01 > 0, 1, 0) # 1=EVENT AFTER THE INCLUSION, 0=NO EVENT
table(hap.data1$event01)


# CREATE THE TRANSPLANTATION FROM TRANSPLANTATION DATE
table(hap.data2$transplantation.d)
hap.data2$transplantation01 = as.numeric(hap.data2$transplantation.d)
hap.data2$transplantation01 = ifelse(hap.data2$transplantation01 > 0, 1, 0) 
#1 = TRANSPLANTATION AFTER EVENT
#0 = TRANSPLANTATION BEFORE EVENT
#1 == 107
table(hap.data2$transplantation01)

#-------------------------------------------------------------------------------

# BASES
count(apkd) # n=2560

# 1 EVENTS
APKD1 = merge(apkd, hap.data1, by.x = "RREC_COD_ANO", by.y = "RREC_COD_ANO",
              all.x = TRUE, all.y = FALSE)
count(APKD1) # n=2560

# 2 TRANSPLANTATION
APKD2 = merge(APKD1, hap.data2, by.x = "RREC_COD_ANO", by.y = "RREC_COD_ANO",
              all.x = TRUE, all.y = FALSE)
count(APKD2) # n=2560

names(APKD2)

# APKD.names = subset(APKD2, select = -c(num_enq.x, num_enq.y, 
#                                       transplantation.d.x, transplantation.d.y, 
#                                       event.d.x, event.d.y, 
#                                       inclusion.d.x, inclusion.d.y))


#-------------------------------------------------------------------------------

# NEW VARIABLES 

eventinclusion <- as.data.frame(APKD2$event01)
eventinclusion[is.na(eventinclusion)] <- "0"
table(eventinclusion)
APKD2$eventinclusion <- eventinclusion

APKD2$event.inclusion[APKD2$eventinclusion == "0"] <- "no event"
APKD2$event.inclusion[APKD2$eventinclusion == "1"] <- "event"
str(APKD2$event.inclusion)
table(APKD2$event.inclusion)

# eventinclusion
# 0    1 
# 2299  261 == 2560

eventtransp <- as.data.frame(APKD2$transplantation01)
eventtransp[is.na(eventtransp)] <- "0"
table(eventtransp)
APKD2$eventtransp <- eventtransp

# eventtransp
# 0    1 
# 2453  107 

APKD2$event.transp[APKD2$eventtransp == "0"] <- "ev after tr"
APKD2$event.transp[APKD2$eventtransp == "1"] <- "ev bef tr"
str(APKD2$event.transp)
table(APKD2$event.transp)

table(APKD2$event.transp, APKD2$TRANSP)

APKD2$event.transpl[APKD2$event.transp == "ev after tr" & APKD2$TRANSP == "0"] <- "transp-"
APKD2$event.transpl[APKD2$event.transp == "ev after tr" & APKD2$TRANSP == "1"] <- "eve-/tr+"
APKD2$event.transpl[APKD2$event.transp == "ev bef tr" & APKD2$TRANSP == "1"] <- "eve+/tr+"
table(APKD2$event.transpl)
table(APKD2$event.transpl, APKD2$event.inclusion)

#           event no event
#eve-/tr+    47     1089
#eve+/tr+    39       68
#transp-    175     1142



################################################################################
#
#APKD2$EVENTUM = ifelse(eventinclusion == "1", "1", "0")
#
#APKD2$EVENTUM[APKD2$eventinclusion == "1" & APKD2$EVENT == "1"] <- "1"
#APKD2$EVENTUM[APKD2$eventinclusion == "0" & APKD2$EVENT == "0"] <- "0"
#APKD2$EVENTUM[APKD2$eventinclusion == "0" & APKD2$EVENT == "1"] <- "0"
#APKD2$EVENTUM[APKD2$eventinclusion == "1" & APKD2$EVENT == "0"] <- "0"
#table(APKD2$EVENTUM)
#
#APKD2$TRANSPONO[APKD2$eventtransp == "1" & APKD2$TRANSP == "1"] <- "1"
#APKD2$TRANSPONO[APKD2$eventtransp == "0" & APKD2$TRANSP == "1"] <- "0"
#APKD2$TRANSPONO[APKD2$eventtransp == "0" & APKD2$TRANSP == "0"] <- "0"
#APKD2$TRANSPONO[APKD2$eventtransp == "1" & APKD2$TRANSP == "0"] <- "1"
#table(APKD2$TRANSPONO)
#table(APKD2$TRANSPONO, APKD2$EVENTUM)
#
# table(APKD2$EVENTUM)
# 0    1 
# 2299  261 
#
# table(APKD2$TRANSPONO)
# 0    1 
# 2453  107 
#
# table(APKD2$TRANSPONO, APKD2$EVENTUM)
#     0    1
#0 2231  222
#1   68   39
#
#APKD2$EVENTUM.T[APKD2$TRANSPONO == 1 & APKD2$EVENTUM == 1] <- "no event"
#APKD2$EVENTUM.T[APKD2$TRANSPONO == 0 & APKD2$EVENTUM == 0] <- "no event"
#APKD2$EVENTUM.T[APKD2$TRANSPONO == 1 & APKD2$EVENTUM == 0] <- "no event"
#APKD2$EVENTUM.T[APKD2$TRANSPONO == 0 & APKD2$EVENTUM == 1] <- "event"
#table(APKD2$EVENTUM.T)
#
#event no event 
#222     2338 
#
################################################################################
################################################################################
################################################################################

# SIMPLIFIED DATASET TO CREATE THE FOLLOW-UP VARIABLE

# DATE VARIABLES FOR THE FOLLOW-UP
# DDC = "Date de décès"
# DINSCMED = "Date de la première inscription sur la liste transplantation"
# DDIRT = "Date de l'insuffisance rénale terminale"
# DGRF = "Date de greffe"
# DSVR = "Date de sevrage (par récupération de la fonc rénale, soit en fin d vie)"
# DPDV = "Date de perdu de vue"
# DATE_DERNOUV2019 = "Date de dernières nouvelles 2019"

dput(names(APKD2))

small <- APKD2[,c(
  "RREC_COD_ANO", "DDC",
  "DDIRT", "DGRF",
  "DATE_DERNOUV2019", 
  "evdate", "DEATH", "event.transpl", "event.inclusion")]

#-------------------------------------------------------------------------------

# SAMPLE RANDOM LINE
library(dplyr)
random = small %>% sample_n(100)
count(random)
is.data.frame(random)
# View(random)
# table(random$EVENT)

# FOLLOW UP FOR TRANSPLANTED PATIENTS

### TRANSPLANTATION DATE
str(random$DGRF)
table(random$DGRF)
# TRANSFORM AS DATE
random$DGRF.d = as.Date(random$DGRF, "%d/%m/%Y") # CONVERT MM/D/YYY IN YYYY-DD-MM
str(random$DGRF.d)
table(random$DGRF.d)
table(random$DGRF.d, random$DGRF)

#                 1/1/2018 1/1/2019 10/1/2015 10/1/2017 10/1/2018
# 2015-01-06 0        0        0         0         0         0
# 2015-01-10 0        0        0         1         0         0
# 2015-01-11 0        0        0         0         0         0
# 2016-01-02 0        0        0         0         0         0
# 2016-01-03 0        0        0         0         0         0
# 2016-01-04 0        0        0         0         0         0
# 2016-01-05 0        0        0         0         0         0
# 2016-01-08 0        0        0         0         0         0
# 2016-01-09 0        0        0         0         0         0
# 2016-01-11 0        0        0         0         0         0
# 2016-01-12 0        0        0         0         0         0
# 2017-01-02 0        0        0         0         0         0
# 2017-01-03 0        0        0         0         0         0
# 2017-01-04 0        0        0         0         0         0
# 2017-01-05 0        0        0         0         0         0
# 2017-01-06 0        0        0         0         0         0
# 2017-01-10 0        0        0         0         1         0
# 2018-01-01 0        3        0         0         0         0
# 2018-01-03 0        0        0         0         0         0
# 2018-01-04 0        0        0         0         0         0
# 2018-01-07 0        0        0         0         0         0
# 2018-01-08 0        0        0         0         0         0
# 2018-01-10 0        0        0         0         0         2
# 2018-01-11 0        0        0         0         0         0
# 2018-01-12 0        0        0         0         0         0
# 2019-01-01 0        0        1         0         0         0
# 2019-01-02 0        0        0         0         0         0

# FOLLOW-UP 
# START : INCLUSION
# END
### DATE DERNIERES NOUVELLES OR 31-12-2019
### EVENT
### TRANSPLANTATION
### DECES

# RECALL THAT THE DATE VARIABLES FOR THE FOLLOW-UP ARE NAMED
### DDC = "Date de décès"
### DINSCMED = "Date de la première inscription sur la liste transplantation"
### DDIRT = "Date de l'insuffisance rénale terminale"
### DGRF = "Date de greffe"
### DSVR = "Date de sevrage (par récupération de la fonc rénale, soit en fin d vie)"
### DPDV = "Date de perdu de vue"
### DATE_DERNOUV2019 = "Date de dernières nouvelles 2019"

#LAST FOLLOW-UP
fup.a = as.Date(random$DATE_DERNOUV2019, "%d/%m/%Y") - as.Date(random$DDIRT, "%d/%m/%Y")
fup.a = as.numeric(fup.a)
#GREFFE
fup.b = as.Date(random$DGRF, "%d/%m/%Y") - as.Date(random$DDIRT, "%d/%m/%Y")
fup.b = as.numeric(fup.b)
#EVENT
fup.c = as.Date(random$evdate, "%d/%m/%Y") - as.Date(random$DDIRT, "%d/%m/%Y")
fup.c = as.numeric(fup.c)
#DEATH
fup.d = as.Date(random$DDC, "%d/%m/%Y") - as.Date(random$DDIRT, "%d/%m/%Y")
fup.d = as.numeric(fup.d)

################################################################################

# table(APKD2$event.transpl, APKD2$event.inclusion)
#           event no event
#eve-/tr+    47     1089
#eve+/tr+    39       68
#transp-    175     1142

random$followup[random$event.transpl == "eve-/tr+" & random$event.inclusion == "event"] <- fup.b
random$followup[random$event.transpl == "eve-/tr+" & random$event.inclusion == "no event"] <- fup.b
random$followup[random$event.transpl == "eve+/tr+" & random$event.inclusion == "event"] <- fup.c
random$followup[random$event.transpl == "eve+/tr+" & random$event.inclusion == "no event"] <- fup.a #??
random$followup[random$event.transpl == "transp-" & random$event.inclusion == "event"] <- fup.c
random$followup[random$event.transpl == "transp-" & random$event.inclusion == "no event"] <- fup.a

random$followup.d = ifelse(random$DEATH == "1", fup.d, random$followup)

foup.t = ifelse(random$event.transpl == "eve-/tr+", fup.b,
                ifelse(random$event.transpl == "eve+/tr+", fup.c, "x"))

foup.t.a = ifelse(random$event.transpl == "eve-/tr+", fup.b,
                ifelse(random$event.transpl == "eve+/tr+", fup.c, fup.a))

foup.t.inclu = ifelse(random$event.inclusion == "no event", foup.t.a, fup.c) #### ca devrait marcher
foup.t.inclu.m = foup.t.inclu / (365.25/12)

random$foup.t.inclu <- foup.t.inclu
random$foup.t.inclu.m <- foup.t.inclu.m

random$foup.t.inclusion[random$event.inclusion == "event" & foup.t == "x"] <- fup.c 
random$foup.t.inclusion[random$event.inclusion == "no event" & foup.t == "x"] <- fup.a

foup.tinc = ifelse(random$event.inclusion == "event", fup.c, foup.t)
foup.tincd = ifelse(random$DEATH == "1", fup.d, foup.tinc)

#-------------------------------------------------------------------------------

random$foup.var[random$event.transpl == "eve-/tr+" & random$event.inclusion == "no event"] <- fup.b
random$foup.var[random$event.transpl == "eve-/tr+" & random$event.inclusion == "event"] <- fup.c
random$foup.var[random$event.transpl == "event" & random$event.inclusion == "no event"] <- fup.b
random$foup.var[random$event.transpl == "event" & random$event.inclusion == "event"] <- fup.c
random$foup.var[random$event.transpl == "transp-" & random$event.inclusion == "no event"] <- fup.a
random$foup.var[random$event.transpl == "transp-" & random$event.inclusion == "event"] <- fup.c

random$foup.d = ifelse(random$DEATH == 1, fup.d, random$foup.var)
################################################################################

# FOLLOW UP IN 3 STEPS 

# TRANSPLANTATION

general_fup1 = ifelse(random$eventtransp == "0", fup.b, fup.a)
# if the event occurs after the transplantation -> fup to transplantation
# if the event occurs before the transplantation -> fup to event

general_fup2 = ifelse(eventinclusion == "1", fup.c, fup.a)

general_fup3 = ifelse(random$DEATH == "1", fup.d, fup.a)

# GENERAL IF ELSE STATEMENT
# This code doesn't calculate the followup
fup.transpl = ifelse(
  random$TRANSPONO == "1", fup.b,
  ifelse(random$EVENTUM == "1", fup.c))

# FOLLOW-UP

random$followup = all_fup
random$followup_month = random$followup/(365.25/12)

#-------------------------------------------------------------------------------

library("dplyr")
randomfup <- random %>%
  mutate(group = case_when(TRASPONO))

#-------------------------------------------------------------------------------

followup1 = ifelse(random$EVENTUM.T == "event", fup.c, fup.a)
followup2 = ifelse
followup2 = ifelse(random$DEATH == "1", fup.d, followup1)

random$followup.2 = followup2

random$followup.2m = random$followup.2 / (365.25/12)

################################################################################

print <- random[,c("DDIRT", "DATE_DERNOUV2019", "DDC", "DGRF", "foup.t.inclusion", "foup.t.inclu.m")]
View(print)

library("reader")
write_csv2(print, "/Users/damianocerasuolo/Desktop/PhD/M2/DATABASES_REIN/csv_data/print_reader.csv")
