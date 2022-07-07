getwd()
setwd("/Users/damianocerasuolo/Desktop/PhD/M2/DATABASES_REIN/csv_data") # ON MAC

################################################################################

# PACKAGES

#install.packages("tidyverse")
library("tidyverse")
#install.packages("dplyr")
library("dplyr")
#install.packages("expss")
library("expss")
#install.packages("here")
library("here")
#install.packages("gapminder")
library("gapminder")

################################################################################

# DATABASE "REIN"
rein <- read.csv2("rein_db.csv", header = TRUE, na.string="NA")
count(rein)
#View(rein)
#names(rein)
#head(rein)
#str(rein$RREC_COD_ANO)

#-------------------------------------------------------------------------------

# HOSPITALISATION DATABSE (SNDS)
hosp <- read.csv2("snds_hospit.csv", header = TRUE, na.string="NA")
count(hosp)
#View(hosp)

#-------------------------------------------------------------------------------

# TREATMENT DATA (SNDS)
treat <- read.csv2("snds_medic.csv", header = TRUE, na.string="NA")
count(treat)
#View(treat)

#-------------------------------------------------------------------------------

# CHANGES IN DIALYSIS TREATMENT DURING THE FU
switch <- read.csv2("rein_treatswitch.csv", header = TRUE, na.string="NA")
count(switch)
#View(switch)

#-------------------------------------------------------------------------------

# CORRESPONDENCE DATABASE (ACCROCHAGE)
rein_s <- read.csv2("snds_rein.csv", header = TRUE, na.string="")
count(rein_s)
#View(rein_s)

################################################################################
################################################################################
################################################################################

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
is.data.frame(rein_m1)

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
ncol(rein_m2)
# 52724
# NEW DATABASE HAS MULTIPLE LINES FOR THE SAME SUBJECT ACCORDING TO THE 
# View(rein_m2)

# RENAME VARIABLES AND ELIMINATE DUPLICATES
rein_m2 = subset(rein_m2, select = -c(DDIRT.x, DGRF.x, DDC.x))
ncol(rein_m2)

rein_m2 <- as_tibble(rein_m2)
rein_m2 <- rein_m2 %>% rename(
		"DDIRT" = "DDIRT.y",
		"DGRF" = "DGRF.y",
		"DDC" = "DDC.y")

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
dgn_complete$eventfulldate <- as.numeric(paste(dgn_complete$SOR_ANN, dgn_complete$SOR_MOI, dgn_complete$SOR_JJ, sep = ""))
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

is.data.table(dgn_one) # FALSE
is.data.frame(dgn_one) # TRUE
# View(dgn_one)

# ADD A COUNT NUMBER FOR EACH EVENT FOR EACH SUBJECT
# THE FOLLWING SQL FUNCTION WILL CREATE A NEW COULM CALLED "count"
# THE NAME OF THE NEW COLUMN/VARIABLE CAN BE CHANGED IN THE SQL FUNCION

# install.packages("sqldf") # INSTALL ON R (BEFORE STUDIO) WHEN WORKING ON PC
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
table(dgn_oneclassifier$count)

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

# install.packages("reader")
# library("reader")

# write_csv2(apkd, "P:/UBRC_M2/REYES/ANALYSIS/DATABASES/csv_data/apkd_reader.csv")

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

# ADD 1 EVENTS
APKD1 = merge(apkd, hap.data1, by.x = "RREC_COD_ANO", by.y = "RREC_COD_ANO",
              all.x = TRUE, all.y = FALSE)
count(APKD1) # n=2560
ncol(APKD1) #94

# ADD 2 TRANSPLANTATION
APKD2 = merge(APKD1, hap.data2, by.x = "RREC_COD_ANO", by.y = "RREC_COD_ANO",
              all.x = TRUE, all.y = FALSE)
count(APKD2) # n=2560
ncol(APKD2) # 99
is.data.table(APKD2)
is.data.frame(APKD2)
names(APKD2) 

# names(APKD2)

APKD2 = subset(APKD2, select = -c(num_enq.x, num_enq.y, 
                                       transplantation.d.x, transplantation.d.y, 
                                       event.d.x, event.d.y, 
                                       inclusion.d.x, inclusion.d.y))

ncol(APKD2) # 91

#-------------------------------------------------------------------------------

# NEW VARIABLES 
# FOLLOWING VARIABLES WILL BE USED TO CALCULATE LAST DATE TO FOLLOW UP 

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

# comment on the following two lines:
# the following twos seems to do exactly the same done by the 4 line code
# per se, not a problem
APKD2$EVENTUM = ifelse(eventinclusion == "1", "1", "0")
table(APKD2$EVENTUM)

APKD2$EVENTUM[APKD2$eventinclusion == "1" & APKD2$EVENT == "1"] <- "1"
APKD2$EVENTUM[APKD2$eventinclusion == "0" & APKD2$EVENT == "0"] <- "0"
APKD2$EVENTUM[APKD2$eventinclusion == "0" & APKD2$EVENT == "1"] <- "0"
APKD2$EVENTUM[APKD2$eventinclusion == "1" & APKD2$EVENT == "0"] <- "0"
table(APKD2$EVENTUM)

APKD2$TRANSPONO[APKD2$eventtransp == "1" & APKD2$TRANSP == "1"] <- "1"
APKD2$TRANSPONO[APKD2$eventtransp == "0" & APKD2$TRANSP == "1"] <- "0"
APKD2$TRANSPONO[APKD2$eventtransp == "0" & APKD2$TRANSP == "0"] <- "0"
APKD2$TRANSPONO[APKD2$eventtransp == "1" & APKD2$TRANSP == "0"] <- "1"
table(APKD2$TRANSPONO)
table(APKD2$TRANSPONO, APKD2$EVENTUM)

# table(APKD2$EVENTUM)
# 0    1 
# 2299  261 

# table(APKD2$TRANSPONO)
# 0    1 
# 2453  107 

# table(APKD2$TRANSPONO, APKD2$EVENTUM)
#     0    1
#0 2231  222
#1   68   39

APKD2$EVENTUM.T[APKD2$TRANSPONO == 1 & APKD2$EVENTUM == 1] <- "no event"
APKD2$EVENTUM.T[APKD2$TRANSPONO == 0 & APKD2$EVENTUM == 0] <- "no event"
APKD2$EVENTUM.T[APKD2$TRANSPONO == 1 & APKD2$EVENTUM == 0] <- "no event"
APKD2$EVENTUM.T[APKD2$TRANSPONO == 0 & APKD2$EVENTUM == 1] <- "event"
table(APKD2$EVENTUM.T)

#event no event 
#222     2338 

################################################################################

# FOLLOW UP ON THE WHOLE DATABASE 

#LAST FOLLOW-UP
#fup.a = as.Date(APKD2$DATE_DERNOUV2019, "%d/%m/%Y") - as.Date(APKD2$DDIRT, "%d/%m/%Y")
#fup.a = as.numeric(fup.a)
#APKD2$fup.a <- fup.a
#	min(APKD2$fup.a) # 0
#	max(APKD2$fup.a) # 1472

#GREFFE
#fup.b = as.Date(APKD2$DGRF, "%d/%m/%Y") - as.Date(APKD2$DDIRT, "%d/%m/%Y")
#fup.b = as.numeric(fup.b)
#APKD2$fup.b <- fup.b
#	min(APKD2$fup.b, na.rm = TRUE) # 0
#	max(APKD2$fup.b, na.rm = TRUE) # 1469
	
#EVENT
#fup.c = as.Date(APKD2$evdate, "%d/%m/%Y") - as.Date(APKD2$DDIRT, "%d/%m/%Y")
#fup.c = as.numeric(fup.c)
#APKD2$fup.c <- fup.c
#	min(APKD2$fup.c, na.rm = TRUE) # - 3550
#	max(APKD2$fup.c, na.rm = TRUE) # 1743
	
#DEATH
#fup.d = as.Date(APKD2$DDC, "%d/%m/%Y") - as.Date(APKD2$DDIRT, "%d/%m/%Y")
#fup.d = as.numeric(fup.d)
#APKD2$fup.d <- fup.d
#	min(APKD2$fup.d, na.rm = TRUE) # 0 (comment: is it possible?)
#	max(APKD2$fup.d, na.rm = TRUE) # 1470

#--------------------------------------------------------------------------------

require("lubridate")

names(APKD2)

APKD2$DDC.d = as.Date(APKD2$DDC, "%d/%m/%Y")
APKD2$DATE_DERNOUV2019.d = as.Date(APKD2$DATE_DERNOUV2019, "%d/%m/%Y")
APKD2$DDIRT.d = as.Date(APKD2$DDIRT, "%d/%m/%Y")
APKD2$DGRF.d = as.Date(APKD2$DGRF, "%d/%m/%Y")
APKD2$evdate.d = as.Date(APKD2$evdate, "%d/%m/%Y")

APKD2$date <- if_else(APKD2$DEATH == "1", APKD2$DDC.d, APKD2$DATE_DERNOUV2019.d)
APKD2$date <- as.Date(APKD2$date, "%d/%m/%Y")

#-------------------------------------------------------------------------------

APKD2$date2 <- if_else(APKD2$event.inclusion == "event", APKD2$evdate.d, APKD2$date)
APKD2$date2 <- as.Date(APKD2$date2, "%d/%m/%Y")

#-------------------------------------------------------------------------------

APKD2$date3 <- if_else(APKD2$event.transpl == "eve-/tr+", APKD2$DGRF.d, APKD2$date2)
APKD2$date3 <- as.Date(APKD2$date3, "%d/%m/%Y")

#-------------------------------------------------------------------------------

# FOLLOW UP 

APKD2$followup.reader = as.Date(APKD2$date3, "%d/%m/%Y") - as.Date(APKD2$DDIRT, "%d/%m/%Y")
# APKD2$followup.reader.months = APKD2$followup.reader / (365.25/12)

################################################################################

# CREATE A EVENTDATE IS EVENTUM IS TRUE
# THE VARIABLE TO IDENTIFY THE EVENT IS "EVENTUM.T"
# NOW, TO STUDY THE INCIDENCE, YOU SHOULD KEEP ONLY DATES FOR WHICH
# EVENTUM.T == 1

# ? is this right
# dunno check it later

table(APKD2$EVENTUM.T)

APKD2$date4 = if_else(APKD2$EVENTUM.T == "no event", APKD2$DGRF.d, APKD2$date3)
APKD2$date4 = as.Date(APKD2$date4, "%d/%m/%Y")


################################################################################

# SMALLER DATABASE TO STUDY INCIDENCE

APKD.I = subset(APKD2, select = c(followup.reader, EVENTUM.T, date3))

# install.packages("reader")
library("reader")

write_csv2(APKD.I, "/Users/damianocerasuolo/Desktop/PhD/M2/DATABASES_REIN/csv_data/APKDI.csv")

################################################################################
################################################################################

names(APKD2)
ncol(APKD2)

#install.packages("plyr")
#library("plyr")
#count(APKD2, vars = "RREC_COD_ANO")

small <- APKD2[,c("RREC_COD_ANO", 
                  "num_enq.x",
                  "URGn",
                  "KTTINIn",
                  "EPOINIn",
                  "liste_longue",
                  "nephgp",
                  "METHOn",
                  "techn",
                  "MODALn",
                  "VAVn",
                  "traitement",
                  "PDS",
                  "TAIL",
                  "IRCn",
                  "O2n",
                  "ICn",
                  "ICOROn",
                  "IDMn",
                  "RYTHMn",
                  "ANEVn",
                  "AMIn",
                  "AVCAITn",
                  "KCn",
                  "VHBn",
                  "VHCn",
                  "CIRHn",
                  "VIHn",
                  "SIDAn",
                  "HANDn",
                  "AMPn",
                  "PLEGn",
                  "CECITEn",
                  "COMPORTn",
                  "TYPDIABn",
                  "STADICn",
                  "STDAMIn",
                  "STDCIRHn",
                  "TABACn",
                  "bmi",
                  "tabac2",
                  "iresp",
                  "sero",
                  "coro",
                  "foie",
                  "comCV",
                  "comcvcl",
                  "comcvcl2",
                  "sex",
                  "age",
                  "ETAT_DERNOUV2019",
                  "delai_IRT",
                  "delai_DC",
                  "delai_TX",
                  "delai_SVR",
                  "delai_PDV",
                  "delai_DERNOUV2019",
                  "groupes6",
                  "categories18",
                  "groupes6_CA1",
                  "categories18_CA1",
                  "groupes6_CA2",
                  "categories18_CA2",
                  "MOTIF_An",
                  "CPKMEDn",
                  "REFUSn",
                  "DDC",
                  "DINSCMED",
                  "DDIRT",
                  "DGRF",
                  "DSVR",
                  "DPDV",
                  "DATE_DERNOUV2019",
                  "qual_appr",
                  "evdate",
                  "DGN_PAL",
                  "count",
                  "apkd01",
                  "grouping",
                  "EVENT",
                  "deathbinary",
                  "DEATH",
                  "DGRF.d",
                  "transplantationbinary",
                  "TRANSP",
                  "DGN_PALs",
                  "inclusion.d.x",
                  "transplantation.d.x",
                  "event.d.x",
                  "event.d.y",
                  "num_enq.y", "inclusion.d.y",
                  "transplantation.d.y",
                  "event01",
                  "event.d",
                  "num_enq",
                  "inclusion.d",
                  "transplantation.d",
                  "transplantation01",
                  "eventinclusion", "event.inclusion",
                  "eventtransp",
                  "event.transp", "event.transpl",
                  "EVENTUM", "TRANSPONO",
                  "EVENTUM.T",
                  "fup.a",
                  "fup.b",
                  "fup.c",
                  "fup.d", 
                  "foup.t.inclu", "foup.t.includ", "foup.t.inclu.m",
                  "DDC.d",
                  "DATE_DERNOUV2019.d",
                  "DDIRT.d",
                  "evdate.d",
                  "date",
                  "date2", "date3",
                  "followup.reader", "followup.reader.months",
                  "status", "time")]

install.packages("survminer")
library("survminer")
install.packages("survival")
library("survival")

APKD2$status[APKD2$EVENTUM.T=="no event"] <- "1"
APKD2$status[APKD2$EVENTUM.T=="event"] <- "2"
str(APKD2$status)
APKD2$status <- as.factor(APKD2$status)
length(APKD2$status)

APKD2$time <- APKD2$followup.reader
APKD2$time <- as.numeric(APKD2$time)

linelistsurv.by = survfit(Surv(time, status) ~ 1, data = APKD2)

#-------------------------------------------------------------------------------
      
ggsurvplot(linelistsurv.by, data = APKD2)
      
testdata<-APKD2[c(263,0,526),]
testdata2<-APKD2[-c(263,0,526),]
count(testdata2)
testdata3<-APKD2[,c("time", "status")]

testdata3$time = as.numeric(testdata3$time)
write_csv2(as.matrix(testdata3), "/Users/damianocerasuolo/Desktop/PhD/M2/DATABASES_REIN/csv_data/testdata.reader.CSV")
write_csv2(testdata3, "/Users/damianocerasuolo/Desktop/PhD/M2/DATABASES_REIN/csv_data/testreader.csv")

#testdata3.1<-testdata3[-c(263,0,526),]
#testdata3.2 = as.data.frame(testdata3.1, check.names=FALSE)
#count(testdata3.1)

linelistsurv.by = survfit(Surv(time, status) ~ 1, data = testdata3)
ggsurvplot(linelistsurv.by, data = testdata3)
      

survminer::ggsurvplot(
  linelistsurv.by, 
  data = random,          
  conf.int = FALSE,              # do not show confidence interval of KM estimates
  surv.scale = "percent",        # present probabilities in the y axis in %
  break.time.by = 10,            # present the time axis with an increment of 10 days
  xlab = "Follow-up days",
  ylab = "Survival Probability",
  pval = T,                      # print p-value of Log-rank test 
  pval.coord = c(40,.91),        # print p-value at these plot coordinates
  risk.table = T,                # print the risk table at bottom 
  legend.title = "overall",     # legend characteristics, "class1" and "class2" most of the time
  #legend.labs = c("class1","class2"),
  font.legend = 10, 
  palette = "Dark2",             # color palette 
  surv.median.line = "hv",       # draw horizontal and vertical lines to the median survivals
  ggtheme = theme_light()        # simplify plot background
)
