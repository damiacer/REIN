###########################
# TO UPDATE               #
###########################


# getwd()
# setwd("P:/UBRC_M2/REYES/ANALYSIS/DATABASES/csv_data") # ON PC
# setwd("/Users/damianocerasuolo/Desktop/PhD/M2/DATABASES_REIN/csv_data") # ON MAC

################################################################################

# SOURCES
## DATA MANAGEMENT ON TIDYVERSE
### https://dplyr.tidyverse.org/reference/index.html
### https://stats.idre.ucla.edu/stat/data/rdm/data_management_seminar.html
### https://stackoverflow.com/questions/28873057/sum-across-multiple-columns-with-dplyr

################################################################################

# DATABASE 1: REGISTRE REIN
## WARNING#1: DATABASE SAVED FROM SAS7DBAT. UFT-8 CODING NOT AVAILABLE.
## WARNING#2: SPECIAL CHARACTERS MAY NOT BE CORRECTLY DISPLAYED.
# rein <- read.csv2("rein_db.csv", header = TRUE, na.string="NA")
count(rein)
#View(rein)

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

# MERGING REIN_M1 WITH HOSPITALISATION DATA (MORE LINES PER SUBJECT)
# THIS PROCESS USES THE "id" TO MERGE DATABASES 
# TO KEEP ONLY FULL CORRESPONDANCE USE THE FOLLOWING PROCEDURE
count(hosp)

# CHECK THE "hospdata.R" BEFORE LAUNCHING THIS MERGING ON WINDOWS

rein_m2 <- merge(rein_m1, hosp, by.x = "num_enq", by.y = "num_enq",
                 all.x = TRUE, all.y = FALSE)
count(rein_m2)
# View(rein_m2)

#-------------------------------------------------------------------------------

# THE VARIABLE DGN_PAL DEFINES THE DIAGNOSIS 
# SOME SUBJECTS CAN HAVE MORE THAN ONE LINE SINCE THEY HAVE MORE THAN ONE EVENT 
rein_m2$DGN_PALB[rein_m2$DGN_PAL==""] <- 0
rein_m2$DGN_PALB[rein_m2$DGN_PAL!=""] <- 1
table(rein_m2$DGN_PALB)
table(rein_m2$DGN_PALB, rein_m2$DGN_PAL)

# TEST TO ELIMINATE THE DOUBLED LINES
# dgn_data <- rein_m2[,c("RREC_COD_ANO", "num_enq", "DGN_PAL", "DGN_PALB", "SOR_ANN", "SOR_MOI")]
# View(dgn_data)
dgn_data <- rein_m2
names(dgn_data)
count(dgn_data)

# VARIABLE TO VERIFY THE EVENT 
table(dgn_data$DGN_PALB)

# CREATE THE BASE WITH ONLY EVENTS 
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

#install.packages("lubridate")
library("lubridate")
# IF NECESSARY (WINDOWS): library(lubridate, warn.conflicts = FALSE)
# MORE ON LUBRIDATE: https://lubridate.tidyverse.org/

# CREATE A NEW VARIABLE FOR DATE
# THE CODE DOES NOT RECOGNIZE THE sep = "" COMMAND. DATE CANNOT BE AUTOMATICALLY CREATED
dgn_complete$eventfulldate <- as.numeric(paste(dgn_complete$SOR_ANN, dgn_complete$SOR_MOI, dgn_complete$SOR_JJ, sep = ""))
table(dgn_complete$eventfulldate)
dgn_complete$evdate = ymd(dgn_complete$eventfulldate)
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

# install.packages("sqldf")
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
count(rein_m1)
count(dgn_line)
rein_mone <- merge(rein_m1, dgn_line, by.x = "num_enq", by.y = "num_enq",
                   all.x = TRUE, all.y = FALSE)
count(rein_mone)

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
rein_mone$apkd01[rein_mone$nephgp=="diabète"] <- "0"
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

#-------------------------------------------------------------------------------

library(tableone)
dput(names(apkd))

# NEW VARIABLES
#apkd_g <- apkd %>%
#  mutate(DGN_PAL = replace(DGN_PAL, DGN_PAL == " ", "E0"))

m <- as.data.frame(apkd$DGN_PAL)
m[is.na(m)] <- "E0"
table(m)
apkd$grouping <- m
apkd$grouping01[apkd$grouping == "E0"]  <- 0
apkd$grouping01[apkd$grouping != "E0"]  <- 1
tabev <- table(apkd$grouping01)
prop.table(tabev)
apkd$grouping01 <- as.character(apkd$grouping01)

# CREATE THE TABLEONE OBJECT
CreateTableOne(data = apkd)  

variables = c("URGn", "KTTINIn", "EPOINIn", 
              "nephgp", "METHOn", "techn", "MODALn", "VAVn", 
              "traitement", "PDS", "TAIL", "IRCn", "O2n", "ICn", "ICOROn", 
              "IDMn", "RYTHMn", "ANEVn", "AMIn", "AVCAITn", "KCn", "VHBn", 
              "VHCn", "CIRHn", "VIHn", "SIDAn", "HANDn", "AMPn", "PLEGn", "CECITEn", 
              "COMPORTn", "TYPDIABn", "STADICn", "STDAMIn", "STDCIRHn", "TABACn", 
              "bmi", "tabac2", "iresp", "sero", "coro", "foie", "comCV", "comcvcl", 
              "comcvcl2", "sex", "age", "ETAT_DERNOUV2019", "delai_IRT", "delai_DC", 
              "delai_TX", "delai_SVR", "delai_PDV", "delai_DERNOUV2019", "groupes6", 
              "categories18", "groupes6_CA1", "categories18_CA1", "groupes6_CA2", 
              "categories18_CA2", "MOTIF_An", "CPKMEDn", "REFUSn", "SOR_ANN", "grouping01")

categorical = c("URGn", "KTTINIn", "EPOINIn", 
                "nephgp", "METHOn", "techn", "MODALn", "VAVn", 
                "traitement", "IRCn", "O2n", "ICn", "ICOROn", 
                "IDMn", "RYTHMn", "ANEVn", "AMIn", "AVCAITn", "KCn", "VHBn", 
                "VHCn", "CIRHn", "VIHn", "SIDAn", "HANDn", "AMPn", "PLEGn", "CECITEn", 
                "COMPORTn", "TYPDIABn", "STADICn", "STDAMIn", "STDCIRHn", "TABACn", 
                "tabac2", "iresp", "sero", "coro", "foie", "comCV", "comcvcl", 
                "comcvcl2", "sex", "groupes6", 
                "categories18", "groupes6_CA1", "categories18_CA1", "groupes6_CA2", 
                "categories18_CA2", "MOTIF_An", "CPKMEDn", "REFUSn", "SOR_ANN", "grouping01")


# CREATE THE DESCRIPTIVE TABLE
tab1 = CreateTableOne(vars = variables, data = apkd, factorVars = categorical)
print(tab1, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

# CREATE THE UNIVARIATE TABLE 
tab2 = CreateTableOne(vars = variables, data = apkd, factorVars = categorical, test = TRUE,
                      strata = "grouping01")
print(tab2, showAllLevels = TRUE, quote = TRUE, nospaces = TRUE)

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

#-------------------------------------------------------------------------------

install.packages("survival")
library("survival")
install.packages("survminer")
library("survminer")

apkd$event = as.numeric(as.character(apkd$grouping01))

apkd$DATE_DERNOUV2019d = as.Date(apkd$DATE_DERNOUV2019, "%d/%m/%Y")
apkd$DDIRTd = as.Date(apkd$DDIRT, "%d/%m/%Y")

apkd$event = as.Date(apkd$event, "%d/%m/%Y")
str(apkd$event)

table(apkd$event, apkd$DATE_DERNOUV2019)

apkd$fu = as.Date(apkd$DATE_DERNOUV2019, "%d/%m/%Y") - as.Date(apkd$DDIRT, "%d/%m/%Y")
apkd$fu_n = as.numeric(as.character(apkd$fu))

apkd$fue = as.Date(apkd$event, "%d/%m/%Y") - as.Date(apkd$DDIRT, "%d/%m/%Y")
apkd$fue_n = as.numeric(as.character(apkd$fue))

mean(apkd$fue_n, na.rm = TRUE)
min(apkd$fu_n)
max(apkd$fu_n)

Surv(apkd$fu_n, apkd$event)


apkd$evdate2 <- strptime(as.character(apkd$evdate), "%Y-%m-%d")
apkd$eventdate <- format(apkd$evdate2, "%d/%m/%Y")

f1 = survfit(Surv(apkd$fu_n, apkd$event) ~ 1)
names(f1)
plot(survfit(Surv(fu_n, event) ~ 1, data = apkd),
     xlab = "Jours",
     ylab = "Survie globale")

#-------------------------------------------------------------------------------

install.packages("rms")
library("rms")
f2 = npsurv(Surv(apkd$fu_n, apkd$event) ~ 1)
rms::survplot(f2, fun = function(x){1-x})

#-------------------------------------------------------------------------------

install.packages("incidence")
library("incidence")
# SEE https://repidemicsconsortium.org/incidence

i.7 = incidence(apkd$evdate, interval = 7)
i.7
plot(i.7)

i.365 = incidence(apkd$evdate, interval = 365)
plot(i.365)

