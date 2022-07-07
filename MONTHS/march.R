getwd()
setwd("/Users/damianocerasuolo/Desktop/PhD/M2/DATABASES_REIN/csv_data") 

#--------------------------------------------------------------------------------

# VAR NAMES

#URGn = "Premier traitement en urgence", 
#prttturg = "Premier traitement en urgence", 
#KTTINIn = "1ère séance d'hémodialyse réalisée avec cathéter",
#EPOINIn = "Traitement par Erythropoietine",
#liste_longue = "Regroupement détaillé néphropathie",
#nephgp = "Regroupement en 8 classes néphropathie",
#METHOn = "Traitement 3 classes",
#techn = "Méthode de traitement",
#MODALn = "Modalité de traitement",
#VAVn = "Voie d'abord vasculaire",
#traitement = "Traitement (concaténation TECHN et MODAL)",
#PDS = "Poids",
#TAIL = "Taille",
#IRCn = "Insuffisance respiratoire chronique",          
#O2n = "Oxygénothérapie",   
#ICn = "Insuffisance cardiaque",
#ICOROn = "Insuffisance coronarienne",
#IDMn = "Infarctus du myocarde",
#RYTHMn = "Troubles du rythme",
#ANEVn = "Anevrysme de l'aorte abdominale",
#AMIn = "Artérite des membres inférieurs",
#AVCAITn = "Variable composite de AVC et AIT",
#KCn = "Cancer évolutif",
#VHBn = "Ag HBS positif",
#VHCn = "PCR VHC positif", 
#CIRHn = "Cirrhose",
#VIHn = "VIH",
#SIDAn = "SIDA",
#HANDn = "Au moins un handicap",
#AMPn = "Amputation membres inférieurs",
#PLEGn = "Paraplégie/Hémiplégie",
#CECITEn = "Cécité",
#COMPORTn = "Troubles du comportement",
#TYPDIABn = "Type de diabète",
#STADICn = "Stade de l'insuffisance cardiaque",
#STDAMIn = "Stade de l'artérite des membres inférieurs",
#STDCIRHn = "Stade de la cirrrhose",
#TABACn = "Statut tabagique 0-1-2",
#bmi = "IMC",
#tabac2 = "Statut tabagique 0-1",   
#iresp = "Variable composite de O2 et IRC",   
#sero = "Variable composite de VIH et SIDA",
#coro = "Variable composite de ICORO et IDM",
#foie = "VariablSe composite de CIRH, VHB, VHC",
#comCV = "Nb de comorbidités  cardiovasc sur 6 chez les patients avec TOUTES LES VAR RENSEIGNEES",
#comcvcl = "Au moins une comorbidité cardiovasculaire",
#comcvcl2 = "Nb de comorbidités cardiovasculaires en 3 classes sur 6 comorbidités",
#sex = "Sexe",
#age = "Age à l'initiation du traitement de suppléance",    
#ETAT_DERNOUV2019 = "Etat aux dernières nouvelles avant 31/12/2019",
#delai_IRT = "Délai insuffisance rénale terminale",
#delai_DC = "Délai décès",
#delai_TX = "",
#delai_SVR = "Délai de sevrage (par récupération de la fonc rénale, soit en fin d vie)",     
#delai_PDV = "Délai de perdue de vue",       
#delai_DERNOUV2019 = "Délai dernières nouvelles 2019",
#groupes6 = "Regroupement causes de décès 6 groupes", 
#categories18 = "Regroupement causes de décès 18 groupes",
#groupes6_CA1 = "Regroupement causes de décès associée 1 en 6 groupes",
#categories18_CA1 = "Regroupement causes de décès associée 1 en 18 groupes",
#groupes6_CA2 = "Regroupement causes de décès associée 2 en 6 groupes",
#categories18_CA2 = "Regroupement causes de décès associée 2 en 18 groupes",
#MOTIF_An = "Motif d'arrêt de la dialyse",
#CPKMEDn = "Fin de traitement pour complication médicale",
#REFUSn = "Fin de traitement par refus du patient",
#DDC = "Date de décès",
#DINSCMED = "Date de la première inscription sur la liste transplantation",
#DDIRT = "Date de l'insuffisance rénale terminale",
#DGRF = "Date de greffe",          
#DSVR = "Date de sevrage (par récupération de la fonc rénale, soit en fin d vie)",
#DPDV = "Date de perdu de vue",
#DATE_DERNOUV2019 = "Date de dernières nouvelles 2019",
#RREC_COD_ANO = "Code anonym"

#--------------------------------------------------------------------------------

# PACKAGES

library("tidyverse")
library("dplyr")
library("expss")
library("here")
library("gapminder")

#--------------------------------------------------------------------------------

# DATABASE "REIN"
rein <- read.csv2("rein_db.csv", header = TRUE, na.string="NA")
dim(rein)

#--------------------------------------------------------------------------------

# HOSPITALISATION DATABSE (SNDS)
hosp <- read.csv2("snds_hospit.csv", header = TRUE, na.string="NA")
dim(hosp)

#--------------------------------------------------------------------------------

# TREATMENT DATA (SNDS)
treat <- read.csv2("snds_medic.csv", header = TRUE, na.string="NA")
dim(treat)

#--------------------------------------------------------------------------------

# CHANGES IN DIALYSIS TREATMENT DURING THE FU
switch <- read.csv2("rein_treatswitch.csv", header = TRUE, na.string="NA")
dim(switch)

#--------------------------------------------------------------------------------

# CORRESPONDENCE DATABASE (ACCROCHAGE)
rein_s <- read.csv2("snds_rein.csv", header = TRUE, na.string="")
dim(rein_s)

#################################################################################

# MERGING "ACCROCHAGE" FOR SNDS AND REIN DATA

rein_m4 <- merge(rein, rein_s, by.x = "RREC_COD_ANO", by.y = "RREC_COD_ANO")
dim(rein_m4)

#-------------------------------------------------------------------------------

# MERGING "HOSP"
# NOTES: THIS STEP WAS ORIGINALLY TO CREATE "rein_m2".
# NOTES: rein_m1 has more lines than rein. This is due to multiple lines for the same patient
# since a patient can experience more than one event during the follow up

rein_h <- merge(rein_m4, hosp, by.x = "num_enq", by.y = "num_enq",
                 all.x = TRUE, all.y = FALSE)
dim(rein_h)
# 52724    85

rein_h <- as_tibble(rein_h)
rein_h <- rein_h %>% rename(
  #new name = old name
  "DDC" = "DDC.x",
  "DGRF" = "DGRF.x",
  "DDIRT" = "DDIRT.x")

rein_h = subset(rein_h, select = -c(DGRF.y, DDIRT.y, DDC.y))

#--------------------------------------------------------------------------------

# MERGING THE FIRST TREATMENT FOR EACH PATENT 

treat_line <- as_tibble(treat_line)
treat_line <- treat_line %>% rename(
  "num_enq" = "NUM_ENQ")

dim(treat_line)
# 33706     5

rein_m3 <- merge(rein_m4, treat_line, by.x = "num_enq", by.y = "num_enq",
                 all.x = TRUE, all.y = FALSE)

dim(rein_m3)
#45026    78

################################################################################

# THE VARIABLE DGN_PAL DEFINES THE DIAGNOSIS 
# SOME SUBJECTS CAN HAVE MORE THAN ONE LINE SINCE THEY HAVE MORE THAN ONE EVENT 
rein_h$DGN_PALB[rein_h$DGN_PAL==""] <- "0"
rein_h$DGN_PALB[rein_h$DGN_PAL!=""] <- "1"
table(rein_h$DGN_PALB)
table(rein_h$DGN_PALB, rein_h$DGN_PAL)
#is.na(rein_h$DGN_PALB)

# TEST TO ELIMINATE THE DOUBLED LINES
# dgn_data <- rein_m3[,c("RREC_COD_ANO", "num_enq", "DGN_PAL", "DGN_PALB", "SOR_ANN", "SOR_MOI")]
# View(dgn_data)
dgn_data <- rein_h
names(dgn_data)
count(dgn_data) # THIS SHOULD BE THE SAME AS "rein_m3"

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
dgn_complete$SOR_JJ <- rep("01", times=18543)

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

# EXLCUDE EVENTES BEFORE 2015

dgn_complete$inclusion = as.Date(dgn_complete$DDIRT, "%d/%m/%Y")
dgn_complete$evdate = as.Date(dgn_complete$evdate, "%d/%m/%Y")

dgn_complete.2015 <- dgn_complete %>% 
  select(evdate, inclusion, num_enq, RREC_COD_ANO, DGN_PAL) %>%
  filter(evdate > inclusion)

count(dgn_complete) # 18543
count(dgn_complete.2015) # 12517

# SELECT FIRST OCCURRENCE BY TIME DATE
# STACK: https://stackoverflow.com/questions/54525745/r-select-first-occurrence-by-time-date-for-multiple-ids
# COMMENT: the following function does not actually create a new database selecting only one occurrence, but
# it helps creating a new dataset with only select() variables and ordered by date.
# the selection is run by dplyr after a counting variable is added to the dataset. 
# the following step could be avoided, though ordering the data makes the upcoming steps run smoother

#dgn_one <- dgn_complete %>%
#  group_by(num_enq, lubridate::date(evdate)) %>%
#  arrange(evdate) %>%
#  slice(1) %>%
#  ungroup() %>%
#  select(evdate, num_enq, DGN_PAL, DDIRT)

# is.data.table(dgn_one) # FALSE
# is.data.frame(dgn_one) # TRUE
# View(dgn_one)

dgn_complete.2015$DDIRT = dgn_complete.2015$inclusion
str(dgn_complete.2015$DDIRT)

dgn_one <- dgn_complete.2015 %>%
  group_by(num_enq, lubridate::date(evdate)) %>%
  arrange(evdate) %>%
  slice(1) %>%
  ungroup() %>%
  select(evdate, num_enq, DGN_PAL, DDIRT)

is.data.table(dgn_one) # FALSE
is.data.frame(dgn_one) # TRUE

count(dgn_one) # 12678 

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
table(dgn_oneclassifier$count)
# 11457 (second round up)
table(dgn_oneclassifier$count)

# NEW DATABASE WITH ONLY THE FIRST EVENT PER PERSON
# THE "filter" FUNCTION REQUIRES dplyr OR tidyverse

dgn_line = filter(dgn_oneclassifier, count == 1)
count(dgn_line)
#names(dgn_line)
#View(dgn_line)
# 7888

#-------------------------------------------------------------------------------

rein_m3 <- as_tibble(rein_m3)
rein_m3 <- rein_m3 %>% rename(
  #new name = old name
  #"DDC" = "DDC.x",
  #"DGRF" = "DGRF.x",
  "DDIRT" = "DDIRT.x")

rein_m3 = subset(rein_m3, select = -c(DDIRT.y))

dim(dgn_line)
dim(rein_m3)
rein_mone <- merge(rein_m3, dgn_line, by.x = "num_enq", by.y = "num_enq",
                   all.x = TRUE, all.y = FALSE)
dim(rein_mone)
# 45026    82

rein_mone <- as_tibble(rein_mone)
rein_mone <- rein_mone %>% rename(
  #new name = old name
  #"DDC" = "DDC.x",
  #"DGRF" = "DGRF.x",
  "DDIRT" = "DDIRT.x")

rein_mone = subset(rein_mone, select = -c(DDIRT.y, count.x, count.y))

################################################################################

rdb <- rein_mone

#------------------------------------------------------------------------------

names(rdb)

################################################################################


# CREATE THE VARIABLE EVENT FOR THE NEW DATASET

rdb$DGN_PAL[is.na(rdb$DGN_PAL)] <- "E0"
rdb$DGN_PAL[rdb$DGN_PAL=="I740"] = "E0"
rdb$DGN_PAL[rdb$DGN_PAL=="I741"] = "E0"
rdb$DGN_PAL[rdb$DGN_PAL=="I742"] = "E0"
rdb$DGN_PAL[rdb$DGN_PAL=="I743"] = "E0"
rdb$DGN_PAL[rdb$DGN_PAL=="I744"] = "E0"
rdb$DGN_PAL[rdb$DGN_PAL=="I745"] = "E0"
rdb$DGN_PAL[rdb$DGN_PAL=="I748"] = "E0"
table(rdb$DGN_PAL)

# RECODE THE "rdb$DGN_PAL" VARIABLE  (DIAGNOSIS)

rdb$DGN_PAL[rdb$DGN_PAL=="G450"] = "G45"
rdb$DGN_PAL[rdb$DGN_PAL=="G451"] = "G45"
rdb$DGN_PAL[rdb$DGN_PAL=="G452"] = "G45"
rdb$DGN_PAL[rdb$DGN_PAL=="G453"] = "G45"
rdb$DGN_PAL[rdb$DGN_PAL=="G454"] = "G45"
rdb$DGN_PAL[rdb$DGN_PAL=="G458"] = "G45"
rdb$DGN_PAL[rdb$DGN_PAL=="G459"] = "G45"
rdb$DGN_PAL[rdb$DGN_PAL=="G460"] = "G46"
rdb$DGN_PAL[rdb$DGN_PAL=="G462"] = "G46"
rdb$DGN_PAL[rdb$DGN_PAL=="G463"] = "G46"
rdb$DGN_PAL[rdb$DGN_PAL=="G464"] = "G46"
rdb$DGN_PAL[rdb$DGN_PAL=="G465"] = "G46"
rdb$DGN_PAL[rdb$DGN_PAL=="G466"] = "G46"
rdb$DGN_PAL[rdb$DGN_PAL=="G467"] = "G46"
rdb$DGN_PAL[rdb$DGN_PAL=="G468"] = "G46"
rdb$DGN_PAL[rdb$DGN_PAL=="G810"] = "G81"
rdb$DGN_PAL[rdb$DGN_PAL=="G8100"] = "G81"
rdb$DGN_PAL[rdb$DGN_PAL=="G8101"] = "G81"
rdb$DGN_PAL[rdb$DGN_PAL=="G8108"] = "G81"
rdb$DGN_PAL[rdb$DGN_PAL=="H534"] = "H53"
rdb$DGN_PAL[rdb$DGN_PAL=="I600"] = "I60"
rdb$DGN_PAL[rdb$DGN_PAL=="I601"] = "I60"
rdb$DGN_PAL[rdb$DGN_PAL=="I602"] = "I60"
rdb$DGN_PAL[rdb$DGN_PAL=="I603"] = "I60"
rdb$DGN_PAL[rdb$DGN_PAL=="I604"] = "I60"
rdb$DGN_PAL[rdb$DGN_PAL=="I605"] = "I60"
rdb$DGN_PAL[rdb$DGN_PAL=="I606"] = "I60"
rdb$DGN_PAL[rdb$DGN_PAL=="I607"] = "I60"
rdb$DGN_PAL[rdb$DGN_PAL=="I608"] = "I60"
rdb$DGN_PAL[rdb$DGN_PAL=="I609"] ="I60"
rdb$DGN_PAL[rdb$DGN_PAL=="I610"] = "I61"
rdb$DGN_PAL[rdb$DGN_PAL=="I611"] = "I61"
rdb$DGN_PAL[rdb$DGN_PAL=="I612"] = "I61"
rdb$DGN_PAL[rdb$DGN_PAL=="I613"] = "I61"
rdb$DGN_PAL[rdb$DGN_PAL=="I614"] = "I61"
rdb$DGN_PAL[rdb$DGN_PAL=="I615"] = "I61"
rdb$DGN_PAL[rdb$DGN_PAL=="I616"] = "I61"
rdb$DGN_PAL[rdb$DGN_PAL=="I618"] = "I61"
rdb$DGN_PAL[rdb$DGN_PAL=="I619"] = "I61"
rdb$DGN_PAL[rdb$DGN_PAL=="I620"] = "I62"
rdb$DGN_PAL[rdb$DGN_PAL=="I621"] = "I62"
rdb$DGN_PAL[rdb$DGN_PAL=="I629"] = "I62"
rdb$DGN_PAL[rdb$DGN_PAL=="I630"] = "I63"
rdb$DGN_PAL[rdb$DGN_PAL=="I631"] = "I63"
rdb$DGN_PAL[rdb$DGN_PAL=="I632"] = "I63"
rdb$DGN_PAL[rdb$DGN_PAL=="I633"] = "I63"
rdb$DGN_PAL[rdb$DGN_PAL=="I634"] = "I63"
rdb$DGN_PAL[rdb$DGN_PAL=="I635"] = "I63"
rdb$DGN_PAL[rdb$DGN_PAL=="I636"] = "I63"
rdb$DGN_PAL[rdb$DGN_PAL=="I638"] = "I63"
rdb$DGN_PAL[rdb$DGN_PAL=="I639"] = "I63"
rdb$DGN_PAL[rdb$DGN_PAL=="I64"] = "I64"
rdb$DGN_PAL[rdb$DGN_PAL=="R470"] = "R47"
rdb$DGN_PAL[rdb$DGN_PAL=="R4700"] = "R47"
rdb$DGN_PAL[rdb$DGN_PAL=="R4701"] = "R47"
rdb$DGN_PAL[rdb$DGN_PAL=="R4702"] = "R47"
rdb$DGN_PAL[rdb$DGN_PAL=="R4703"] = "R47"
rdb$DGN_PAL[rdb$DGN_PAL=="R471"] = "R47"
rdb$DGN_PAL[rdb$DGN_PAL=="R478"] = "R47"

table(rdb$DGN_PAL)

#E0   G45   G46   G81   H53   I60   I61   I62   I63   I64   R47 
#43030   360    16    23     3    47   245   122   989   101    90

################################################################################

# DEATH VARIABLE

rdb$delai_DC[is.na(rdb$delai_DC)] <- "0"
rdb$DEATH[rdb$delai_DC == 0] <- "0"
rdb$DEATH[rdb$delai_DC > 0] <- "1"
table(rdb$DEATH)

# EVENT VARIABLE

rdb$EVENT[rdb$DGN_PAL == "E0"] <- "0"
rdb$EVENT[rdb$DGN_PAL != "E0"] <- "1"
table(rdb$EVENT)

################################################################################

# CREATE A DAY FOR EVERY ENTRY IN THE DATABASE
# EVERY ENTRY DAY WILL BE SET TO 31
rdb$december.d <- rep(31, times=45026)
rdb$december.m <- rep(12, times=45026)
rdb$december.y <- rep(2019, times=45026)

# install.packages("lubridate")
library("lubridate")
# IF NECESSARY (WINDOWS): library(lubridate, warn.conflicts = FALSE)
# MORE ON LUBRIDATE: https://lubridate.tidyverse.org/

# CREATE A NEW VARIABLE FOR DATE
# THE CODE DOES NOT RECOGNIZE THE sep = "" COMMAND. DATE CANNOT BE AUTOMATICALLY CREATED
rdb$december <- as.numeric(paste(rdb$december.y, rdb$december.m, rdb$december.d, sep = ""))
table(rdb$december)
rdb$december = ymd(rdb$december)
table(rdb$december)
str(rdb$december)

#-------------------------------------------------------------------------------

# FOLLOW-UPS

# TIL END 2019
FUP.19 = as.Date(rdb$december, "%d/%m/%Y") - as.Date(rdb$DDIRT, "%d/%m/%Y") 

# TIL DEATH
FUP.D = as.Date(rdb$DDC, "%d/%m/%Y") - as.Date(rdb$DDIRT, "%d/%m/%Y") 

# TIL EVENT
FUP.E = as.Date(rdb$evdate, "%d/%m/%Y") - as.Date(rdb$DDIRT, "%d/%m/%Y")

# TIL TRANSPLANTATION
FUP.T = as.Date(rdb$DGRF, "%d/%m/%Y") - as.Date(rdb$DDIRT, "%d/%m/%Y")

#-------------------------------------------------------------------------------

rdb$inclusion.d = as.Date(rdb$DDIRT, "%d/%m/%Y")
rdb$inclusion.n = as.numeric(rdb$inclusion.d)
# table(rdb$inclusion.n, rdb$inclusion.d)

rdb$transplantation.d = as.Date(rdb$DGRF, "%d/%m/%Y")
rdb$transplantation.n = as.numeric(rdb$transplantation.d)

rdb$event.d = as.Date(rdb$evdate, "%d/%m/%Y")
rdb$event.n = as.numeric(rdb$event.d)

rdb$death.d = as.Date(rdb$DDC, "%d/%m/%Y")
rdb$death.n = as.numeric(rdb$death.d)

# FILTER EVENTS AFTER INCLUSION
# _VARNAME ?_ <- rdb %>% 
#select(_VARNAMES HERE WITHOUT ""_) %>%
#  filter(event.d > inclusion.d)

# EVENT HAPPENS BEFORE THE INCLUSION
rdb$e.b.i = if_else(rdb$event.n < rdb$inclusion.n, "1", "0")
table(rdb$e.b.i)

# TRANSPLANTATION HAPPENS BEFORE THE EVENT
rdb$t.b.e = if_else(rdb$transplantation.n < rdb$event.n, "1", "0")
table(rdb$t.b.e)

# EVENT HAPPEN BEFORE DEATH
# IF "1", THE EVENT HAPPENS BEFORE DEATH, OTHERWISE IT EQUALS ZERO
# THE EVENTS THAT HAPPEN AFTER DEATH (WHERE THE EVENT NUMERIC DATE 
# IS > TO DEATH NUMERIC) HAPPENS 
rdb$e.b.d = if_else(rdb$event.n < rdb$death.n, "1", "0")
table(rdb$e.b.d)
# 0  (event after death) 1 (event before death) 
# 1007                   1710 

# NEW DATASET TO VERIFY
rdb.death <- rdb[!(rdb$e.b.d=="1"),]
#View(rdb.death)
table(rdb.death$evdate, rdb.death$DDC)

rdb.death2 <- rdb[!(rdb$e.b.d=="0"),]
#View(rdb.death2)
table(rdb.death2$evdate, rdb.death2$DDC)

################################################################################

# FOLLOW UP COMPOSITE
# case_when here: https://www.r-bloggers.com/2019/10/if-ifelse-had-more-ifs/
# THE CASE case_when FUNCTION CAN EXCLUDE THE LINES WHEN APPROPRIATE
# SO THERE IS NO NEED TO SPECIFY - IF WE HAVE ALREADY SPECIFIED THE PREVIOUS EVENT 
# THE EXCLUDING CONDITIONS 
# I.E. IF ALL DEATHS ARE FOLLOWED UNTIL DEATH, NO NEED TO SPECIFIY THAT THEY SHOULD
# NOT BE FOLLOWED UNDER OTHER CIRCUMSTANCES

# BUT THE FUNCTION IS ORDER SENSITIVE! WHAT THE FUCK
# NOT TO THE FUTURE SELF: A VARIABLE THAT COULD TAKE INTO ACCOUNT THE 
# FACT THAT DEATH COULD OCCUR AFTER THE EVENT SHOULD BE INCLUDED

library(dplyr)
rdb <- rdb %>% 
  mutate(epilogus = case_when(
    # the patient is dead
    DEATH == "1" & e.b.d == "0" ~ FUP.D, 
    # the patient is transplanted before the event
    t.b.e == "1" ~ FUP.T, 
    # the patient is transplanted after the event
    t.b.e == "0" ~ FUP.E,
    # event
    EVENT == "1" & e.b.i != "1" #& DEATH != "1" 
    ~ FUP.E,
    # no event
    EVENT == "0" #& DEATH == "0" 
    ~ FUP.19
  )) 

mean(rdb$epilogus)
# Time difference of 1205.994 days

################################################################################

# TRANSPLANTATION VARIABLE

rdb$DGRF.d = as.numeric(as.Date(rdb$DGRF, "%d/%m/%Y"))
rdb$DGRF.d[is.na(rdb$DGRF.d)] <- 0
rdb$TRANSP = ifelse(rdb$DGRF.d  > 0, "1", "0")
table(rdb$TRANSP)

# EVENT DEFINTION
# EVENT DOES NOT OCCUR IF IT HAPPENS BEFORE THE TRANSPLANTATION
# THE EVENTS HAPPENING BEFORE THE INCLUSION ARE ALREADY NOT INCLUDED 
# IN THIS DATABASE
# THE VARIABLE FOR EVENT IS "EVENT"
# THE VARIABLE FOR THE TRANSPLANTATION IS "TRANSP"
# THE VARIABLE FOR TRANSPLANTATION BEFORE AND AFTER THE EVENT IS "t.b.e"
# IF t.b.e == 1 THE TRANSPLANTATION OCCURS BEFORE THE EVENT 
# IF t.b.e == 0 THE TRANSPLANTATION OCCURS AFTER THE EVENT (THEN THE PATIENT IS FO-UP UNTIL EVENT)

table(rdb$EVENT)
table(rdb$TRANSP)
table(rdb$t.b.e)
#   0   1 
# 322 276 

table(rdb$EVENT, rdb$t.b.e)
#    0   1
#0 266 183
#1  56  93
# ALL THE OTHER ONES ARE MISSING

# VALUE == 3 FOR PATIENTS WITHOUT EVENT AND TRANSPLANTATION 
# (THEY STILL CAN HAVE AN EVENT!)
rdb$t.b.eN = as.numeric(as.character(rdb$t.b.e))
rdb$t.b.eN[is.na(rdb$t.b.eN)] <- 2 #<- mean(rdb$t.b.e, na.rm = TRUE)
table(rdb$t.b.eN)

rdb$EVENTUM[rdb$t.b.eN == "2" & rdb$EVENT == "1"] <- "event"
rdb$EVENTUM[rdb$t.b.eN == "2" & rdb$EVENT == "0"] <- "no event"
rdb$EVENTUM[rdb$t.b.eN == "1" & rdb$EVENT == "1"] <- "no event"
rdb$EVENTUM[rdb$t.b.eN == "0" & rdb$EVENT == "1"] <- "event"
table(rdb$EVENTUM) 
#    event no event 
#     1903    42674 

table(rdb$EVENTUM, rdb$EVENT)
#              0     1
# event        0  1903
#no event  42581    93
# 93 EVENTS HAPPEN AFTER THE TRANSPLANTATION
# ONLY event WILL BE CONSIDERED AS EVENT

################################################################################
################################################################################
################################################################################

# MODELS 

# 1st MODEL
# LIST OF VARIABLES 
#     * cardiovascular disease (cerebral excluded) ICn ICOROn IDMn RYTHMn ANEVn
#     * tobacco tabac2
#     * dialysis method METHOn OU MODALn OU techn
#     *** cerebrovascular event (are the events different in SNDS and REIN) AVCAITn
#     * ADPKD
#     * sex sex
#     * age age

# POPULATION 
#     * complete population
#     * population after the exclusion of diabetes

#--------------------------------------------------------------------------------

# CARDIOVASCULAR DISEASE
# 1 = AT LEAST ONE CARDIOVASCULAR AFFECTION 
# 0 = NO CARDIOVASCULAR DISEASE
# CEREBRAL ISCHEMIA NOT TAKEN INTO ACCOUNT

rdb$cardiovasc = if_else(rdb$ICn + rdb$ICOROn + rdb$IDMn + rdb$RYTHMn + 
                           rdb$ANEVn > 0, "1", "0")

#-----------------------------

# DIALYSIS METHOD

rdb <- rdb %>% 
  mutate(dial = case_when(
    # HEM
    METHOn == "1" ~ "1", 
    # PERIT
    METHOn == "2" ~ "2",
  )) 

table(rdb$dial)

#-----------------------------

# APKD

rdb$apkd01 = if_else(rdb$nephgp == "APKD", "1", "0")
table(rdb$apkd01)
#     0     1 
# 42466  2560 

# DIABETES Y/N

# IF 0 = NO DIABETES
# IF 1 OR 2 = DIABETES TYPE 1 OR 2
# 0     1     2 
# 22943  1112 19044 

rdb$diabetes = if_else(rdb$TYPDIABn > 0, "1", "0")
table(rdb$diabetes)
#0     1 
#22943 20156

rdb$diabetesMISS <- rdb$diabetes
rdb$diabetesMISS[is.na(rdb$diabetes)] <- "miss"
prop.table(table(rdb$diabetesMISS))*100
#0         1          miss 
#50.955004 44.765247  4.279749 

# NOTE : I WILL USE THIS VARIABLE TO RUN THE NEW ANALYSIS ON DB PATIENTS ONLY.
# THE PATIENTS WITH MISSING VALUES FOR DIABETES WILL BE THEN EXCLUDED

#--------------------------------------------------------------------------------

# PACKAGE INSTALLATION
# AND DEFINITION OF TIME AND STATUS 

install.packages("survminer")
library("survminer")
install.packages("survival")
library("survival")

rdb$time = rdb$epilogus
rdb$time = as.numeric(as.character(rdb$time))
rdb$status = if_else(rdb$EVENTUM == "event", "1", "0")

rdb$status = as.factor(rdb$status)
str(rdb$status)
rdb$status = as.numeric(as.character(rdb$status))

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# ALL POPULATION MODEL

#linelistsurv.by = survfit(Surv(time, status) ~ cardiovasc + 
#                            tabac2 + dial + apkd01 + #AVCAITn + 
#                            sex + age, data = rdb)
#ggsurvplot(linelistsurv.by, data = rdb)


coxpop <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
        apkd01 + #AVCAITn + 
        sex + age, data = rdb)
summary(coxpop)

#--------------------------------------------------------------------------------

# UNIVARIATE ANALYSIS 

dput(names(rdb))

covariates <- c("URGn", "KTTINIn", "EPOINIn", 
                "nephgp", "METHOn", "techn", "MODALn", "VAVn", "traitement", 
                "PDS", "TAIL", "IRCn", "O2n", "ICn", "ICOROn", "IDMn", "RYTHMn", 
                "ANEVn", "AMIn", "AVCAITn", "KCn", "VHBn", "VHCn", "CIRHn", "VIHn", 
                "SIDAn", "HANDn", "AMPn", "PLEGn", "CECITEn", "COMPORTn", "TYPDIABn", 
                "STADICn", "STDAMIn", "STDCIRHn", "TABACn", "bmi", "tabac2", 
                "iresp", "sero", "coro", "foie", "comCV", "comcvcl", "comcvcl2", 
                "sex", "age", "ETAT_DERNOUV2019", "delai_IRT", "delai_DC", "delai_TX", 
                "delai_SVR", "delai_PDV", "groupes6", 
                "MOTIF_An", "CPKMEDn", "REFUSn", 
                 "DSVR", "DPDV", "qual_appr", "delivrance", 
                #"L_ATC4", 
                "apkd01", "TRANSP", "diabetes", 
                "diabetesMISS")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))

univ_models <- lapply(univ_formulas, function(x){coxph(x, data = rdb)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# ALL POPULATION INCLUDING DIABETES AS COVARIABLE 

coxpopdia <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                  apkd01 + #AVCAITn +
                  + diabetes +
                  sex + age, data = rdb)
summary(coxpopdia)

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# EXCLUDING DIABETES+ POPULATION

#didb<-rdb[!(rdb$diabetesMISS=="1"),]
didb<-rdb[(rdb$diabetesMISS=="0"),]
count(adb) # 24870  = 45026 - 20156

coxapop <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                     apkd01 + #AVCAITn +
                     sex + age, data = didb)
summary(coxapop)

################################################################################
################################################################################

# 2nd ANALYS
# RUN ONLY ON ADPKD POPULATION 
# MAIN OBJECTIVE: IS DIALYSIS METHOD IMPORTANT TO THE OUTCOME
# NOTES: fewer events
# NOTES: the dialysis method should be verified and updated if possible (switch database)

apdb<-rdb[!(rdb$apkd01=="0"),]
count(apdb)
# 2560

cox_apdkpop <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                   #apkd01 + AVCAITn +
                   sex + age, data = apdb)
summary(cox_apdkpop)

#--------------------------------------------------------------------------------

# APKD WITHOUT DIABETES
apdbdiab<-apdb[(apdb$diabetesMISS=="0"),]
count(apdbdiab)

cox_apdkpopdia <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                       #apkd01 + AVCAITn +
                       sex + age, data = apdbdiab)
summary(cox_apdkpopdia)
