getwd()
setwd("/Users/damianocerasuolo/Desktop/PhD/M2/DATABASES_REIN/csv_data") 

#################################################################################

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
#install.packages("mitml")
library("mitml")

#--------------------------------------------------------------------------------

# DATABASE "REIN"
rein <- read.csv2("rein_db.csv", header = TRUE, na.string="NA")
dim(rein)
names(rein)

#--------------------------------------------------------------------------------

# HOSPITALISATION DATABSE (SNDS)
hosp <- read.csv2("snds_hospit.csv", header = TRUE, na.string="NA")
names(hosp)

#--------------------------------------------------------------------------------

# TREATMENT DATA (SNDS)
treat <- read.csv2("snds_medic.csv", header = TRUE, na.string="NA")
dim(treat)
names(treat)

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

#################################################################################
#################################################################################
#################################################################################

# ADDING TREATMENT TO BASE

# CREATE A DELIVRANCE DATE
treat$jour_delivrance <- rep("01", times=473902)

treat$mois_delivrance[treat$mois_delivrance=="1"] <- "01"
treat$mois_delivrance[treat$mois_delivrance=="2"] <- "02"
treat$mois_delivrance[treat$mois_delivrance=="3"] <- "03"
treat$mois_delivrance[treat$mois_delivrance=="4"] <- "04"
treat$mois_delivrance[treat$mois_delivrance=="5"] <- "05"
treat$mois_delivrance[treat$mois_delivrance=="6"] <- "06"
treat$mois_delivrance[treat$mois_delivrance=="7"] <- "07"
treat$mois_delivrance[treat$mois_delivrance=="8"] <- "08"
treat$mois_delivrance[treat$mois_delivrance=="9"] <- "09"
treat$mois_delivrance[treat$mois_delivrance=="10"] <- "10"
treat$mois_delivrance[treat$mois_delivrance=="11"] <- "11"
treat$mois_delivrance[treat$mois_delivrance=="12"] <- "12"

#---------------------------------------------------------------------------------

library(lubridate)

treat$delivrance <- as.numeric(paste(treat$an_delivrance, treat$mois_delivrance, treat$jour_delivrance, sep = ""))
table(treat$delivrance)
treat$delivrance = ymd(treat$delivrance)
str(treat$delivrance)

# IS DELIVRANCE MISSING? 
treat$delivrance.num = as.numeric(treat$delivrance)
dmiss <- as.data.frame(treat$delivrance.num)
dmiss[is.na(dmiss)] <- "missing"
table(dmiss)
#View(dmiss)

#--------------------------------------------------------------------------------

# SELECT FIRST OCCURRENCE BY TIME DATE
# STACK: https://stackoverflow.com/questions/54525745/r-select-first-occurrence-by-time-date-for-multiple-ids

library(dplyr)

treat_one <- treat %>%
  group_by(NUM_ENQ, lubridate::date(delivrance)) %>%
  arrange(delivrance) %>%
  slice(1) %>%
  ungroup() %>%
  select(delivrance, L_ATC4, NUM_ENQ, DDIRT)

is.data.table(treat_one) # FALSE
is.data.frame(treat_one) # TRUE

count(treat) # 473902
count(treat_one) # 405293

# ADD A COUNT NUMBER FOR EACH EVENT FOR EACH SUBJECT
# THE FOLLWING SQL FUNCTION WILL CREATE A NEW COULM CALLED "count"
# THE NAME OF THE NEW COLUMN/VARIABLE CAN BE CHANGED IN THE SQL FUNCION

#treat_one = treat.one

# install.packages("sqldf") # INSTALL ON R (BEFORE STUDIO) WHEN WORKING ON PC
library("sqldf")
treat_oneclassifier <- sqldf("SELECT a.*, COUNT(*) count
       FROM treat_one a, treat_one b 
       WHERE a.NUM_ENQ = b.NUM_ENQ AND b.ROWID <= a.ROWID 
       GROUP BY a.ROWID"
)

#      treat_oneclassifier2 <- sqldf("SELECT a.*, COUNT(*) count
#             FROM treat a, treat b 
#             WHERE a.NUM_ENQ = b.NUM_ENQ AND b.ROWID <= a.ROWID 
#             GROUP BY a.ROWID"
#      )

is.data.table(treat_oneclassifier)
# View(dgn_oneclassifier)
names(treat_oneclassifier)
count(treat_oneclassifier)
# 405293
table(treat_oneclassifier$count)

#1     2     3     4     5     6     7     8     9     10 
#33706 28810 25999 23974 22325 20941 19596 18375 17197 16147 
#11    12    13    14    15    16    17    18    19    20 
#15149 14151 13205 12289 11514 10696  9916  9208 8530 7876 
#21    22    23    24    25    26    27    28    29    30 
#7287  6721  6136  5616  5137  4661  4192  3789  3394  3005 
#31    32    33    34    35    36    37    38    39    40 
#2688  2350  2063  1788  1526  1269  1051   841  659  509 
#41    42    43    44    45    46    47    48 
#365   273   172   105    48    29   11     4 

# NEW DATABASE WITH ONLY THE FIRST EVENT PER PERSON
# THE "filter" FUNCTION REQUIRES dplyr OR tidyverse

treat_line = filter(treat_oneclassifier, count == 1)
count(treat_line) # 33706

#################################################################################
#################################################################################
#################################################################################

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

#################################################################################

# THE VARIABLE DGN_PAL DEFINES THE DIAGNOSIS 
# SOME SUBJECTS CAN HAVE MORE THAN ONE LINE SINCE THEY HAVE MORE THAN ONE EVENT 
# 3 CASES

# 1ST = WE ARE INTERESTED IN ALL EVENTS 

rein_h <- rein_h %>% 
  mutate(DGN_PALB = case_when(
    DGN_PAL == "" ~ "0",
    DGN_PAL != "" ~ "1"
  ))
table(rein_h$DGN_PALB)
rein_h$DGN_PALB[is.na(rein_h$DGN_PALB)] <- "0"

rein_h <- rein_h %>%
  mutate(DGN_PAL.cc1 = case_when(
    DGN_PALB == "1" ~ DGN_PAL,
    DGN_PALB == "0" ~ "missing"
  ))
table(rein_h$DGN_PAL.cc1, useNA = "always")

#################################################################################

# TEST TO ELIMINATE THE DOUBLED LINES
# dgn_data <- rein_m3[,c("RREC_COD_ANO", "num_enq", "DGN_PAL", "DGN_PALB", "SOR_ANN", "SOR_MOI")]
# View(dgn_data)
count(rein_h)
dgn_data <- rein_h
names(dgn_data)
count(dgn_data)

# CREATE THE BASE WITH ONLY EVENTS 
# IN THIS DATABASE, ONE PATIENT CAN HAVE MORE THAN ONE EVENT 
# AND PATIENTS FROM THE ORIGINAL DATABASES (BUT WHO HAVE NO EVENT), COMPLETELY MISSING
dgn_complete<-dgn_data[!(dgn_data$DGN_PAL.cc1=="missing"),]

# Use the folling function to exclude the non-complete lines
#dgn_complete = dgn_data[complete.cases(dgn_data[ , "DGN_PAL"]), ] 

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
dgn_complete$eventfulldate <- as.numeric(paste(dgn_complete$SOR_ANN, dgn_complete$SOR_MOI, 
                                               dgn_complete$SOR_JJ, sep = ""))
table(dgn_complete$eventfulldate, useNA = "always")
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

# CREATING THE ATCD VARIABLE 

table(dgn_complete.2015$atcd)

dgn_atcd = subset(dgn_complete, select = c(num_enq, atcd))
#View(dgn_atcd)
summary(dgn_atcd)
dim(dgn_atcd)

# enumerate the lines in order to keep only the first one 

library("sqldf")
dgn_atcd_one <- sqldf("SELECT a.*, COUNT(*) count
       FROM dgn_atcd a, dgn_atcd b 
       WHERE a.num_enq = b.num_enq AND b.ROWID <= a.ROWID 
       GROUP BY a.ROWID"
)
summary(dgn_atcd_one)

# select the first occurrence only 
# df = mydata[!(mydata$var == ""),]

dgn_atcd_first = dgn_atcd_one[(dgn_atcd_one$count == "1"),]
dim(dgn_atcd_one)
table(dgn_atcd_one$count, useNA = "always")
dim(dgn_atcd_first)

dgn_atcd_first = subset(dgn_atcd_first, select = -c(count))
dim(dgn_atcd_first)

# SELECT FIRST OCCURRENCE BY TIME DATE
# STACK: https://stackoverflow.com/questions/54525745/r-select-first-occurrence-by-time-date-for-multiple-ids
# COMMENT: the following function does not actually create a new database selecting only one occurrence, 
# but it helps creating a new dataset with only select() variables and ordered by date.
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

count(dgn_one) # 12517 

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
count(dgn_oneclassifier) #12517
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

#-ADD THE ATCD VARIABLE

dim(dgn_atcd_first) # 10845
dim(rein_mone) # 45026

rein_mone <- merge(rein_mone, dgn_atcd_first, by.x = "num_enq", by.y = "num_enq",
                   all.x = TRUE, all.y = FALSE)

################################################################################

rdb <- rein_mone
names(rdb)

################################################################################
################################################################################
################################################################################

# DEFINITION OF THE EVENT FOR THE REIN DATABASE (AFTER THE MERGE)

# FIRST STEP: WHEN DGN_PAL IS MISSING
rdb$DGN_PAL[is.na(rdb$DGN_PAL)] <- "E0"

# SECOND STEP: WHEN DGN_PAL IS NOT MISSING
rdb <- rdb %>% 
  mutate(DGN_PAL = case_when(
    # missing
    DGN_PAL == "E0" ~ "E0", 
    # embolie et thrombose artérielles
    DGN_PAL == "I740" | DGN_PAL == "I741" | DGN_PAL == "I742"  | DGN_PAL == "I743" | 
      DGN_PAL == "I744" | DGN_PAL == "I745" | DGN_PAL == "I748" ~ "E0",
    # accidentes ischémiques cérébraux transitoires et syndromes apparentés
    DGN_PAL == "G450" | DGN_PAL == "G451" | DGN_PAL == "G452" | DGN_PAL == "G453" | DGN_PAL == "G454" |
      DGN_PAL == "G458" | DGN_PAL == "G459" ~ "G45",
    DGN_PAL == "G460" | DGN_PAL == "G462" | DGN_PAL == "G463" | DGN_PAL == "G464" | DGN_PAL == "G465" |
      DGN_PAL == "G466" | DGN_PAL == "G467" | DGN_PAL == "G468" ~ "G46", 
    # hémiplégie
    DGN_PAL == "G810" | DGN_PAL == "G8100" | DGN_PAL == "G8101" | DGN_PAL == "G8108" ~ "G81",
    # anomalies du champ visuels
    DGN_PAL == "H534" ~ "E0",
    # hémorragie sous-arachnoïdienne
    DGN_PAL == "I600" | DGN_PAL == "I601" | DGN_PAL == "I602" | DGN_PAL == "I603" | DGN_PAL == "I604" |
      DGN_PAL == "I605" | DGN_PAL == "I606" | DGN_PAL == "I607" | DGN_PAL == "I608" | DGN_PAL == "I609" ~ "I60",
    # hémorragie intracérébrale
    DGN_PAL == "I610" | DGN_PAL == "I611" | DGN_PAL == "I612" | DGN_PAL == "I613" | DGN_PAL == "I614" |
      DGN_PAL == "I615" | DGN_PAL == "I616" | DGN_PAL == "I618" | DGN_PAL == "I619" ~ "I61",
    # infarctus cérébral 
    DGN_PAL == "I630" | DGN_PAL == "I631" | DGN_PAL == "I632" | DGN_PAL == "I633" | DGN_PAL == "I634" | 
      DGN_PAL == "I635" | DGN_PAL == "I636" | DGN_PAL == "I6308" | DGN_PAL == "I639" ~ "I63",
    # accident vasculaire cérébral, non précisé comme étant hémorragique ou par infarctus
    DGN_PAL == "I64" ~ "I64",
    # dysphasie et aphasie
    DGN_PAL == "R470" | DGN_PAL == "R4700" | DGN_PAL == "R4701" | DGN_PAL == "R4702" | DGN_PAL == "R4703" |
      DGN_PAL == "R471" | DGN_PAL == "R478" ~ "E0"
  ))

table(rdb$DGN_PAL, useNA = "always")
# E0      G45   G46   G81   ##H53   I60   I61   I63   I64    ##R47 
# 43030   360    16    23   ##3     47    245   832   101      ##90 

# when useNA is always 
#E0      G45   G46   G81   I60   I61   I63   I64  <NA> 
#43123   360    16    23    47   245   832   101   279 

rdb$DGN_PAL[is.na(rdb$DGN_PAL)] <- "E0"

table(rdb$DGN_PAL, useNA = "always")
#E0   G45   G46   G81   I60   I61   I63   I64  <NA> 
#43402   360    16    23    47   245   832   101     0 

################################################################################
################################################################################
################################################################################

### NEW VARIABLES

# DEATH VARIABLE

rdb$delai_DC[is.na(rdb$delai_DC)] <- "0"
rdb$DEATH[rdb$delai_DC == 0] <- "0"
rdb$DEATH[rdb$delai_DC > 0] <- "1"
table(rdb$DEATH)
#0     1 
#30686 14340 

#-------------------------------------------------------------------------------

# EVENT VARIABLE

rdb$EVENT[rdb$DGN_PAL == "E0"] <- "0"
rdb$EVENT[rdb$DGN_PAL != "E0"] <- "1"
table(rdb$EVENT)
#    0     1 
#43030  1996

#-------------------------------------------------------------------------------

# TRANSPLANTATION VARIABLE 

rdb$DGRF.d = as.numeric(as.Date(rdb$DGRF, "%d/%m/%Y"))
rdb$DGRF.d[is.na(rdb$DGRF.d)] <- 0
rdb$TRANSP = if_else(rdb$DGRF.d  > 0, "1", "0")
table(rdb$TRANSP)

# 0     1 
# 37730  7296 

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

################################################################################

# FOLLOW-UPS

# TIL END 2019
FUP.19 = as.Date(rdb$december, "%d/%m/%Y") - as.Date(rdb$DDIRT, "%d/%m/%Y") 

# TIL DEATH
FUP.D = as.Date(rdb$DDC, "%d/%m/%Y") - as.Date(rdb$DDIRT, "%d/%m/%Y") 

# TIL EVENT
FUP.E = as.Date(rdb$evdate, "%d/%m/%Y") - as.Date(rdb$DDIRT, "%d/%m/%Y")

# TIL TRANSPLANTATION
FUP.T = as.Date(rdb$DGRF, "%d/%m/%Y") - as.Date(rdb$DDIRT, "%d/%m/%Y")

# DATE VARIABLES AS NUMERIC 

#rdb$DGRF.d = as.numeric(as.Date(rdb$DGRF, "%d/%m/%Y"))
rdb$DDC.d = as.numeric(as.Date(rdb$DDC, "%d/%m/%Y"))
rdb$evdate.d = as.numeric(as.Date(rdb$evdate, "%d/%m/%Y"))

# DATE VARIABLES AS DATE VARIABLES 

rdb$DGRF.asd = as.Date(rdb$DGRF, "%d/%m/%Y")
rdb$DDC.asd = as.Date(rdb$DDC, "%d/%m/%Y")
rdb$evdate.asd = as.Date(rdb$evdate, "%d/%m/%Y")
rdb$DDIRT.asd = as.Date(rdb$DDIRT, "%d/%m/%Y")

# VARIABLE BEFORE AND AFTER TRANSPLANTATION 

# EVENT
rdb$eventbeforeT = if_else(rdb$evdate.asd < rdb$DGRF.asd, "1", "0") # 1 = event before transplant
table(rdb$eventbefore, useNA = "always")

# DEATH
rdb$eventbeforeD = if_else(rdb$evdate.asd < rdb$DDC.asd, "1", "0") # 1 = event before death
table(rdb$deathbeforeE, useNA = "always")

# DEATH AND TRANSP 
rdb$deathbeforeT = if_else(rdb$DGRF.asd < rdb$DDC.asd, "1", "0") # 1 = death before transpl
table(rdb$eventbeforeT, useNA = "always")

#-FUP------------------------------------------------------------------------
#DEFINITIVE CODE FOR FUP !!!

rdb <- rdb %>%
  mutate(epilogus = case_when(
    # DEAD PATIENTS
    DEATH == "1" & EVENT == "1" & TRANSP == "1" & eventbeforeT == "1" ~ FUP.E,
    DEATH == "1" & EVENT == "1" & TRANSP == "1" & eventbeforeT == "0" ~ FUP.T,
    DEATH == "1" & EVENT == "0" & TRANSP == "0" ~ FUP.D, 
    DEATH == "1" & EVENT == "0" & TRANSP == "1" ~ FUP.T,
    DEATH == "1" & EVENT == "1" & TRANSP == "0" ~ FUP.E,
    
    # NOT DEAD
    DEATH == "0" & EVENT == "1" & TRANSP == "1" & eventbeforeT == "1" ~ FUP.E,
    DEATH == "0" & EVENT == "1" & TRANSP == "1" & eventbeforeT == "0" ~ FUP.T,
    DEATH == "0" & EVENT == "1" & TRANSP == "0" ~ FUP.E,
    DEATH == "0" & EVENT == "0" & TRANSP == "1" ~ FUP.T, 
    DEATH == "0" & EVENT == "0" & TRANSP == "0" ~ FUP.19
  ))
table(rdb$epilogus, useNA = "always")
mean(rdb$epilogus)
min(rdb$epilogus)
max(rdb$epilogus)

#epigDATA = subset(rdb, select = c(epilogus, DEATH, EVENT, DDIRT.asd, DGRF.asd, DDC.asd, evdate.asd, TRANSP))
#View(epigDATA)

#rdb$epilogus.numM <- rdb$epilogus.num
#rdb$epilogus.numM[is.na(rdb$epilogus.numM)] <- "missing"
#table(rdb$epilogus.numM)

#rdb.misscheck<-rdb[(rdb$epilogus.numM=="missing"),]
#rdb.misscheck = subset(rdb.misscheck, select = c(epilogus, DEATH, EVENT, december, DDIRT.asd, DGRF.asd, DDC.asd, evdate.asd, TRANSP))
#View(rdb.misscheck)

#

#rdb <- rdb %>% 
#  mutate(epilogus.num = case_when(
#    # DEAD PATIENTS 
#    DEATH == "1" & (DDC.d > evdate.d) ~ FUP.E, 
#    DEATH == "1" & (DDC.d > DGRF.d) & (DGRF.d > evdate.d) ~ FUP.E,
#    DEATH == "1" & (DDC.d > DGRF.d) & (DGRF.d < evdate.d) ~ FUP.T,
#    DEATH == "1" & EVENT == "0" ~ FUP.D, 
#    # NOT DEAD PATIENTS
#    DEATH == "0" & (evdate.d > DGRF.d) ~ FUP.E,
#    DEATH == "0" & (evdate.d < DGRF.d) ~ FUP.T,
#    DEATH == "0" & EVENT == "0" ~ FUP.19,
#    DEATH == "0" & EVENT == "0" & TRANSP == "1" ~ FUP.T   
#  ))
#table(rdb$epilogus.num, useNA = "always")

#

# misclassification issues 
# table(rdb$epilogus, rdb$epilogus.num)

# deciles = quantile(rdb$epilogus, probs = seq(0.1, 1, by = 0.1), na.rm = T)
#  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
#  358  718  723  728 1084 1091 1449 1457 1817 2171 

# deciles = quantile(rdb$epilogus.num, probs = seq(0.1, 1, by = 0.1), na.rm = T)
#  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
#  359  718  724  729 1085 1091 1450 1457 1817 2172 

# str(rdb$epilogus)

# rdb$epilogus.numb = as.numeric(rdb$epilogus)
# rdb$epilogus.num.b = as.numeric(rdb$epilogus.num)

# --> lines
# rdb <- rdb %>% 
#  mutate(ep.class = case_when(
#    epilogus.numb == 0 ~ "001",
#    epilogus.numb > 0 & epilogus.numb < 358 ~ "01",
#    epilogus.numb >= 358 & epilogus.numb < 718 ~ "02",
#    epilogus.numb >= 718 & epilogus.numb < 723 ~ "03",
#    epilogus.numb >= 723 & epilogus.numb < 728 ~ "04",
#    epilogus.numb >= 728 & epilogus.numb < 1084 ~ "05",
#    epilogus.numb >= 1084 & epilogus.numb < 1091 ~ "06",
#    epilogus.numb >= 1091 & epilogus.numb < 1449 ~ "07",
#    epilogus.numb >= 1449 & epilogus.numb < 1457 ~ "08",
#    epilogus.numb >= 1457 & epilogus.numb < 1817 ~ "09",
#    epilogus.numb >= 1817 & epilogus.numb < 2171 ~ "10"
#  ))

# --> column
#rdb <- rdb %>% 
#  mutate(epnum.class = case_when(
#    epilogus.num.b == 0 ~ "000",
#    epilogus.num.b > 0 & epilogus.num.b < 358 ~ "01",
#    epilogus.num.b >= 358 & epilogus.num.b < 718 ~ "02",
#    epilogus.num.b >= 718 & epilogus.num.b < 724 ~ "03",
#    epilogus.num.b >= 724 & epilogus.num.b < 729 ~ "04",
#    epilogus.num.b >= 729 & epilogus.num.b < 1085 ~ "05",
#    epilogus.num.b >= 1085 & epilogus.num.b < 1091 ~ "06",
#    epilogus.num.b >= 1091 & epilogus.num.b < 1450 ~ "07",
#    epilogus.num.b >= 1450 & epilogus.num.b < 1457 ~ "08",
#    epilogus.num.b >= 1457 & epilogus.num.b < 1817 ~ "09",
#    epilogus.num.b >= 1817 & epilogus.num.b < 2172 ~ "10"
#  ))

#table(rdb$ep.class, rdb$epnum.class)

#       000   01   02   03   04   05   06   07   08   09   10
# 001  443    0    0    0    0    0    0    0    0    0    0
# 01     0 3721    0    0    0    0    0    0    0    0    0
# 02     0    0 4211    0    0    0    0    0    0    0    0
# 03     0    0    0 3781    0   35    0    0    0    0    0
# 04     0    0    0  793 3615   63    0    0    0    0    0
# 05     0    0    0    0  908 3350    0   15    0    0    0
# 06     0    0    0    0    0  678 4223   73    0    0    0
# 07     0    0    0    0    0    0    0 4080    0    0    0
# 08     0    0    0    0    0    0    0  538 3927   56    0
# 09     0    0    0    0    0    0    0    0    0 4470   26
# 10     0    0    0    0    0    0    0    0    0    0 4541


# CREATE THE VARIABLE EVENT TAKING INTO ACCOUNT THE TRANSPLANTATION DATE 

#rdb$DGRF.asd
#rdb$DDC.asd 
#rdb$evdate.asd 

table(rdb$EVENT)
#     0     1 
# 43936  1090 

rdb <- rdb %>% 
  mutate(EVENTUM = case_when(
    TRANSP == "0" & EVENT == "1" ~ "event",
    TRANSP == "0" & EVENT == "0" ~ "no.event",
    TRANSP == "1" & EVENT == "0" ~ "no.event",
    (DGRF.asd > evdate.asd) & EVENT == "1" ~ "event",
    (DGRF.asd < evdate.asd) & EVENT == "1" ~ "no.event",
    (DGRF.asd > evdate.asd) & EVENT == "0" ~ "no.event",
    (DGRF.asd < evdate.asd) & EVENT == "0" ~ "no.event",
  ))
table(rdb$EVENTUM)
table(rdb$EVENT, rdb$EVENTUM)

# la transplantation
# aucun événement est enregistré pour les patients ayant eu une 
# greffe - car l'événement doit toujours être antécédent à la transplantation 

#table(rdb$METHOn, rdb$EVENTUM)
#  event no.event
#1   919    33312
#2    93     3498
#3     0       23

#-CARDIOVASCULAR DISEASE--------------------------------------------------------
# 1 = AT LEAST ONE CARDIOVASCULAR AFFECTION 
# 0 = NO CARDIOVASCULAR DISEASE
# CEREBRAL ISCHEMIA NOT TAKEN INTO ACCOUNT

rdb$cardiovasc = if_else(rdb$ICn + rdb$ICOROn + rdb$IDMn + rdb$RYTHMn + 
                           rdb$ANEVn + rdb$AVCAITn > 0, "1", "0")
table(rdb$cardiovasc)

#-DIALYSIS METHOD---------------------------------------------------------------

rdb <- rdb %>% 
  mutate(dial = case_when(
    # HEM
    METHOn == "1" ~ "1", 
    # PERIT
    METHOn == "2" ~ "2"
  )) 

table(rdb$dial)
# lines columns
table(rdb$dial, rdb$TRANSP)

table(rdb$dial, rdb$EVENTUM)
table(rdb$METHOn, rdb$EVENTUM)
table(rdb$METHOn, rdb$TRANSP)

#-APKD---------------------------------------------------------------------------

rdb$apkd01 = if_else(rdb$nephgp == "APKD", "1", "0")
table(rdb$apkd01)
#     0     1 
# 42466  2560 

#-DIABETES Y/N------------------------------------------------------------------

# IF 0 = NO DIABETES
# IF 1 OR 2 = DIABETES TYPE 1 OR 2
# 0      1     2 
# 22943  1112 19044 

str(rdb$TYPDIABn)
rdb$diabetes = if_else(rdb$TYPDIABn > 0, "1", "0")
table(rdb$diabetes)

#0     1 
#22943 20156

rdb$diabetesMISS <- rdb$diabetes
rdb$diabetesMISS[is.na(rdb$diabetes)] <- "miss"
prop.table(table(rdb$diabetesMISS))*100
#                0                 1          missing 
# 22943 (50.955004) 20156 (44.765247)  1927 (4.279749) 

# NOTE : I WILL USE THIS VARIABLE TO RUN THE NEW ANALYSIS ON DB PATIENTS ONLY.
# THE PATIENTS WITH MISSING VALUES FOR DIABETES WILL BE THEN EXCLUDED

#-BMI IN CLASSES----------------------------------------------------------------

rdb <- rdb %>% 
  mutate(bmic = case_when(
    bmi < 18.5 ~ "1", 
    bmi >= 18.5 & bmi < 25 ~ "2", 
    bmi >= 25.0 & bmi < 30 ~ "3",
    bmi > 30 ~ "4"
  )) 
table(rdb$bmic)

#-ATCD-------------------------------------------------------------------------

str(rdb$atcd)
rdb$atcd[is.na(rdb$atcd)] <- 0
table(rdb$atcd, useNA = "always")

#-TIME AND STATUS--------------------------------------------------------------

rdb$time = rdb$epilogus
rdb$time = as.numeric(as.character(rdb$time))

rdb$status = if_else(rdb$EVENTUM == "event", "1", "0")
table(rdb$status)
rdb$status = as.factor(rdb$status)


#-FOLLOWUP ACCORDING TO EVENT/DEATH---------------------------------------------

# event variable: EVENT 
# death variable: DEATH
# time variabble: TIME

#-CUMHAZ TO IMPUTE--------------------------------------------------------------

library(survminer)
library(survival)

rdb$cardiovasc <- as.factor(rdb$cardiovasc)
rdb$tabac2 <- as.factor(rdb$tabac2)
rdb$dial <- as.factor(rdb$dial)
rdb$apkd01 <- as.factor(rdb$apkd01)
rdb$sex <- as.factor(rdb$sex)
rdb$age <- as.numeric(as.character(rdb$age))
rdb$diabetes <- as.factor(rdb$diabetes)
rdb$status <- as.factor(rdb$status)
rdb$time <- as.numeric(as.character(rdb$time))
rdb$bmic <- as.factor(rdb$bmic)

cox.fit <- coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                   dial + apkd01 +
                   bmic + sex + age + diabetes, data = rdb)
summary(cox.fit)

#cox.fit2 <- coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
#                   dial + apkd01*diabetes +
#                   bmic + sex + age , data = rdb)
#summary(cox.fit2)

# -> adding the interaction term in the model does not affect the result.
# -> there is no interaction between diabetes and adpkd

# survfit(), in addition to the survival function, also computes the cumulative baseline hazard function.

SF <- survfit(cox.fit)
rdb$cumhaz <- NA
for(i in 1:nrow(rdb)) {
  #Can only compute at non-missing times
  if (!is.na(rdb$time[i])) {
    rdb$cumhaz[i] <- 
      summary(SF, times = rdb$time[i])$cumhaz
  }
}

rdb.for.imp <- rdb %>% 
  select(cumhaz, cardiovasc, tabac2, dial, apkd01, 
         sex, age, diabetes, bmic, status)

summary(rdb.for.imp)

# data imputation

library("mice")
imp.rdb <- mice(rdb.for.imp,
                seed  = 21051986,
                m     = 20, #nimpute(rdb.for.imp),
                maxit = 10,
                meth = "pmm",
                print = F)
imp.rdb


# Checking... Is the data in the same order as before?
# First imputation
imp.rdb.dat <- complete(imp.rdb)
table(imp.rdb.dat$tabac2, rdb$tabac2, exclude=NULL)
# etc...
# All in the same order

# Imputed datasets in long form
imp.rdb.dat <- complete(imp.rdb, "long", include = TRUE)

# Repeat time variable m + 1 times since impdat
# includes the original data as well as m imputations
imp.rdb.dat$time <- rep(rdb$time, imp.rdb$m + 1)

# Replace missing time values with time corresponding
# to the imputed cumulative hazard value
SUB <- imp.rdb.dat$.imp > 0 & is.na(imp.rdb.dat$time)
if(sum(SUB) > 0) {
  
  # Create a data frame with the unique event times
  # and corresponding cumulative hazards from the
  # complete case analysis
  bhz <- data.frame(time   = survfit(cox.fit)$time,
                    cumhaz = survfit(cox.fit)$cumhaz)
  
  # The following only works if pmm (the default) was used
  # to impute missing cumhaz values (because it relies on
  # the imputed values being values present in the non-missing
  # values)
  for(i in 1:sum(SUB)) {
    # Use max since last 2 times have the same cumhaz
    imp.rdb.dat$time[SUB][i] <-
      max(bhz$time[bhz$cumhaz == imp.rdb.dat$cumhaz[SUB][i]])
  }
}

# Convert back to a mids object
imp.rdb.new <- as.mids(imp.rdb.dat)

# fit the cox model 
#fit.imp.cox <- with(imp.natality.new,
#                coxph(Surv(gestage37, preterm01) ~
#                      RF_PPTERM + MAGER + MRACEHISP + DMAR))
# Do NOT include the -1 here since a Cox model has no intercept
# summary(pool(fit.imp.cox), conf.int = T,
#         exponentiate = T)[, c("term", "estimate", "2.5 %", "97.5 %", "p.value")]
#round.summary(fit.imp.cox, digits = 3,
#              exponentiate = T)[, c("estimate", "2.5 %", "97.5 %", "p.value")]

#-MULTIVARIATE ANALYSIS FOR ALL PATIENTS----------------------------------------


allcox = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                   dial + apkd01 +
                                   bmic + sex + age + diabetes))
summary(pool(allcox))
est.allcox <- pool(allcox)
summary(est.allcox, conf.int = TRUE, exponentiate = TRUE)

# with interaction?
allcox.int = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                   dial + apkd01*diabetes +
                                   bmic + sex + age))
summary(pool(allcox.int))
est.allcoxint <- pool(allcox.int)
summary(est.allcoxint, conf.int = TRUE, exponentiate = TRUE)

# apkd011:diabetes1 0.4604450 0.600500515 -1.2915256 1535.8221 1.967158e-01 0.1417842 1.4952985
# -> there is no interaction

# empty model for comparaison
est.allcox.e = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                    dial + apkd01 +
                                    sex + age + diabetes))
D1(allcox, est.allcox.e)

#-BIVARIATE ANALYSIS------------------------------------------------------------

# cardiovascular
cox.t2.cardiovasc = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc))
summary(pool(cox.t2.cardiovasc))
est.card <- pool(cox.t2.cardiovasc)
summary(est.card, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$cardiovasc, useNA = "always")

# tabac2
cox.t2.tab = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ tabac2))
summary(pool(cox.t2.tab))
est.tab <- pool(cox.t2.tab)
summary(est.tab, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$tabac2, useNA = "always")

# dial
cox.t2.dial = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ dial))
summary(pool(cox.t2.dial))
est.dial <- pool(cox.t2.dial)
summary(est.dial, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$dial, useNA = "always")

# adpkd
cox.t2.adpkd = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ apkd01))
summary(pool(cox.t2.adpkd))
est.adpkd <- pool(cox.t2.adpkd)
summary(est.adpkd, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$apkd01)

# bmic
cox.t2.bmic = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ bmic))
summary(pool(cox.t2.bmic))
est.bmic <- pool(cox.t2.bmic)
summary(est.bmic, conf.int = TRUE, exponentiate = TRUE)

cox.t2.bmicE = with(rdb.imputed, coxph(Surv(time, status=="1") ~ 1))
library("mitml")
D1(cox.t2.bmic, cox.t2.bmicE)
  # events
  table(rdb$status, rdb$bmic)


# sex
cox.t2.sex = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ sex))
summary(pool(cox.t2.sex))
est.sex <- pool(cox.t2.sex)
summary(est.sex, conf.int = TRUE, exponentiate = TRUE)
  # event
  table(rdb$status, rdb$sex)

# age
cox.t2.age = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ age))
summary(pool(cox.t2.age))
est.age <- pool(cox.t2.age)
summary(est.age, conf.int = TRUE, exponentiate = TRUE)
  

# diabetes
cox.t2.dia = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ diabetes))
summary(pool(cox.t2.dia))
est.dia <- pool(cox.t2.dia)
summary(est.dia, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$diabetes)

#-EXCLUSION OF THE DIABETIC PATIENTS----------------
  
#-CUMHAZ TO IMPUTE--------------------------------------------------------------

library(survminer)
library(survival)

rdb$cardiovasc <- as.factor(rdb$cardiovasc)
rdb$tabac2 <- as.factor(rdb$tabac2)
rdb$dial <- as.factor(rdb$dial)
rdb$apkd01 <- as.factor(rdb$apkd01)
rdb$sex <- as.factor(rdb$sex)
rdb$age <- as.numeric(as.character(rdb$age))
rdb$diabetes <- as.factor(rdb$diabetes)
rdb$status <- as.factor(rdb$status)
rdb$time <- as.numeric(as.character(rdb$time))
rdb$bmic <- as.factor(rdb$bmic)
rdb$a

cox.fit <- coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                   dial + apkd01 +
                   bmic + sex + age + diabetes, data = rdb)
summary(cox.fit)

#cox.fit2 <- coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
#                   dial + apkd01*diabetes +
#                   bmic + sex + age , data = rdb)
#summary(cox.fit2)

# -> adding the interaction term in the model does not affect the result.
# -> there is no interaction between diabetes and adpkd

# survfit(), in addition to the survival function, also computes the cumulative baseline hazard function.

SF <- survfit(cox.fit)
rdb$cumhaz <- NA
for(i in 1:nrow(rdb)) {
  #Can only compute at non-missing times
  if (!is.na(rdb$time[i])) {
    rdb$cumhaz[i] <- 
      summary(SF, times = rdb$time[i])$cumhaz
  }
}

rdb.for.imp <- rdb %>% 
  select(cumhaz, cardiovasc, tabac2, dial, apkd01, 
         sex, age, diabetes, bmic, status)

summary(rdb.for.imp)

# data imputation

library("mice")
imp.rdb <- mice(rdb.for.imp,
                seed  = 21051986,
                m     = 20, #nimpute(rdb.for.imp),
                maxit = 10,
                meth = "pmm",
                print = F)
imp.rdb


# Checking... Is the data in the same order as before?
# First imputation
imp.rdb.dat <- complete(imp.rdb)
table(imp.rdb.dat$tabac2, rdb$tabac2, exclude=NULL)
# etc...
# All in the same order

# Imputed datasets in long form
imp.rdb.dat <- complete(imp.rdb, "long", include = TRUE)

# Repeat time variable m + 1 times since impdat
# includes the original data as well as m imputations
imp.rdb.dat$time <- rep(rdb$time, imp.rdb$m + 1)

# Replace missing time values with time corresponding
# to the imputed cumulative hazard value
SUB <- imp.rdb.dat$.imp > 0 & is.na(imp.rdb.dat$time)
if(sum(SUB) > 0) {
  
  # Create a data frame with the unique event times
  # and corresponding cumulative hazards from the
  # complete case analysis
  bhz <- data.frame(time   = survfit(cox.fit)$time,
                    cumhaz = survfit(cox.fit)$cumhaz)
  
  # The following only works if pmm (the default) was used
  # to impute missing cumhaz values (because it relies on
  # the imputed values being values present in the non-missing
  # values)
  for(i in 1:sum(SUB)) {
    # Use max since last 2 times have the same cumhaz
    imp.rdb.dat$time[SUB][i] <-
      max(bhz$time[bhz$cumhaz == imp.rdb.dat$cumhaz[SUB][i]])
  }
}

# Convert back to a mids object
imp.rdb.new <- as.mids(imp.rdb.dat)

# fit the cox model 
#fit.imp.cox <- with(imp.natality.new,
#                coxph(Surv(gestage37, preterm01) ~
#                      RF_PPTERM + MAGER + MRACEHISP + DMAR))
# Do NOT include the -1 here since a Cox model has no intercept
# summary(pool(fit.imp.cox), conf.int = T,
#         exponentiate = T)[, c("term", "estimate", "2.5 %", "97.5 %", "p.value")]
#round.summary(fit.imp.cox, digits = 3,
#              exponentiate = T)[, c("estimate", "2.5 %", "97.5 %", "p.value")]

#-MULTIVARIATE ANALYSIS FOR ALL PATIENTS----------------------------------------


allcox = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                   dial + apkd01 +
                                   bmic + sex + age + diabetes))
summary(pool(allcox))
est.allcox <- pool(allcox)
summary(est.allcox, conf.int = TRUE, exponentiate = TRUE)

# with interaction?
allcox.int = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                   dial + apkd01*diabetes +
                                   bmic + sex + age))
summary(pool(allcox.int))
est.allcoxint <- pool(allcox.int)
summary(est.allcoxint, conf.int = TRUE, exponentiate = TRUE)

# apkd011:diabetes1 0.4604450 0.600500515 -1.2915256 1535.8221 1.967158e-01 0.1417842 1.4952985
# -> there is no interaction

# empty model for comparaison
est.allcox.e = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                    dial + apkd01 +
                                    sex + age + diabetes))
D1(allcox, est.allcox.e)

#-BIVARIATE ANALYSIS------------------------------------------------------------

# cardiovascular
cox.t2.cardiovasc = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc))
summary(pool(cox.t2.cardiovasc))
est.card <- pool(cox.t2.cardiovasc)
summary(est.card, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$cardiovasc, useNA = "always")

# tabac2
cox.t2.tab = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ tabac2))
summary(pool(cox.t2.tab))
est.tab <- pool(cox.t2.tab)
summary(est.tab, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$tabac2, useNA = "always")

# dial
cox.t2.dial = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ dial))
summary(pool(cox.t2.dial))
est.dial <- pool(cox.t2.dial)
summary(est.dial, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$dial, useNA = "always")

# adpkd
cox.t2.adpkd = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ apkd01))
summary(pool(cox.t2.adpkd))
est.adpkd <- pool(cox.t2.adpkd)
summary(est.adpkd, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$apkd01)

# bmic
cox.t2.bmic = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ bmic))
summary(pool(cox.t2.bmic))
est.bmic <- pool(cox.t2.bmic)
summary(est.bmic, conf.int = TRUE, exponentiate = TRUE)

cox.t2.bmicE = with(rdb.imputed, coxph(Surv(time, status=="1") ~ 1))
library("mitml")
D1(cox.t2.bmic, cox.t2.bmicE)
  # events
  table(rdb$status, rdb$bmic)


# sex
cox.t2.sex = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ sex))
summary(pool(cox.t2.sex))
est.sex <- pool(cox.t2.sex)
summary(est.sex, conf.int = TRUE, exponentiate = TRUE)
  # event
  table(rdb$status, rdb$sex)

# age
cox.t2.age = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ age))
summary(pool(cox.t2.age))
est.age <- pool(cox.t2.age)
summary(est.age, conf.int = TRUE, exponentiate = TRUE)
  

# diabetes
cox.t2.dia = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ diabetes))
summary(pool(cox.t2.dia))
est.dia <- pool(cox.t2.dia)
summary(est.dia, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$diabetes)
  
######

#-EXCLUSION OF THE DIABETIC PATIENTS--------------------------------------------
  
  #-NEW DATASET EXCLUDING DIABETIC PATIENTS 
  
  table(rdb$diabetesMISS, useNA = "always")
  #     0     1 
  # 22943 20156 
  str(rdb$diabetes)
  
  rdball.nodiab <- rdb[(rdb$diabetesMISS=="0"),]
  dim(rdball.nodiab) # 22943   109
  
  #-CUMHAZ TO IMPUTE--------------------------------------------------------------
  
  library(survminer)
  library(survival)
  
  rdball.nodiab$cardiovasc <- as.factor(rdball.nodiab$cardiovasc)
  rdball.nodiab$tabac2 <- as.factor(rdball.nodiab$tabac2)
  rdball.nodiab$dial <- as.factor(rdball.nodiab$dial)
  rdball.nodiab$apkd01 <- as.factor(rdball.nodiab$apkd01)
  rdball.nodiab$sex <- as.factor(rdball.nodiab$sex)
  rdball.nodiab$age <- as.numeric(as.character(rdball.nodiab$age))
  rdball.nodiab$status <- as.factor(rdball.nodiab$status)
  rdball.nodiab$time <- as.numeric(as.character(rdball.nodiab$time))
  rdball.nodiab$bmic <- as.factor(rdball.nodiab$bmic)
  
  cox.fit <- coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                     dial + apkd01 +
                     bmic + sex + age, data = rdball.nodiab)
  summary(cox.fit)
  
  #cox.fit2 <- coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
  #                   dial + apkd01*diabetes +
  #                   bmic + sex + age , data = rdball.nodiab)
  #summary(cox.fit2)
  
  # -> adding the interaction term in the model does not affect the result.
  # -> there is no interaction between diabetes and adpkd
  
  # survfit(), in addition to the survival function, also computes the cumulative baseline hazard function.
  
  SF <- survfit(cox.fit)
  rdball.nodiab$cumhaz <- NA
  for(i in 1:nrow(rdball.nodiab)) {
    #Can only compute at non-missing times
    if (!is.na(rdball.nodiab$time[i])) {
      rdball.nodiab$cumhaz[i] <- 
        summary(SF, times = rdball.nodiab$time[i])$cumhaz
    }
  }
  
  rdball.nodiab.for.imp <- rdball.nodiab %>% 
    select(cumhaz, cardiovasc, tabac2, dial, apkd01, 
           sex, age, diabetes, bmic, status)
  
  summary(rdball.nodiab.for.imp)
  
  # data imputation
  
  library("mice")
  imp.rdball.nodiab <- mice(rdball.nodiab.for.imp,
                  seed  = 21051986,
                  m     = 20, #nimpute(rdball.nodiab.for.imp),
                  maxit = 10,
                  meth = "pmm",
                  print = F)
  imp.rdball.nodiab
  
  
  # Checking... Is the data in the same order as before?
  # First imputation
  imp.rdball.nodiab.dat <- complete(imp.rdball.nodiab)
  table(imp.rdball.nodiab.dat$tabac2, rdball.nodiab$tabac2, exclude=NULL)
  # etc...
  # All in the same order
  
  # Imputed datasets in long form
  imp.rdball.nodiab.dat <- complete(imp.rdball.nodiab, "long", include = TRUE)
  
  # Repeat time variable m + 1 times since impdat
  # includes the original data as well as m imputations
  imp.rdball.nodiab.dat$time <- rep(rdball.nodiab$time, imp.rdball.nodiab$m + 1)
  
  # Replace missing time values with time corresponding
  # to the imputed cumulative hazard value
  SUB <- imp.rdball.nodiab.dat$.imp > 0 & is.na(imp.rdball.nodiab.dat$time)
  if(sum(SUB) > 0) {
    
    # Create a data frame with the unique event times
    # and corresponding cumulative hazards from the
    # complete case analysis
    bhz <- data.frame(time   = survfit(cox.fit)$time,
                      cumhaz = survfit(cox.fit)$cumhaz)
    
    # The following only works if pmm (the default) was used
    # to impute missing cumhaz values (because it relies on
    # the imputed values being values present in the non-missing
    # values)
    for(i in 1:sum(SUB)) {
      # Use max since last 2 times have the same cumhaz
      imp.rdball.nodiab.dat$time[SUB][i] <-
        max(bhz$time[bhz$cumhaz == imp.rdball.nodiab.dat$cumhaz[SUB][i]])
    }
  }
  
  # Convert back to a mids object
  imp.rdball.nodiab.new <- as.mids(imp.rdball.nodiab.dat)
  
  # fit the cox model 
  #fit.imp.cox <- with(imp.natality.new,
  #                coxph(Surv(gestage37, preterm01) ~
  #                      RF_PPTERM + MAGER + MRACEHISP + DMAR))
  # Do NOT include the -1 here since a Cox model has no intercept
  # summary(pool(fit.imp.cox), conf.int = T,
  #         exponentiate = T)[, c("term", "estimate", "2.5 %", "97.5 %", "p.value")]
  #round.summary(fit.imp.cox, digits = 3,
  #              exponentiate = T)[, c("estimate", "2.5 %", "97.5 %", "p.value")]
  
  #-MULTIVARIATE ANALYSIS FOR ALL PATIENTS----------------------------------------
  
  
  allcox = with(imp.rdball.nodiab.new, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                     dial + apkd01 +
                                     bmic + sex + age))
  summary(pool(allcox))
  est.allcox <- pool(allcox)
  summary(est.allcox, conf.int = TRUE, exponentiate = TRUE)
  
  # with interaction?
  allcox.int = with(imp.rdball.nodiab.new, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                         dial + apkd01*diabetes +
                                         bmic + sex + age))
  summary(pool(allcox.int))
  est.allcoxint <- pool(allcox.int)
  summary(est.allcoxint, conf.int = TRUE, exponentiate = TRUE)
  
  # apkd011:diabetes1 0.4604450 0.600500515 -1.2915256 1535.8221 1.967158e-01 0.1417842 1.4952985
  # -> there is no interaction
  
  # empty model for comparaison
  est.allcox.e = with(imp.rdball.nodiab.new, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                           dial + apkd01 +
                                           sex + age + diabetes))
  D1(allcox, est.allcox.e)
  
  #-BIVARIATE ANALYSIS------------------------------------------------------------
  
  # cardiovascular
  cox.t2.cardiovasc = with(imp.rdball.nodiab.new, coxph(Surv(time, status=="1") ~ cardiovasc))
  summary(pool(cox.t2.cardiovasc))
  est.card <- pool(cox.t2.cardiovasc)
  summary(est.card, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdball.nodiab$status, rdball.nodiab$cardiovasc, useNA = "always")
  
  # tabac2
  cox.t2.tab = with(imp.rdball.nodiab.new, coxph(Surv(time, status=="1") ~ tabac2))
  summary(pool(cox.t2.tab))
  est.tab <- pool(cox.t2.tab)
  summary(est.tab, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdball.nodiab$status, rdball.nodiab$tabac2, useNA = "always")
  
  # dial
  cox.t2.dial = with(imp.rdball.nodiab.new, coxph(Surv(time, status=="1") ~ dial))
  summary(pool(cox.t2.dial))
  est.dial <- pool(cox.t2.dial)
  summary(est.dial, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdball.nodiab$status, rdball.nodiab$dial, useNA = "always")
  
  # adpkd
  cox.t2.adpkd = with(imp.rdball.nodiab.new, coxph(Surv(time, status=="1") ~ apkd01))
  summary(pool(cox.t2.adpkd))
  est.adpkd <- pool(cox.t2.adpkd)
  summary(est.adpkd, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdball.nodiab$status, rdball.nodiab$apkd01)
  
  # bmic
  cox.t2.bmic = with(imp.rdball.nodiab.new, coxph(Surv(time, status=="1") ~ bmic))
  summary(pool(cox.t2.bmic))
  est.bmic <- pool(cox.t2.bmic)
  summary(est.bmic, conf.int = TRUE, exponentiate = TRUE)
  
  cox.t2.bmicE = with(rdball.nodiab.imputed, coxph(Surv(time, status=="1") ~ 1))
  library("mitml")
  D1(cox.t2.bmic, cox.t2.bmicE)
  # events
  table(rdball.nodiab$status, rdball.nodiab$bmic)
  
  # sex
  cox.t2.sex = with(imp.rdball.nodiab.new, coxph(Surv(time, status=="1") ~ sex))
  summary(pool(cox.t2.sex))
  est.sex <- pool(cox.t2.sex)
  summary(est.sex, conf.int = TRUE, exponentiate = TRUE)
  # event
  table(rdball.nodiab$status, rdball.nodiab$sex)
  
  # age
  cox.t2.age = with(imp.rdball.nodiab.new, coxph(Surv(time, status=="1") ~ age))
  summary(pool(cox.t2.age))
  est.age <- pool(cox.t2.age)
  summary(est.age, conf.int = TRUE, exponentiate = TRUE)
  
  
  # diabetes
  cox.t2.dia = with(imp.rdball.nodiab.new, coxph(Surv(time, status=="1") ~ diabetes))
  summary(pool(cox.t2.dia))
  est.dia <- pool(cox.t2.dia)
  summary(est.dia, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdball.nodiab$status, rdball.nodiab$diabetes)
  
######
  
#-COMPLETE CASE ANALYSIS--------------------------------------------------------

# DATASET
  
dim(rdb) # 45026   109

rdbcomplete = subset(rdb, select = c(cardiovasc, tabac2, dial, apkd01, bmic, sex,
                                     age, diabetes, status, time))
dim(rdbcomplete) # 45026     10
rdbcomplete = na.omit(rdbcomplete)
dim(rdbcomplete) # 29182     10

#-MULTIVARIATE ANALYSIS---------------------------------------------------------

allcox.compl = coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                               dial + apkd01 +
                                               bmic + sex + age
                     + diabetes, data = rdbcomplete)
summary(allcox.compl)

allcox.complE = coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                       dial + apkd01 +
                       sex + age
                     + diabetes, data = rdbcomplete)
D1(allcox.compl, allcox.complE)


allcox.complInt = coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                          dial + apkd01*diabetes +
                          bmic + sex + age, data = rdbcomplete)
summary(allcox.complInt)

#-BIVARIATE ANALYSIS------------------------------------------------------------

# cardiovascular
compl.cardio = coxph(Surv(time, status=="1") ~ cardiovasc, data = rdbcomplete)
summary(compl.cardio)
  # events
  table(rdbcomplete$status, rdbcomplete$cardiovasc, useNA = "always")

# tabac2
compl.tab = coxph(Surv(time, status=="1") ~ tabac2, data = rdbcomplete)
summary(compl.tab)
# events
table(rdbcomplete$status, rdbcomplete$tabac2, useNA = "always")

# dial
compl.dial = coxph(Surv(time, status=="1") ~ dial, data = rdbcomplete)
summary(compl.dial)
  # events
  table(rdbcomplete$status, rdbcomplete$dial, useNA = "always")

# adpkd
compl.adpkd = coxph(Surv(time, status=="1") ~ apkd01, data = rdbcomplete)
summary(compl.adpkd)
  # events
  table(rdbcomplete$status, rdbcomplete$apkd01)

# bmic
compl.bmic = coxph(Surv(time, status=="1") ~ bmic, data = rdbcomplete)
summary(compl.bmic)

cox.t2.bmicE = with(rdbcomplete.imputed, coxph(Surv(time, status=="1") ~ 1))
library("mitml")
D1(cox.t2.bmic, cox.t2.bmicE)
  # events
  table(rdbcomplete$status, rdbcomplete$bmic)

# sex
compl.sex = coxph(Surv(time, status=="1") ~ sex, data = rdbcomplete)
summary(compl.sex)
  # event
  table(rdbcomplete$status, rdbcomplete$sex)

# age
compl.age = coxph(Surv(time, status=="1") ~ age, data = rdbcomplete)
summary(compl.age)

# diabetes
comp.dia = coxph(Surv(time, status=="1") ~ diabetes, data = rdbcomplete)
summary(comp.dia)
  # events
  table(rdbcomplete$status, rdbcomplete$diabetes)

##########

#-ADDING ATCD VARIABLE TO THE ANALYSIS--------------------------------------------
  
  #-CUMHAZ TO IMPUTE--------------------------------------------------------------
  
  library(survminer)
  library(survival)
  
  rdb$cardiovasc <- as.factor(rdb$cardiovasc)
  rdb$tabac2 <- as.factor(rdb$tabac2)
  rdb$dial <- as.factor(rdb$dial)
  rdb$apkd01 <- as.factor(rdb$apkd01)
  rdb$sex <- as.factor(rdb$sex)
  rdb$age <- as.numeric(as.character(rdb$age))
  rdb$diabetes <- as.factor(rdb$diabetes)
  rdb$status <- as.factor(rdb$status)
  rdb$time <- as.numeric(as.character(rdb$time))
  rdb$bmic <- as.factor(rdb$bmic)
  rdb$atcd <- as.factor(rdb$atcd)

  cox.fit <- coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                     dial + apkd01 +
                     bmic + sex + age + diabetes + atcd, data = rdb)
  summary(cox.fit)
  
  #cox.fit2 <- coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
  #                   dial + apkd01*diabetes +
  #                   bmic + sex + age , data = rdb)
  #summary(cox.fit2)
  
  # -> adding the interaction term in the model does not affect the result.
  # -> there is no interaction between diabetes and adpkd
  
  # survfit(), in addition to the survival function, also computes the cumulative baseline hazard function.
  
  SF <- survfit(cox.fit)
  rdb$cumhaz <- NA
  for(i in 1:nrow(rdb)) {
    #Can only compute at non-missing times
    if (!is.na(rdb$time[i])) {
      rdb$cumhaz[i] <- 
        summary(SF, times = rdb$time[i])$cumhaz
    }
  }
  
  rdb.for.imp <- rdb %>% 
    select(cumhaz, cardiovasc, tabac2, dial, apkd01, 
           sex, age, diabetes, bmic, status, atcd)
  
  summary(rdb.for.imp)
  
  # data imputation
  
  library("mice")
  imp.rdb <- mice(rdb.for.imp,
                  seed  = 21051986,
                  m     = 20, #nimpute(rdb.for.imp),
                  maxit = 10,
                  meth = "pmm",
                  print = F)
  imp.rdb
  
  
  # Checking... Is the data in the same order as before?
  # First imputation
  imp.rdb.dat <- complete(imp.rdb)
  table(imp.rdb.dat$tabac2, rdb$tabac2, exclude=NULL)
  # etc...
  # All in the same order
  
  # Imputed datasets in long form
  imp.rdb.dat <- complete(imp.rdb, "long", include = TRUE)
  
  # Repeat time variable m + 1 times since impdat
  # includes the original data as well as m imputations
  imp.rdb.dat$time <- rep(rdb$time, imp.rdb$m + 1)
  
  # Replace missing time values with time corresponding
  # to the imputed cumulative hazard value
  SUB <- imp.rdb.dat$.imp > 0 & is.na(imp.rdb.dat$time)
  if(sum(SUB) > 0) {
    
    # Create a data frame with the unique event times
    # and corresponding cumulative hazards from the
    # complete case analysis
    bhz <- data.frame(time   = survfit(cox.fit)$time,
                      cumhaz = survfit(cox.fit)$cumhaz)
    
    # The following only works if pmm (the default) was used
    # to impute missing cumhaz values (because it relies on
    # the imputed values being values present in the non-missing
    # values)
    for(i in 1:sum(SUB)) {
      # Use max since last 2 times have the same cumhaz
      imp.rdb.dat$time[SUB][i] <-
        max(bhz$time[bhz$cumhaz == imp.rdb.dat$cumhaz[SUB][i]])
    }
  }
  
  # Convert back to a mids object
  imp.rdb.new <- as.mids(imp.rdb.dat)
  
  # fit the cox model 
  #fit.imp.cox <- with(imp.natality.new,
  #                coxph(Surv(gestage37, preterm01) ~
  #                      RF_PPTERM + MAGER + MRACEHISP + DMAR))
  # Do NOT include the -1 here since a Cox model has no intercept
  # summary(pool(fit.imp.cox), conf.int = T,
  #         exponentiate = T)[, c("term", "estimate", "2.5 %", "97.5 %", "p.value")]
  #round.summary(fit.imp.cox, digits = 3,
  #              exponentiate = T)[, c("estimate", "2.5 %", "97.5 %", "p.value")]
  
  #-MULTIVARIATE ANALYSIS FOR ALL PATIENTS----------------------------------------
  
  
  allcox = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                     dial + apkd01 +
                                     bmic + sex + age + diabetes + atcd))
  summary(pool(allcox))
  est.allcox <- pool(allcox)
  summary(est.allcox, conf.int = TRUE, exponentiate = TRUE)
  
  # with interaction?
  allcox.int = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc*atcd + tabac2 +
                                         dial + apkd01 + diabetes +
                                         bmic + sex + age))
  summary(pool(allcox.int))
  est.allcoxint <- pool(allcox.int)
  summary(est.allcoxint, conf.int = TRUE, exponentiate = TRUE)
  
  # apkd011:diabetes1 0.4604450 0.600500515 -1.2915256 1535.8221 1.967158e-01 0.1417842 1.4952985
  # -> there is no interaction
  
  # empty model for comparaison
  allcox.inte = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                           dial + apkd01 +
                                           sex + age + diabetes + atcd))
  D1(allcox.int, allcox.inte)
  
  #-BIVARIATE ANALYSIS------------------------------------------------------------
  
  # cardiovascular
  cox.t2.cardiovasc = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc))
  summary(pool(cox.t2.cardiovasc))
  est.card <- pool(cox.t2.cardiovasc)
  summary(est.card, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$cardiovasc, useNA = "always")
  
  # tabac2
  cox.t2.tab = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ tabac2))
  summary(pool(cox.t2.tab))
  est.tab <- pool(cox.t2.tab)
  summary(est.tab, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$tabac2, useNA = "always")
  
  # dial
  cox.t2.dial = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ dial))
  summary(pool(cox.t2.dial))
  est.dial <- pool(cox.t2.dial)
  summary(est.dial, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$dial, useNA = "always")
  
  # adpkd
  cox.t2.adpkd = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ apkd01))
  summary(pool(cox.t2.adpkd))
  est.adpkd <- pool(cox.t2.adpkd)
  summary(est.adpkd, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$apkd01)
  
  # bmic
  cox.t2.bmic = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ bmic))
  summary(pool(cox.t2.bmic))
  est.bmic <- pool(cox.t2.bmic)
  summary(est.bmic, conf.int = TRUE, exponentiate = TRUE)
  
  cox.t2.bmicE = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ 1))
  library("mitml")
  D1(cox.t2.bmic, cox.t2.bmicE)
  # events
  table(rdb$status, rdb$bmic)
  
  # sex
  cox.t2.sex = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ sex))
  summary(pool(cox.t2.sex))
  est.sex <- pool(cox.t2.sex)
  summary(est.sex, conf.int = TRUE, exponentiate = TRUE)
  # event
  table(rdb$status, rdb$sex)
  
  # age
  cox.t2.age = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ age))
  summary(pool(cox.t2.age))
  est.age <- pool(cox.t2.age)
  summary(est.age, conf.int = TRUE, exponentiate = TRUE)
  
  # diabetes
  cox.t2.dia = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ diabetes))
  summary(pool(cox.t2.dia))
  est.dia <- pool(cox.t2.dia)
  summary(est.dia, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$diabetes)
  
  # atcd
  cox.t2.atcd = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ atcd))
  summary(pool(cox.t2.atcd))
  est.atcd <- pool(cox.t2.atcd)
  summary(est.atcd, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$atcd)
  
#####
  
  #-ADDING ATCD VARIABLE TO THE ANALYSIS-AND ELIMINATING CARDIVOCASCULAR EVENT----
  
  #-CUMHAZ TO IMPUTE--------------------------------------------------------------
  
  library(survminer)
  library(survival)
  
  rdb$cardiovasc <- as.factor(rdb$cardiovasc)
  rdb$tabac2 <- as.factor(rdb$tabac2)
  rdb$dial <- as.factor(rdb$dial)
  rdb$apkd01 <- as.factor(rdb$apkd01)
  rdb$sex <- as.factor(rdb$sex)
  rdb$age <- as.numeric(as.character(rdb$age))
  rdb$diabetes <- as.factor(rdb$diabetes)
  rdb$status <- as.factor(rdb$status)
  rdb$time <- as.numeric(as.character(rdb$time))
  rdb$bmic <- as.factor(rdb$bmic)
  rdb$atcd <- as.factor(rdb$atcd)
  
  cox.fit <- coxph(Surv(time, status=="1") ~ tabac2 +
                     dial + apkd01 +
                     bmic + sex + age + diabetes + atcd, data = rdb)
  summary(cox.fit)
  
  #cox.fit2 <- coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
  #                   dial + apkd01*diabetes +
  #                   bmic + sex + age , data = rdb)
  #summary(cox.fit2)
  
  # -> adding the interaction term in the model does not affect the result.
  # -> there is no interaction between diabetes and adpkd
  
  # survfit(), in addition to the survival function, also computes the cumulative baseline hazard function.
  
  SF <- survfit(cox.fit)
  rdb$cumhaz <- NA
  for(i in 1:nrow(rdb)) {
    #Can only compute at non-missing times
    if (!is.na(rdb$time[i])) {
      rdb$cumhaz[i] <- 
        summary(SF, times = rdb$time[i])$cumhaz
    }
  }
  
  rdb.for.imp <- rdb %>% 
    select(cumhaz, tabac2, dial, apkd01, 
           sex, age, diabetes, bmic, status, atcd)
  
  summary(rdb.for.imp)
  
  # data imputation
  
  library("mice")
  imp.rdb <- mice(rdb.for.imp,
                  seed  = 21051986,
                  m     = 20, #nimpute(rdb.for.imp),
                  maxit = 10,
                  meth = "pmm",
                  print = F)
  imp.rdb
  
  
  # Checking... Is the data in the same order as before?
  # First imputation
  imp.rdb.dat <- complete(imp.rdb)
  table(imp.rdb.dat$tabac2, rdb$tabac2, exclude=NULL)
  # etc...
  # All in the same order
  
  # Imputed datasets in long form
  imp.rdb.dat <- complete(imp.rdb, "long", include = TRUE)
  
  # Repeat time variable m + 1 times since impdat
  # includes the original data as well as m imputations
  imp.rdb.dat$time <- rep(rdb$time, imp.rdb$m + 1)
  
  # Replace missing time values with time corresponding
  # to the imputed cumulative hazard value
  SUB <- imp.rdb.dat$.imp > 0 & is.na(imp.rdb.dat$time)
  if(sum(SUB) > 0) {
    
    # Create a data frame with the unique event times
    # and corresponding cumulative hazards from the
    # complete case analysis
    bhz <- data.frame(time   = survfit(cox.fit)$time,
                      cumhaz = survfit(cox.fit)$cumhaz)
    
    # The following only works if pmm (the default) was used
    # to impute missing cumhaz values (because it relies on
    # the imputed values being values present in the non-missing
    # values)
    for(i in 1:sum(SUB)) {
      # Use max since last 2 times have the same cumhaz
      imp.rdb.dat$time[SUB][i] <-
        max(bhz$time[bhz$cumhaz == imp.rdb.dat$cumhaz[SUB][i]])
    }
  }
  
  # Convert back to a mids object
  imp.rdb.new <- as.mids(imp.rdb.dat)
  
  # fit the cox model 
  #fit.imp.cox <- with(imp.natality.new,
  #                coxph(Surv(gestage37, preterm01) ~
  #                      RF_PPTERM + MAGER + MRACEHISP + DMAR))
  # Do NOT include the -1 here since a Cox model has no intercept
  # summary(pool(fit.imp.cox), conf.int = T,
  #         exponentiate = T)[, c("term", "estimate", "2.5 %", "97.5 %", "p.value")]
  #round.summary(fit.imp.cox, digits = 3,
  #              exponentiate = T)[, c("estimate", "2.5 %", "97.5 %", "p.value")]
  
  #-MULTIVARIATE ANALYSIS FOR ALL PATIENTS----------------------------------------
  
  
  allcox = with(imp.rdb.new, coxph(Surv(time, status=="1") ~  tabac2 +
                                     dial + apkd01 +
                                     bmic + sex + age + diabetes + atcd))
  summary(pool(allcox))
  est.allcox <- pool(allcox)
  summary(est.allcox, conf.int = TRUE, exponentiate = TRUE)
  
  # with interaction?
  allcox.int = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc*atcd + tabac2 +
                                         dial + apkd01 + diabetes +
                                         bmic + sex + age))
  summary(pool(allcox.int))
  est.allcoxint <- pool(allcox.int)
  summary(est.allcoxint, conf.int = TRUE, exponentiate = TRUE)
  
  # apkd011:diabetes1 0.4604450 0.600500515 -1.2915256 1535.8221 1.967158e-01 0.1417842 1.4952985
  # -> there is no interaction
  
  # empty model for comparaison
  allcox.inte = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
                                          dial + apkd01 +
                                          sex + age + diabetes + atcd))
  D1(allcox.int, allcox.inte)
  
  #-BIVARIATE ANALYSIS------------------------------------------------------------
  
  # tabac2
  cox.t2.tab = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ tabac2))
  summary(pool(cox.t2.tab))
  est.tab <- pool(cox.t2.tab)
  summary(est.tab, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$tabac2, useNA = "always")
  
  # dial
  cox.t2.dial = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ dial))
  summary(pool(cox.t2.dial))
  est.dial <- pool(cox.t2.dial)
  summary(est.dial, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$dial, useNA = "always")
  
  # adpkd
  cox.t2.adpkd = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ apkd01))
  summary(pool(cox.t2.adpkd))
  est.adpkd <- pool(cox.t2.adpkd)
  summary(est.adpkd, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$apkd01)
  
  # bmic
  cox.t2.bmic = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ bmic))
  summary(pool(cox.t2.bmic))
  est.bmic <- pool(cox.t2.bmic)
  summary(est.bmic, conf.int = TRUE, exponentiate = TRUE)
  
  cox.t2.bmicE = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ 1))
  library("mitml")
  D1(cox.t2.bmic, cox.t2.bmicE)
  # events
  table(rdb$status, rdb$bmic)
  
  # sex
  cox.t2.sex = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ sex))
  summary(pool(cox.t2.sex))
  est.sex <- pool(cox.t2.sex)
  summary(est.sex, conf.int = TRUE, exponentiate = TRUE)
  # event
  table(rdb$status, rdb$sex)
  
  # age
  cox.t2.age = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ age))
  summary(pool(cox.t2.age))
  est.age <- pool(cox.t2.age)
  summary(est.age, conf.int = TRUE, exponentiate = TRUE)
  
  # diabetes
  cox.t2.dia = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ diabetes))
  summary(pool(cox.t2.dia))
  est.dia <- pool(cox.t2.dia)
  summary(est.dia, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$diabetes)
  
  # atcd
  cox.t2.atcd = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ atcd))
  summary(pool(cox.t2.atcd))
  est.atcd <- pool(cox.t2.atcd)
  summary(est.atcd, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb$status, rdb$atcd)
  
  #####
  
  #-ADDING ATCD VARIABLE TO THE ANALYSIS-AND ELIMINATING CARDIVOCASCULAR EVENT----
  #-ONLY DIABETIC PATIENTS--------------------------------------------------------
  
  #-CUMHAZ TO IMPUTE--------------------------------------------------------------
  
  library(survminer)
  library(survival)
  
  rdb$cardiovasc <- as.factor(rdb$cardiovasc)
  rdb$tabac2 <- as.factor(rdb$tabac2)
  rdb$dial <- as.factor(rdb$dial)
  rdb$apkd01 <- as.factor(rdb$apkd01)
  rdb$sex <- as.factor(rdb$sex)
  rdb$age <- as.numeric(as.character(rdb$age))
  rdb$diabetes <- as.factor(rdb$diabetes)
  rdb$status <- as.factor(rdb$status)
  rdb$time <- as.numeric(as.character(rdb$time))
  rdb$bmic <- as.factor(rdb$bmic)
  rdb$atcd <- as.factor(rdb$atcd)
  
  table(rdb$diabetesMISS)
  #     0     1  miss 
  # 22943 20156  1927 
  
  rdb.nodiab <- rdb[(rdb$diabetesMISS == "0"),]
  dim(rdb.nodiab)
  
  cox.fit <- coxph(Surv(time, status=="1") ~ tabac2 +
                     dial + apkd01 +
                     bmic + sex + age + atcd, data = rdb.nodiab)
  summary(cox.fit)
  
  #cox.fit2 <- coxph(Surv(time, status=="1") ~ cardiovasc + tabac2 +
  #                   dial + apkd01*diabetes +
  #                   bmic + sex + age , data = rdb)
  #summary(cox.fit2)
  
  # -> adding the interaction term in the model does not affect the result.
  # -> there is no interaction between diabetes and adpkd
  
  # survfit(), in addition to the survival function, also computes the cumulative baseline hazard function.
  
  SF <- survfit(cox.fit)
  rdb$cumhaz <- NA
  for(i in 1:nrow(rdb.nodiab)) {
    #Can only compute at non-missing times
    if (!is.na(rdb.nodiab$time[i])) {
      rdb.nodiab$cumhaz[i] <- 
        summary(SF, times = rdb.nodiab$time[i])$cumhaz
    }
  }
  
  rdb.for.imp <- rdb.nodiab %>% 
    select(cumhaz, tabac2, dial, apkd01, 
           sex, age, bmic, status, atcd)
  
  summary(rdb.for.imp)
  
  # data imputation
  
  library("mice")
  imp.rdb <- mice(rdb.for.imp,
                  seed  = 21051986,
                  m     = 20, #nimpute(rdb.for.imp),
                  maxit = 10,
                  meth = "pmm",
                  print = F)
  imp.rdb
  
  
  # Checking... Is the data in the same order as before?
  # First imputation
  imp.rdb.dat <- complete(imp.rdb)
  table(imp.rdb.dat$tabac2, rdb.nodiab$tabac2, exclude=NULL)
  # etc...
  # All in the same order
  
  # Imputed datasets in long form
  imp.rdb.dat <- complete(imp.rdb, "long", include = TRUE)
  
  # Repeat time variable m + 1 times since impdat
  # includes the original data as well as m imputations
  imp.rdb.dat$time <- rep(rdb.nodiab$time, imp.rdb$m + 1)
  
  # Replace missing time values with time corresponding
  # to the imputed cumulative hazard value
  SUB <- imp.rdb.dat$.imp > 0 & is.na(imp.rdb.dat$time)
  if(sum(SUB) > 0) {
    
    # Create a data frame with the unique event times
    # and corresponding cumulative hazards from the
    # complete case analysis
    bhz <- data.frame(time   = survfit(cox.fit)$time,
                      cumhaz = survfit(cox.fit)$cumhaz)
    
    # The following only works if pmm (the default) was used
    # to impute missing cumhaz values (because it relies on
    # the imputed values being values present in the non-missing
    # values)
    for(i in 1:sum(SUB)) {
      # Use max since last 2 times have the same cumhaz
      imp.rdb.dat$time[SUB][i] <-
        max(bhz$time[bhz$cumhaz == imp.rdb.dat$cumhaz[SUB][i]])
    }
  }
  
  # Convert back to a mids object
  imp.rdb.new <- as.mids(imp.rdb.dat)
  
  # fit the cox model 
  #fit.imp.cox <- with(imp.natality.new,
  #                coxph(Surv(gestage37, preterm01) ~
  #                      RF_PPTERM + MAGER + MRACEHISP + DMAR))
  # Do NOT include the -1 here since a Cox model has no intercept
  # summary(pool(fit.imp.cox), conf.int = T,
  #         exponentiate = T)[, c("term", "estimate", "2.5 %", "97.5 %", "p.value")]
  #round.summary(fit.imp.cox, digits = 3,
  #              exponentiate = T)[, c("estimate", "2.5 %", "97.5 %", "p.value")]
  
  #-MULTIVARIATE ANALYSIS FOR ALL PATIENTS (non-diab)----------------------------------
  
  allcox = with(imp.rdb.new, coxph(Surv(time, status=="1") ~  tabac2 +
                                     dial + apkd01 +
                                     bmic + sex + age + atcd))
  summary(pool(allcox))
  est.allcox <- pool(allcox)
  summary(est.allcox, conf.int = TRUE, exponentiate = TRUE)
  
  # empty model for comparaison
  allcox.inte = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ tabac2 +
                                          dial + apkd01 +
                                          sex + age + atcd))
  D1(allcox, allcox.inte)
  
  #-BIVARIATE ANALYSIS------------------------------------------------------------
  
  # tabac2
  cox.t2.tab = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ tabac2))
  summary(pool(cox.t2.tab))
  est.tab <- pool(cox.t2.tab)
  summary(est.tab, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb.nodiab$status, rdb.nodiab$tabac2, useNA = "always")
  
  # dial
  cox.t2.dial = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ dial))
  summary(pool(cox.t2.dial))
  est.dial <- pool(cox.t2.dial)
  summary(est.dial, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb.nodiab$status, rdb.nodiab$dial, useNA = "always")
  
  # adpkd
  cox.t2.adpkd = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ apkd01))
  summary(pool(cox.t2.adpkd))
  est.adpkd <- pool(cox.t2.adpkd)
  summary(est.adpkd, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb.nodiab$status, rdb.nodiab$apkd01)
  
  # bmic
  cox.t2.bmic = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ bmic))
  summary(pool(cox.t2.bmic))
  est.bmic <- pool(cox.t2.bmic)
  summary(est.bmic, conf.int = TRUE, exponentiate = TRUE)
  
  cox.t2.bmicE = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ 1))
  library("mitml")
  D1(cox.t2.bmic, cox.t2.bmicE)
  # events
  table(rdb.nodiab$status, rdb.nodiab$bmic)
  
  # sex
  cox.t2.sex = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ sex))
  summary(pool(cox.t2.sex))
  est.sex <- pool(cox.t2.sex)
  summary(est.sex, conf.int = TRUE, exponentiate = TRUE)
  # event
  table(rdb.nodiab$status, rdb.nodiab$sex)
  
  # age
  cox.t2.age = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ age))
  summary(pool(cox.t2.age))
  est.age <- pool(cox.t2.age)
  summary(est.age, conf.int = TRUE, exponentiate = TRUE)
  
  # atcd
  cox.t2.atcd = with(imp.rdb.new, coxph(Surv(time, status=="1") ~ atcd))
  summary(pool(cox.t2.atcd))
  est.atcd <- pool(cox.t2.atcd)
  summary(est.atcd, conf.int = TRUE, exponentiate = TRUE)
  # events
  table(rdb.nodiab$status, rdb.nodiab$atcd)
  
#####
  
  #-COMPLETE CASE ANALYSIS WITH ATCD---------------------------------------------
  
  # DATASET
  
  dim(rdb) # 45026   109
  
  rdbcomplete = subset(rdb, select = c(tabac2, dial, apkd01, bmic, sex,
                                       age, diabetes, status, time, atcd))
  dim(rdbcomplete) # 45026     10
  rdbcomplete = na.omit(rdbcomplete)
  dim(rdbcomplete) # 30657     10
  
  #-MULTIVARIATE ANALYSIS---------------------------------------------------------
  
  allcox.compl = coxph(Surv(time, status=="1") ~ atcd + tabac2 +
                         dial + apkd01 +
                         bmic + sex + age
                       + diabetes, data = rdbcomplete)
  summary(allcox.compl)
  
  allcox.complE = coxph(Surv(time, status=="1") ~ atcd + tabac2 +
                          dial + apkd01 +
                          sex + age
                        + diabetes, data = rdbcomplete)
  anova(allcox.compl, allcox.complE)
  
  
  allcox.complInt = coxph(Surv(time, status=="1") ~ atcd + tabac2 +
                            dial + apkd01*diabetes +
                            bmic + sex + age, data = rdbcomplete)
  summary(allcox.complInt)
  
  #-BIVARIATE ANALYSIS------------------------------------------------------------
  
  # cardiovascular
  compl.cardio = coxph(Surv(time, status=="1") ~ cardiovasc, data = rdbcomplete)
  summary(compl.cardio)
  # events
  table(rdbcomplete$status, rdbcomplete$cardiovasc, useNA = "always")
  
  # tabac2
  compl.tab = coxph(Surv(time, status=="1") ~ tabac2, data = rdbcomplete)
  summary(compl.tab)
  # events
  table(rdbcomplete$status, rdbcomplete$tabac2, useNA = "always")
  
  # dial
  compl.dial = coxph(Surv(time, status=="1") ~ dial, data = rdbcomplete)
  summary(compl.dial)
  # events
  table(rdbcomplete$status, rdbcomplete$dial, useNA = "always")
  
  # adpkd
  compl.adpkd = coxph(Surv(time, status=="1") ~ apkd01, data = rdbcomplete)
  summary(compl.adpkd)
  # events
  table(rdbcomplete$status, rdbcomplete$apkd01)
  
  # bmic
  compl.bmic = coxph(Surv(time, status=="1") ~ bmic, data = rdbcomplete)
  summary(compl.bmic)
  
  compl.bmicE = coxph(Surv(time, status=="1") ~ 1, data = rdbcomplete)
  library("mitml")
  anova(compl.bmic, compl.bmicE)
  # events
  table(rdbcomplete$status, rdbcomplete$bmic)
  
  # sex
  compl.sex = coxph(Surv(time, status=="1") ~ sex, data = rdbcomplete)
  summary(compl.sex)
  # event
  table(rdbcomplete$status, rdbcomplete$sex)
  
  # age
  compl.age = coxph(Surv(time, status=="1") ~ age, data = rdbcomplete)
  summary(compl.age)
  
  # diabetes
  comp.dia = coxph(Surv(time, status=="1") ~ diabetes, data = rdbcomplete)
  summary(comp.dia)
  # events
  table(rdbcomplete$status, rdbcomplete$diabetes)
  
  # atcd
  comp.atcd = coxph(Surv(time, status=="1") ~ atcd, data = rdbcomplete)
  summary(comp.atcd)
  # events
  table(rdbcomplete$status, rdbcomplete$atcd)
  
  
  
