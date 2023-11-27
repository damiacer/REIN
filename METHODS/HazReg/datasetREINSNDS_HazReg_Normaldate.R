getwd()
#setwd("//calebasse/cerasuo191/Documents/UNICAEN_PHD/PhD/M2/DATABASES_REIN/csv_data") 
setwd("/Users/damianocerasuolo/Desktop/PhD/M2/DATABASES_REIN/csv_data") 

#-PACKAGES-----------------------------------------------------------------------

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
#install.packages("knitr")
library("knitr")

#-DATABASE "REIN"----------------------------------------------------------------
rein <- read.csv2("rein_db.csv", header = TRUE, na.string="NA")
dim(rein)
names(rein)

#-HOSPITALISATION DATABSE (SNDS)-------------------------------------------------
hosp <- read.csv2("snds_hospit.csv", header = TRUE, na.string="NA")
names(hosp)
dim(hosp)

#-TREATMENT DATA (SNDS)----------------------------------------------------------
treat <- read.csv2("snds_medic.csv", header = TRUE, na.string="NA")
dim(treat)
names(treat)

#-CHANGES IN DIALYSIS TREATMENT DURING THE FU------------------------------------
switch <- read.csv2("rein_treatswitch.csv", header = TRUE, na.string="NA")
dim(switch)

# CORRESPONDENCE DATABASE (ACCROCHAGE)------------------------------------------
rein_s <- read.csv2("snds_rein.csv", header = TRUE, na.string="")
dim(rein_s)

#################################################################################

# MERGING "ACCROCHAGE" FOR SNDS AND REIN DATA
rein_m4 <- merge(rein, rein_s, by.x = "RREC_COD_ANO", by.y = "RREC_COD_ANO")
dim(rein_m4)
names(rein_m4)


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

#-REIN-SNDS----------------------------------------------------------------------

names(rein_h)

reinh2 = rein_h
summary(reinh2)
names(reinh2)
# num_enq = hosp
# RREC_COD_ANO = rein

#################################################################################

# ADDING TREATMENT TO BASE-------------------------------------------------------

# CREATE A DELIVRANCE DATE-------------------------------------------------------
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

# SELECT FIRST OCCURRENCE BY TIME DATE------------------------------------------
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

# MERGING THE FIRST TREATMENT FOR EACH PATENT------------------------------------

treat_line <- as_tibble(treat_line)
treat_line <- treat_line %>% rename(
  "num_enq" = "NUM_ENQ")

dim(treat_line)
# 33706     5

rein_m3 <- merge(rein_m4, treat_line, by.x = "num_enq", by.y = "num_enq",
                 all.x = TRUE, all.y = FALSE)

dim(rein_m3)
#45026    78

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

# EXLCUDE EVENTES BEFORE 2015----------------------------------------------

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
dim(dgn_atcd_first) # 10845     2

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

# DEFINITION OF THE EVENT FOR THE REIN DATABASE (AFTER THE MERGE)---------------

# FIRST STEP: WHEN DGN_PAL IS MISSING
rdb$DGN_PAL[is.na(rdb$DGN_PAL)] <- "E0"

# SECOND STEP: WHEN DGN_PAL IS NOT MISSING
rdb <- rdb %>% 
  mutate(DGN_PAL = case_when(
    # missing
    DGN_PAL == "E0" ~ "E0", 
    # embolie et thrombose artérielles (NO EVENT)
    DGN_PAL == "I740" | DGN_PAL == "I741" | DGN_PAL == "I742"  | DGN_PAL == "I743" | 
      DGN_PAL == "I744" | DGN_PAL == "I745" | DGN_PAL == "I748" ~ "E0",
    # accidentes ischémiques cérébraux transitoires et syndromes apparentés (G45 and G46)
    DGN_PAL == "G450" | DGN_PAL == "G451" | DGN_PAL == "G452" | DGN_PAL == "G453" | DGN_PAL == "G454" |
      DGN_PAL == "G458" | DGN_PAL == "G459" ~ "G45",
    DGN_PAL == "G460" | DGN_PAL == "G462" | DGN_PAL == "G463" | DGN_PAL == "G464" | DGN_PAL == "G465" |
      DGN_PAL == "G466" | DGN_PAL == "G467" | DGN_PAL == "G468" ~ "G46", 
    # hémiplégie (G81)
    DGN_PAL == "G810" | DGN_PAL == "G8100" | DGN_PAL == "G8101" | DGN_PAL == "G8108" ~ "G81",
    # anomalies du champ visuels (NO EVENT)
    DGN_PAL == "H534" ~ "E0",
    # hémorragie sous-arachnoïdienne (I60)
    DGN_PAL == "I600" | DGN_PAL == "I601" | DGN_PAL == "I602" | DGN_PAL == "I603" | DGN_PAL == "I604" |
      DGN_PAL == "I605" | DGN_PAL == "I606" | DGN_PAL == "I607" | DGN_PAL == "I608" | DGN_PAL == "I609" ~ "I60",
    # hémorragie intracérébrale (I61)
    DGN_PAL == "I610" | DGN_PAL == "I611" | DGN_PAL == "I612" | DGN_PAL == "I613" | DGN_PAL == "I614" |
      DGN_PAL == "I615" | DGN_PAL == "I616" | DGN_PAL == "I618" | DGN_PAL == "I619" ~ "I61",
    # infarctus cérébral (I63)
    DGN_PAL == "I630" | DGN_PAL == "I631" | DGN_PAL == "I632" | DGN_PAL == "I633" | DGN_PAL == "I634" | 
      DGN_PAL == "I635" | DGN_PAL == "I636" | DGN_PAL == "I6308" | DGN_PAL == "I639" ~ "I63",
    # accident vasculaire cérébral, non précisé comme étant hémorragique ou par infarctus (I64)
    DGN_PAL == "I64" ~ "I64",
    # dysphasie et aphasie (NO EVENT)
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

### NEW VARIABLES----------------------------------------------------------------

# DEATH VARIABLE

rdb$delai_DC[is.na(rdb$delai_DC)] <- "0"
rdb$DEATH[rdb$delai_DC == 0] <- "0"
rdb$DEATH[rdb$delai_DC > 0] <- "1"
table(rdb$DEATH)
#0     1 
#30686 14340 

# EVENT VARIABLE

rdb$EVENT[rdb$DGN_PAL == "E0"] <- "0"
rdb$EVENT[rdb$DGN_PAL != "E0"] <- "1"
table(rdb$EVENT)
#    0     1 
#43030  1996

# TRANSPLANTATION VARIABLE 

rdb$DGRF.d = as.numeric(as.Date(rdb$DGRF, "%d/%m/%Y"))
rdb$DGRF.d[is.na(rdb$DGRF.d)] <- 0
rdb$TRANSP = if_else(rdb$DGRF.d  > 0, "1", "0")
table(rdb$TRANSP)

# 0     1 
# 37730  7296 

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

# FOLLOW-UPS------------------------------------------------------------------

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

#-ATCD--------------------------------------------------------------------------

str(rdb$atcd)
rdb$atcd[is.na(rdb$atcd)] <- 0
table(rdb$atcd, useNA = "always")

#-TIME AND STATUS---------------------------------------------------------------

rdb$time = rdb$epilogus
rdb$time = as.numeric(as.character(rdb$time))

rdb$status = if_else(rdb$EVENTUM == "event", "1", "0")
table(rdb$status)
rdb$status = as.factor(rdb$status)

#-FINAL DATASET-----------------------------------------------------------------

table(rdb$METHOn)
dim(rdb) #45026   106
rdb <- rdb[!(rdb$METHOn=="3"),] # 43241   106
dim(rdb)
#table(rdb$status)
#0     1 
#41692  1549 

#-NUM_ENQ AND RREC_COD_ANO------------------------------------------------------

rdb$num_enqM = rdb$num_enq
rdb$RCODM = rdb$RREC_COD_ANO 

rdb <- rdb %>%
  separate(num_enqM, c(NA, "numeroen"))
str(rdb$numeroen)
rdb$numeroen = as.numeric(as.character(rdb$numeroen))
rdb$numeroen01 = rdb$numeroen 
rdb$numeroen01[is.na(rdb$numeroen)] <- 9999999999
rdb$numeroen01 = if_else(rdb$numeroen01 == 9999999999, "notmach", "match")
table(rdb$numeroen01)

rdb$RCODM[is.na(rdb$RCODM)] <- "missing"
rdb$RCODM2 = if_else(rdb$RCODM == "missing", "1", "0")
table(rdb$RCODM2) # is never missing

rdb <- rdb[!(rdb$numeroen01=="notmach"),] 
dim(rdb) # 40980 110

#-FOLLOWUP ACCORDING TO EVENT/DEATH---------------------------------------------

# event variable: EVENT 
# death variable: DEATH
# time variabble: TIME

# variables used in the model: cardiovasc + tabac2 +
# dial + apkd01 +
#  bmic + sex + age + diabetes

################################################################################
#-END OF THE DATASET CODE-------------------------------------------------------
################################################################################

#-no missing value dataset----

#-smaller dataset

rdb = subset(rdb, select = c(EVENT, time, cardiovasc, tabac2,
                             dial, apkd01, bmic, sex, age, diabetes))

dim(rdb) # 40980   10
rdb = na.omit(rdb)

#-normalised time variable----
# VE

rdb = as.data.frame(rdb)
lapply(rdb[, 1:6], class)
#rdb = na.omit(rdb)
#rdb = rdb[sample(1:nrow(rdb), size = 4000),]

# excluding patients with time = 0
rdb <- rdb[!(rdb$time < 10),] #### PATIENTS WITH A FOLLOW UP < 10 = EXCLUDED: 25946 

#-Normal time variable to test the model 
#rdb$time = rnorm(4000, mean = 120, sd = 20)
#-End of the Normal time variable

hist(rdb$time)
quantile(rdb$time, probs = seq(0, 1, 0.1))
# 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 10  365  603  723  727  735 1087 1094 1452 1466 1825 
table(rdb$time)
max(rdb$time)
min(rdb$time)
# creating a class time variable based on quantiles 

rdb <- rdb %>%
  mutate(timecl = case_when(
    time <= 364 ~ "1",
    time > 364 & time <= 727 ~ "2",
    time > 727 & time <= 1087 ~ "3",
    time > 1087 & time <= 1453 ~ "4", # ans
    time > 1453 ~ "5"
  ))
table(rdb$timecl, useNA = "always")
#1    2    3    4    5 <NA> 
#2528 8044 5071 5370 4933    0 
table(rdb$time, rdb$timecl)

# create a normally distributed variable to replace the time
# the qunorms

#install.packages("dyngen")
require("dyngen")

q1 <- rnorm_bounded(25946, mean = 182, sd = 80, min = 10, max = 364)
q2 <- rnorm_bounded(25946, mean = 545, sd = 160, min = 364, max = 727)
q3 <- rnorm_bounded(25946, mean = 907.5, sd = 320, min = 728, max = 1087)
q4 <- rnorm_bounded(25946, mean = 1635.5, sd = 640, min = 1088, max = 1453)
q5 <- rnorm_bounded(25946, mean = 1639, sd = 1280, min = 1453, max = 1825)

# followup time 
# drop wrong colums if needed
# drop <- c("q1", "q2", "q3", "q4", "q5")
# rdb = rdb[,!(names(rdb) %in% drop)]

rdb <- rdb %>%
  add_column(q1 = q1,
             q2 = q2,
             q3 = q3,
             q4 = q4,
             q5 = q5)

rdb <- rdb %>% 
  mutate(timenom = case_when(
    timecl == "1" ~ q1, 
    timecl == "2" ~ q2,
    timecl == "3" ~ q3,
    timecl == "4" ~ q4,
    timecl == "5" ~ q5
  ))
min(rdb$timenom)
max(rdb$timenom)
mean(rdb$timenom)
min(rdb$time)
max(rdb$time)
mean(rdb$time)

hist(rdb$timenom)

#-time if event 0 or 1----

str(rdb$EVENT)
str(rdb$timenom)
str(rdb$time)

rdb <- rdb %>%
  mutate(timefinal = case_when(
    EVENT == "1" ~ time,
    EVENT == "0" ~ timenom
  ))

mean(rdb$time)
mean(rdb$timefinal)
mean(rdb$timenom)

#-final dataset

rdb$time <- rdb$timenom # mean about 939.4704

rdb = subset(rdb, select = c(EVENT, time, cardiovasc, tabac2, dial, 
                             apkd01, bmic, sex, age, diabetes))

#-HAZREG package----

# install.packages("devtools")
require("devtools")

# install_github("FJRubio67/HazReg")
library("HazReg")

rdb$cardiovasc = as.numeric(as.character(rdb$cardiovasc))
rdb$dial = as.numeric(as.character(rdb$dial))
rdb$apkd01 = as.numeric(as.character(rdb$apkd01))
rdb$bmic = as.numeric(as.character(rdb$bmic))
rdb$diabetes = as.numeric(as.character(rdb$diabetes))

# design matrix for HAZARD LEVEL EFFECTS 
X <- as.matrix(cbind(scale(rdb$age), rdb$cardiovasc, rdb$tabac2, rdb$dial, rdb$apkd01,
                     rdb$bmic, rdb$sex, rdb$diabetes))

Xt <- as.matrix(cbind(scale(rdb$age)))

rdb$EVENT = as.integer(rdb$EVENT)
status <- as.vector(rdb$EVENT) 

times <- as.vector(rdb$time)/365.25

# histogram of survival times 
hist(times, probability = TRUE, breaks = 30)
box()

#-model fits and MLES----

# PGWGH
OPTPGWGH <- PGWMLE(init = rep(0, 3 + ncol(X) + ncol(Xt)), times = times, status = status, 
                   hstr = "PGWGH", des = X, des_t = Xt, method = "nlminb", maxit = 10000)

# PGWAFT
OPTPGWAFT <- PGWMLE(init = rep(0, 3 + ncol(X)), times = times, status = status, 
                    hstr = "PGWAFT", des = X, method = "nlminb", maxit = 10000)

# PGWPH
OPTPGWPH <- PGWMLE(init = rep(0, 3 + ncol(X)), times = times, status = status, 
                   hstr = "PGWPH", des = X, method = "nlminb", maxit = 10000)

# PGWAH
OPTPGWAH <- PGWMLE(init = rep(0, 3 + ncol(X)), times = times, status = status, 
                   hstr = "PGWAH", des_t = X, method = "nlminb", maxit = 10000)


# LLGH
OPTLLGH <- LLMLE(init = rep(0, 2 + ncol(X) + ncol(Xt)), times = times, status = status, 
                 hstr = "LLGH", des = X, des_t = Xt, method = "nlminb", maxit = 10000)

# LLAFT
OPTLLAFT <- LLMLE(init = rep(0, 2 + ncol(X)), times = times, status = status, 
                  hstr = "LLAFT", des = X, method = "nlminb", maxit = 10000)

# LLPH
OPTLLPH <- LLMLE(init = rep(0, 2 + ncol(X)), times = times, status = status, 
                 hstr = "LLPH", des = X, method = "nlminb", maxit = 10000)

# LLAH
OPTLLAH <- LLMLE(init = rep(0, 2 + ncol(X)), times = times, status = status, 
                 hstr = "LLAH", des_t = X, method = "nlminb", maxit = 10000)


# EWGH
OPTEWGH <- EWMLE(init = rep(0, 3 + ncol(X) + ncol(Xt)), times = times, status = status, 
                 hstr = "EWGH", des = X, des_t = Xt, method = "nlminb", maxit = 10000)

# GGGH
OPTGGGH <- GGMLE(init = rep(0, 3 + ncol(X) + ncol(Xt)), times = times, status = status, 
                 hstr = "GGGH", des = X, des_t = Xt, method = "nlminb", maxit = 10000)

# LNGH
OPTLNGH <- LNMLE(init = rep(0, 2 + ncol(X) + ncol(Xt)), times = times, status = status, 
                 hstr = "LNGH", des = X, des_t = Xt, method = "nlminb", maxit = 10000)

# GGH
OPTGGH <- GMLE(init = rep(0, 2 + ncol(X) + ncol(Xt)), times = times, status = status, 
               hstr = "GGH", des = X, des_t = Xt, method = "nlminb", maxit = 10000)

# MLEs in the original parameterisations
MLEPGWGH <- c(exp(OPTPGWGH$OPT$par[1:3]),OPTPGWGH$OPT$par[-c(1:3)])
MLEEWGH <- c(exp(OPTEWGH$OPT$par[1:3]),OPTEWGH$OPT$par[-c(1:3)])
MLEGGGH <- c(exp(OPTGGGH$OPT$par[1:3]),OPTGGGH$OPT$par[-c(1:3)])
MLEGGH <- c(exp(OPTGGH$OPT$par[1:2]),OPTGGH$OPT$par[-c(1:2)])
MLELNGH <- c(OPTLNGH$OPT$par[1], exp(OPTLNGH$OPT$par[2]), OPTLNGH$OPT$par[-c(1,2)])
MLELLGH <- c(OPTLLGH$OPT$par[1], exp(OPTLLGH$OPT$par[2]), OPTLLGH$OPT$par[-c(1,2)])

MLES <- cbind(MLEPGWGH, MLEEWGH, MLEGGGH, c(MLEGGH[1:2], NA, MLEGGH[-(1:2)]), 
              c(MLELNGH[1:2], NA, MLELNGH[-(1:2)]), c(MLELLGH[1:2], NA, MLELLGH[-(1:2)]))
colnames(MLES) <- c("PGWGH", "EWGH", "GGGH", "GGH", "LNGH", "LLGH")
rownames(MLES) <- c("theta[1]","theta[2]","theta[3]","age_t", "age","cardiovasc","tabac2","dial", "apkd01",
                    "bmic","sex", "diabetes")

# MLEs for GH models
# theta[1] in GGGH < 0.001, which is the reason why the rounded value appears as 0.000
kable(MLES, digits = 3)

# model comparaison----

# AIC for models with PGW baseline hazard
AICPGWGH <- 2*OPTPGWGH$OPT$objective + 2*length(OPTPGWGH$OPT$par)
AICPGWAFT <- 2*OPTPGWAFT$OPT$objective + 2*length(OPTPGWAFT$OPT$par)
AICPGWPH <- 2*OPTPGWPH$OPT$objective + 2*length(OPTPGWPH$OPT$par)
AICPGWAH <- 2*OPTPGWAH$OPT$objective + 2*length(OPTPGWAH$OPT$par)

# AICs for models with LL baseline hazard
AICLLGH <- 2*OPTLLGH$OPT$objective + 2*length(OPTLLGH$OPT$par)
AICLLAFT <- 2*OPTLLAFT$OPT$objective + 2*length(OPTLLAFT$OPT$par)
AICLLPH <- 2*OPTLLPH$OPT$objective + 2*length(OPTLLPH$OPT$par)
AICLLAH <- 2*OPTLLAH$OPT$objective + 2*length(OPTLLAH$OPT$par)

# AICs for GH models with GG, EW, LN, and G hazards
AICGGGH <- 2*OPTGGGH$OPT$objective + 2*length(OPTGGGH$OPT$par)
AICEWGH <- 2*OPTEWGH$OPT$objective + 2*length(OPTEWGH$OPT$par)
AICLNGH <- 2*OPTLNGH$OPT$objective + 2*length(OPTLNGH$OPT$par)
AICGGH <- 2*OPTGGH$OPT$objective + 2*length(OPTGGH$OPT$par)


# All AICs
AICs <- c(AICPGWGH, AICPGWAFT, AICPGWPH, AICPGWAH,
          AICLLGH, AICLLAFT, AICLLPH, AICLLAH,
          AICGGGH, AICEWGH, AICLNGH, AICGGH)

round(AICs, digits = 2)

# Best model:
which.min(AICs) # EWGH

# Baseline hazards for GH models----

# Fitted baseline hazard functions for GH models
PGWGHhaz <- Vectorize(function(t) hpgw(t, MLEPGWGH[1], MLEPGWGH[2], MLEPGWGH[3]) ) 
EWGHhaz <- Vectorize(function(t) hew(t, MLEEWGH[1], MLEEWGH[2], MLEEWGH[3]) ) 
GGGHhaz <- Vectorize(function(t) hggamma(t, MLEGGGH[1], MLEGGGH[2], MLEGGGH[3]) ) 
GGHhaz <- Vectorize(function(t) hgamma(t, MLEGGH[1], MLEGGH[2]) ) 
LNGHhaz <- Vectorize(function(t) hlnorm(t, MLELNGH[1], MLELNGH[2])) 
LLGHhaz <- Vectorize(function(t) hllogis(t, MLELLGH[1], MLELLGH[2])) 

# Note that the baseline hazards associated to the top models look similar
curve(PGWGHhaz,1e-6, max(times), xlab = "Time (years)", ylab = "Baseline Hazard", main = "",
      cex.axis = 1.5, cex.lab = 1.5, lwd = 2, ylim = c(0,0.03), n = 1000)
curve(EWGHhaz,1e-6, max(times), lwd = 2, n = 1000, lty = 2, add = TRUE) 
curve(GGGHhaz,1e-6, max(times), lwd = 2, n = 1000, lty = 3, add = TRUE) 
curve(GGHhaz,1e-6, max(times), lwd = 2, n = 1000, lty = 4, add = TRUE) 
curve(LNGHhaz,1e-6, max(times), lwd = 2, n = 1000, lty = 5, add = TRUE) 
curve(LLGHhaz,1e-6, max(times), lwd = 2, n = 1000, lty = 6, add = TRUE) 
legend("topright", legend = c("PGWGH", "EWGH", "GGGH", "GGH", "LNGH", "LLGH"), lwd = rep(2,6), lty = 1:6)

# best model summary

# MLE in the original parameterisation
MLE <- c(OPTLLGH$OPT$par[1], exp(OPTLLGH$OPT$par[2]), OPTLLGH$OPT$par[-c(1,2)])
round(MLE, digits = 3)

# 95% Confidence intervals under the reparameterisation
require("numDeriv")
CI <- Conf_Int(FUN = OPTLLGH$log_lik, MLE = OPTLLGH$OPT$par, level = 0.95)
rownames(CI) <- c("theta[1]","theta[2]","theta[3]","age","cardiovasc",
                  "tabac2","dial", "apkd01",
                  "bmic","sex", "diabetes")

kable(CI, digits = 3)

# Fitted baseline hazard function
fit_haz <- Vectorize(function(t) hllogis(t, MLE[1], MLE[2])) 

curve(fit_haz,0.001, max(times), xlab = "Time (years)", ylab = "Baseline Hazard", main = "",
      cex.axis = 0.9, cex.lab = 1.3, lwd = 2, ylim = c(0,0.01), xlim = c(0,5), n = 1000)

# Average population survival function and KM estimator
require("survival")

pop_surv <- Vectorize(function(t){
  p0 <- dim(Xt)[2]
  p1 <- dim(X)[2]
  theta1 <- MLE[1]; theta2 <- MLE[2]; alpha <- MLE[3:(2+p0)]; beta <- MLE[(3+p0):(2+p0+p1)]
  x.alpha <- Xt%*%alpha
  x.dif <- X%*%beta - x.alpha
  out <- mean( exp( - chllogis(t*exp(x.alpha), theta1, theta2)*exp(x.dif)  )  )
  return(out)
})


# Kaplan-Meier estimator 
km <- survfit(Surv(times, status) ~ 1)

# Comparison
plot(km$time, km$surv, type = "l", col = "black", lwd = 2, lty = 1, ylim = c(0,1),
     xlab = "Time (years)", ylab = "Population Survival", main = "",
     cex.axis = 1.5, cex.lab = 1.5)
curve(pop_surv,0.001,14, lwd = 2, n = 1000, add = TRUE, lty = 2, col = "gray")
legend("bottomright", legend = c("KM","Parametric"), col = c("black","gray"), lwd = c(2,2), lty = c(1,2))

# Confidence intervals for the survival function based on a normal approximation
# at specific time points t0

#install.packages("SimDesign")
require("SimDesign")

HESS <- hessian(func = OPTLLGH$log_lik, x = OPTLLGH$OPT$par)

# Hessian and asymptotic covariance matrix
Sigma <- solve(HESS)

# Reparameterised MLE 
r.MLE <- OPTLLGH$OPT$par

# The function to obtain approximate CIs based on Monte Carlo simulations 
# from the asymptotic normal distribution of the MLEs
# t0 : time where the confidence interval will be calculated
# level : confidence level
# n.mc : number of Monte Carlo iterations

conf.int.surv <- function(t0, level, n.mc){
  p0 <- dim(Xt)[2]
  p1 <- dim(X)[2]
  mc <- vector()
  S.par <- function(par){ mean( exp( - chllogis(t0*exp(Xt%*%par[3:(2+p0)]), par[1], par[2])*
                                       exp(X%*%par[(3+p0):(2+p0+p1)]-Xt%*%par[3:(2+p0)])  )  )
  }
  
  for(i in 1:n.mc) {
    val <- rmvnorm(1,mean = r.MLE, sigma = Sigma)
    val[2] <- exp(val[2])
    mc[i] <- S.par(val)
  }
  
  L <- quantile(mc,(1-level)*0.5)
  U <- quantile(mc,(1+level)*0.5)
  
  M <- S.par(MLE)
  
  return(c(L,M,U))
}


# times for CIs calculations
timesCI <- c(1,2,3,4,5)

CIS <- matrix(0, ncol = 4, nrow = length(timesCI))

for(k in 1:length(timesCI)) CIS[k,] <- c(timesCI[k],conf.int.surv(timesCI[k],0.95,10000))

colnames(CIS) <- cbind("year","lower","population survival","upper")
print(kable(CIS,digits=4))
