getwd()
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
names(switch)

# CORRESPONDENCE DATABASE (ACCROCHAGE)------------------------------------------
rein_s <- read.csv2("snds_rein.csv", header = TRUE, na.string="")
dim(rein_s)

# MERGING "ACCROCHAGE" FOR SNDS AND REIN DATA-----------------------------------
rein_m4 <- merge(rein, rein_s, by.x = "RREC_COD_ANO", by.y = "RREC_COD_ANO")
dim(rein_m4)
names(rein_m4)

# MERGING "HOSP"----------------------------------------------------------------
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

# DATA--------------------------------------------------------------------------
rdb <- rein_mone
names(rdb)

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
rdb$FUP.19 = as.Date(rdb$december, "%d/%m/%Y") - as.Date(rdb$DDIRT, "%d/%m/%Y") 

# TIL DEATH
rdb$FUP.D = as.Date(rdb$DDC, "%d/%m/%Y") - as.Date(rdb$DDIRT, "%d/%m/%Y") 
mean(rdb$FUP.D, na.rm = T)
#rdb$FUP.D2 = rdb$DDC.asd - rdb$DDIRT.asd
#mean(rdb$FUP.D2, na.rm = T)

# TIL EVENT
rdb$FUP.E = as.Date(rdb$evdate, "%d/%m/%Y") - as.Date(rdb$DDIRT, "%d/%m/%Y")
mean(rdb$FUP.E, na.rm = T)
#rdb$FUP.E2 = rdb$evdate.asd - rdb$DDIRT.asd
#mean(rdb$FUP.E2, na.rm = T)

# TIL TRANSPLANTATION
rdb$FUP.T = as.Date(rdb$DGRF, "%d/%m/%Y") - as.Date(rdb$DDIRT, "%d/%m/%Y")

# DATE VARIABLES AS NUMERIC 

#rdb$DGRF.d = as.numeric(as.Date(rdb$DGRF, "%d/%m/%Y"))
rdb$DDC.d = as.numeric(as.Date(rdb$DDC, "%d/%m/%Y"))
rdb$evdate.d = as.numeric(as.Date(rdb$evdate, "%d/%m/%Y"))

# DATE VARIABLES AS DATE VARIABLES 

rdb$DGRF.asd = as.Date(rdb$DGRF, "%d/%m/%Y")
rdb$DDC.asd = as.Date(rdb$DDC, "%d/%m/%Y")
rdb$evdate.asd = as.Date(rdb$evdate, "%d/%m/%Y")
rdb$DDIRT.asd = as.Date(rdb$DDIRT, "%d/%m/%Y")

#-FUP---------------------------------------------------------------------------

rdb <- rdb %>%
  mutate(epilogus = case_when(
    DEATH == "1" ~ FUP.D, 
    TRANSP == "1" ~ FUP.T, 
    DEATH == "0" ~ FUP.19, 
    TRANSP == "0" & DEATH == "1" ~ FUP.D,
    TRANSP == "1" & DEATH == "1" ~ FUP.D,
  ))
#-REDIFINING THE EVENT ACCORDING TO THE FUP-------------------------------------

rdb <- rdb %>%
  mutate(EVENT_T = case_when(
    EVENT == "1" & (FUP.T < FUP.E) ~ "0",
    EVENT == "1" & (FUP.T > FUP.E) ~ "1",
    EVENT == "0" & (FUP.T < FUP.E) ~ "0",
    EVENT == "0" & (FUP.T < FUP.E) ~ "0"
  ))

rdb$EVENT_T[is.na(rdb$EVENT_T)] <- "0"
table(rdb$EVENT_T, useNA = "always")

table(rdb$EVENT, useNA = "always")
#0     1 
#39375  1605 
table(rdb$EVENT, rdb$EVENT_T, useNA = "always")
#          0     1  <NA>
#0     39375     0     0
#1      1567    38     0
#<NA>      0     0     0

rdb <- rdb %>%
  mutate(EVENT = case_when(
    EVENT == "1" & EVENT_T == "1" ~ "0",
    EVENT == "1" & EVENT_T == "0" ~ "1",
    EVENT == "0" & EVENT_T == "1" ~ "0",
    EVENT == "0" & EVENT_T == "0" ~ "0"
  ))

table(rdb$EVENT, useNA = "always")

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

table(rdb$dial, useNA = "always")
# lines columns
table(rdb$dial, rdb$TRANSP)

#-APKD---------------------------------------------------------------------------

rdb$apkd01 = if_else(rdb$nephgp == "APKD", "1", "0")
table(rdb$apkd01)
#     0     1 
# 42466  2560 

table(rdb$apkd01, rdb$EVENT, useNA = "always")

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

rdb$status = if_else(rdb$EVENT == "1", "1", "0")
table(rdb$status)
rdb$status = as.factor(rdb$status)

#-FINAL DATASET---------------------------------------------------------------

table(rdb$METHOn)
dim(rdb) #45026   106
rdb <- rdb[!(rdb$METHOn=="3"),] # 43241   106
dim(rdb)
#table(rdb$status)
#0     1 
#41692  1549 

#-NUM_ENQ AND RREC_COD_ANO---------------------------------------------------

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
dim(rdb) # 40980

#-ONLY ADPKD POPULATION------------------------------------------------------

adpkd = rdb[!(rdb$apkd01 == "0"),]
dim(adpkd)
# 2182  111

#-DEATH AND CVA EVENT--------------------------------------------------------

table(adpkd$EVENT)
#   0   1 
#2125   53

table(adpkd$DEATH)
#0     1 
#1961  221 

adpkd <- adpkd %>% 
  mutate(cva_death = case_when(
    EVENT == "1" & DEATH == "1" ~ "DEATH_POST",
    EVENT == "0" & DEATH == "1" ~ "NODEATH_POST",
    EVENT == "1" & DEATH == "0" ~ "NODEATH_POST",
    EVENT == "0" & DEATH == "0" ~ "NODEATH_POST"
  ))
table(adpkd$cva_death)

# DEATH_POST NODEATH_POST 
# 18         238 

# variable cva_death with non-dead ADPKD+ patients

adpkd <- adpkd %>% 
  mutate(cva_death3 = case_when(
    EVENT == "1" & DEATH == "1" ~ "postavc_death",
    EVENT == "0" & DEATH == "1" ~ "noavec_death",
    EVENT == "1" & DEATH == "0" ~ "avc_nodeath",
    EVENT =="0" & DEATH == "0" ~ "noavc_nodeath",
  ))
table(adpkd$cva_death3, useNA = "always")

table(adpkd$cva_death3, adpkd$EVENT, useNA = "always")

#                   0    1 <NA>
#  avc_nodeath      0   35    0
#  noavc_nodeath 1926    0    0
#  noavec_death   203    0    0
#  postavc_death    0   18    0
#  <NA>             0    0    0

# variable décès binaire 

adpkd <- adpkd %>%
  mutate(cva_death2 = case_when(
    EVENT == "1" & DEATH == "1" ~ "deces",
    EVENT == "1" & DEATH == "0" ~ "pasdeces"
  ))
table(adpkd$cva_death2)


#-POST CVA DEATH BUT DRUG ADMINISTRATION IN THE FIRST 3 MONTHS---------------

require(lubridate)

str(adpkd$delivrance)

adpkd$JJ15 <- rep("31", times=2182)
adpkd$MM15 <- rep("03", times=2182)
adpkd$YY15 <- rep("2015", times=2182)

adpkd$threemonths<- as.numeric(paste(adpkd$YY15, adpkd$MM15, adpkd$JJ15, sep = ""))
table(adpkd$threemonths)
adpkd$threemonths = ymd(adpkd$threemonths)
str(adpkd$threemonths)

# delivrance sans données manquantes
adpkd$delivranceNA[is.na(adpkd$delivrance)] <- "missing"

adpkd <- adpkd %>%
  mutate(threemex = case_when(
    delivrance < threemonths ~ "toexclude",
    delivrance > threemonths ~ "notexclude",
    delivranceNA == "missing" ~ "notexclude" 
  ))
table(adpkd$threemex, useNA = "always")
# notexclude  toexclude 
# 2121         61 

table(adpkd$threemex, adpkd$EVENT)
#              0    1
#notexclude 2068   53
#toexclude    57    4

table(adpkd$threemex, adpkd$cva_death)
#           DEATH_POST NODEATH_POST
#notexclude         17          222
#toexclude           2           18

#-variable ttt (L_ATC4) after exclusion of patients having 

table(adpkd$L_ATC4, useNA = "always")

# recoding of treatment variable 

adpkd <- adpkd %>%
  mutate(L_ATC4.g = case_when(
    L_ATC4 == "ACIDE SALICYLIQUE ET DERIVES" ~ "plaq",
    L_ATC4 == "ANTIFIBRINOLYTIQUES" ~ "coag",
    L_ATC4 == "ANTIVITAMINES K" ~ "coag",
    L_ATC4 == "AUTRES ANTITHROMBOTIQUES" ~ "coag",
    L_ATC4 == "AUTRES HEMOSTATIQUES SYSTEMIQUES" ~ "coag",
    #L_ATC4 == "AUTRES PREPARATIONS ANTIANEMIQUES" ~ "coag",
    L_ATC4 == "GROUPE DE L'HEPARINE" ~ "coag",
    L_ATC4 == "INHIBITEURES DIRECTS DU FACTEUR Xa" ~ "coag",
    L_ATC4 == "INHIBITEURS DE L'AGREGATION PLAQUETTAIRE, HEPARINE EXCLUE" ~ "plaq",
    L_ATC4 == "VITAMINE K" ~ "coag"
  )) 
table(adpkd$L_ATC4.g, useNA = "always")
tab<-table(adpkd$L_ATC4.g, adpkd$cva_death)
(prop.table(tab, margin = 2))*100

#tab2 <- table(adpkd$L_ATC4.g, adpkd$cva_death2)
#(prop.table(tab2, margin = 2))*100

tab3 <- table(adpkd$L_ATC4.g, adpkd$cva_death3, useNA = "always")
(prop.table(tab3, margin = 2))*100

#ACIDE SALICYLIQUE ET DERIVES 
#38 
#ANTIFIBRINOLYTIQUES 
#10 
#ANTIVITAMINES K 
#226 
#AUTRES ANTITHROMBOTIQUES 
#4 
#AUTRES HEMOSTATIQUES SYSTEMIQUES 
#1 
#AUTRES PREPARATIONS ANTIANEMIQUES 
#500 
#GROUPE DE L'HEPARINE 
#289 
#INHIBITEURES DIRECTS DU FACTEUR Xa 
#2 
#INHIBITEURS DE L'AGREGATION PLAQUETTAIRE, HEPARINE EXCLUE 
#479 
#VITAMINE K 
#1 
#<NA> 
#  632 

adpkd <- adpkd %>%
  mutate(L_ATC4.2 = case_when(
    L_ATC4 == "ACIDE SALICYLIQUE ET DERIVES" & threemex == "notexclude" ~ "ACIDE SALICYLIQUE ET DERIVES",
    L_ATC4 == "ANTIFIBRINOLYTIQUES" & threemex == "notexclude" ~ "ANTIFIBRINOLYTIQUES",
    L_ATC4 == "ANTIVITAMINES K" & threemex == "notexclude" ~ "ANTIVITAMINES K",
    L_ATC4 == "AUTRES ANTITHROMBOTIQUES" & threemex == "notexclude" ~ "AUTRES ANTITHROMBOTIQUES",
    L_ATC4 == "AUTRES HEMOSTATIQUES SYSTEMIQUES" & threemex == "notexclude" ~ "AUTRES HEMOSTATIQUES SYSTEMIQUES",
    L_ATC4 == "AUTRES PREPARATIONS ANTIANEMIQUES" & threemex == "notexclude" ~ "AUTRES PREPARATIONS ANTIANEMIQUES",
    L_ATC4 == "GROUPE DE L'HEPARINE" & threemex == "notexclude" ~ "GROUPE DE L'HEPARINE",
    L_ATC4 == "INHIBITEURES DIRECTS DU FACTEUR Xa" & threemex == "notexclude" ~ "INHIBITEURES DIRECTS DU FACTEUR Xa",
    L_ATC4 == "INHIBITEURS DE L'AGREGATION PLAQUETTAIRE, HEPARINE EXCLUE" & threemex == "notexclude" ~ "INHIBITEURS DE L'AGREGATION PLAQUETTAIRE, HEPARINE EXCLUE",
    L_ATC4 == "VITAMINE K" & threemex == "notexclude" ~ "VITAMINE K"
  ))

table(adpkd$L_ATC4.2, useNA = "always")
table(adpkd$L_ATC4, useNA = "always")

adpkd <- adpkd %>%
  mutate(L_ATC4c = case_when(
    L_ATC4.2 == "ACIDE SALICYLIQUE ET DERIVES" ~ "antiplaq",
    L_ATC4.2 == "ANTIFIBRINOLYTIQUES" ~ "anticoag",
    L_ATC4.2 == "ANTIVITAMINES K" ~ "anticoag", 
    L_ATC4.2 == "AUTRES ANTITHROMBOTIQUES" ~ "anticoag",
    L_ATC4.2 == "AUTRES HEMOSTATIQUES SYSTEMIQUES" ~ "anticoag",
    L_ATC4.2 == "GROUPE DE L'HEPARINE" ~ "anticoag",
    L_ATC4.2 == "INHIBITEURES DIRECTS DU FACTEUR Xa" ~ "anticoag",
    L_ATC4.2 == "INHIBITEURS DE L'AGREGATION PLAQUETTAIRE, HEPARINE EXCLUE" ~ "antiplaq"
  ))

#-TRAITEMENT EN DEUX MODALITES (DP et HD)

adpkd <- adpkd %>%
  mutate(ttt2 = case_when(
    traitement == 1 ~ "HD",
    traitement == 2 ~ "HD",
    traitement == 3 ~ "HD",
    traitement == 5 ~ "HD",
    traitement == 6 ~ "DP",
    traitement == 7 ~ "DP",
    traitement == 8 ~ "DP"
  ))

table(adpkd$ttt2)
table(adpkd$ttt2, adpkd$cva_death3)
prop.table(table(adpkd$ttt2, adpkd$cva_death3), margin = 2)*100

#-ALL CVA--------------------------------------------------------------------
#-ANALYSE DESCRIPTIVE DE LA POPULATION AVC PLUS------------------------------

table(adpkd$EVENT, useNA = "always")
#   0    1 <NA> 
#2129   53    0 

table(adpkd$DGN_PAL, useNA = "always")

adpkd.e <- adpkd[!(adpkd$DGN_PAL=="E0"),]
dim(adpkd.e) # 53 124

#-TABLEONE-------------------------------------------------------------------
require(tableone)

variables.s <- c("ttt2","tabac2", "sex", "age", 
                 "L_ATC4.2", "DGN_PAL", "atcd", "DEATH", "EVENT", "groupes6",
                 "TRANSP", "cardiovasc", "L_ATC4.g", 
                 "apkd01", "diabetes", "bmic", "time", "status", "cva_death2")

catvariables.s <- c("ttt2","tabac2", "sex", 
                    "L_ATC4.2", "DGN_PAL", "atcd", "DEATH", "EVENT", "groupes6", 
                    "TRANSP", "cardiovasc", "L_ATC4.g", 
                    "apkd01", "diabetes", "bmic", "status", "cva_death2")


adpkd.des <- CreateTableOne(vars = variables.s, 
                            factorVars = catvariables.s,
                            data = adpkd.e,
                            includeNA = F)
print(adpkd.des, showAllLevels = TRUE, quote = F, nospaces = TRUE)

adpkd.death = CreateTableOne(vars = variables.s, data = adpkd, factorVars = catvariables.s, 
                             test = F, includeNA = F, strata = "cva_death2")
print(adpkd.death, showAllLevels = TRUE, quote = F, noSpaces = TRUE)

#-VARIABLE RECODING AND DESCRITPION------------------------------------------

table(adpkd.e$bmic)

adpkd.e <- adpkd.e %>% 
  mutate(bmic2 = case_when(
    bmic == "1" | bmic == "2" ~ "1",
    bmic == "3" ~ "2",
    bmic == "4" ~ "3"
  ))
table(adpkd.e$bmic2)
(prop.table(table(adpkd.e$bmic2)))*100

table(adpkd.e$bmic2, adpkd.e$cva_death2)
(prop.table(table(adpkd.e$bmic2, adpkd.e$cva_death2)))*100

#-COX MODEL------------------------------------------

require("survival")
require("survminer")

str(adpkd.e$time)
adpkd.e$status = as.numeric(as.character(if_else(adpkd.e$cva_death2 == "pasdeces", "0", "1")))

# tt2
res.cox1 <- coxph(Surv(time, status) ~ ttt2, data = adpkd.e)
test1 <- cox.zph(res.cox1)
#      chisq df    p
# ttt2    2.78  1 0.095
# GLOBAL  2.78  1 0.095
# proportional hazard 
ggcoxzph(test1)
summary(res.cox1)

#coef exp(coef) se(coef)     z Pr(>|z|)
#ttt2HD 0.6187    1.8565   0.7514 0.823     0.41
#
#exp(coef) exp(-coef) lower .95 upper .95
#ttt2HD     1.857     0.5386    0.4257     8.097

# tabac2
res.cox2 <- coxph(Surv(time, status) ~ tabac2, data = adpkd.e)
test2 <- cox.zph(res.cox2)
#      chisq df    p
#tabac2  2.28  1 0.13
#GLOBAL  2.28  1 0.13
summary(res.cox2)

# sex
res.cox3 <- coxph(Surv(time, status) ~ sex, data = adpkd.e)
test3 <- cox.zph(res.cox3)
#       chisq df    p
#sex     2.63  1 0.11
#GLOBAL  2.63  1 0.11
summary(res.cox3)

# age
res.cox4 <- coxph(Surv(time, status) ~ age, data = adpkd.e)
test4 <- cox.zph(res.cox4)
#       chisq df    p
#age    0.612  1 0.43
#GLOBAL 0.612  1 0.43
ggcoxfunctional(Surv(time, status) ~ age + log(age) + sqrt(age), data = adpkd.e)
# nonlinearity
summary(res.cox4)

# DGN_PAL

adpkd.e <- adpkd.e %>%
  mutate(diagnostic = case_when(
    DGN_PAL == "G45" ~ "1",
    DGN_PAL == "I60" ~ "2",
    DGN_PAL == "I61" ~ "3", 
    DGN_PAL == "I63" ~ "4",
    DGN_PAL == "I64" ~ "5"
  ))
table(adpkd.e$diagnostic)
adpkd.e$diagnostic = as.factor(adpkd.e$diagnostic)

res.cox5 <- coxph(Surv(time, as.numeric(status)) ~ diagnostic, data = adpkd.e)
test5 <- cox.zph(res.cox5)
#           chisq df    p
#diagnostic  3.14  4 0.53
#GLOBAL      3.14  4 0.53
summary(res.cox5)

require(rms)
surv5 <- cph(Surv(as.numeric(time), as.numeric(status)) ~ diagnostic, data = adpkd.e, x = T, y = T)
exp(coef(surv5))
exp(confint(surv5))
se <- predict(surv5, se.fit=TRUE)$se.fit 

# atcd

res.cox6 <- coxph(Surv(time, status) ~ atcd, data = adpkd.e)
test6 <- cox.zph(res.cox6)
#       chisq df    p
#atcd   0.602  1 0.44
#GLOBAL 0.602  1 0.44
summary(res.cox6)

# TRANSP

res.cox7 <- coxph(Surv(time, status) ~ TRANSP, data = adpkd.e)
test7 <- cox.zph(res.cox7)
#       chisq df      p
#TRANSP  7.31  1 0.0068
#GLOBAL  7.31  1 0.0068
summary(res.cox7)

# cardiovasc
res.cox8 <- coxph(Surv(time, status) ~ cardiovasc, data = adpkd.e)
test8 <- cox.zph(res.cox8)
#           chisq df   p
#cardiovasc  1.09  1 0.3
#GLOBAL      1.09  1 0.3
summary(res.cox8)

# L_ATC4.g
res.cox9 <- coxph(Surv(time, status) ~ L_ATC4.g, data = adpkd.e)
test9 <- cox.zph(res.cox9)
#         chisq df    p
#L_ATC4.g  1.29  1 0.26
#GLOBAL    1.29  1 0.26
summary(res.cox9)

# diabetes 
res.cox10 <- coxph(Surv(time, status) ~ diabetes, data = adpkd.e)
test9 <- cox.zph(res.cox10)
#         chisq df   p
#diabetes 0.278  1 0.6
#GLOBAL   0.278  1 0.6
summary(res.cox10)

# bmic
res.cox11 <- coxph(Surv(time, status) ~ bmic, data = adpkd.e)
test11 <- cox.zph(res.cox11)
#       chisq df    p
#bmic     3.3  3 0.35
#GLOBAL   3.3  3 0.35
summary(res.cox11)

res.cox112 <- coxph(Surv(time, status) ~ bmic2, data = adpkd.e)
test112 <- cox.zph(res.cox112)
#       chisq df    p
#bmic2  0.607  2 0.74
#GLOBAL 0.607  2 0.74
summary(res.cox112)

#-KM FOR DEATH------------------------------------------------

??ggsurvplot
require(survminer)
require(survival)

#km_fit <- survfit(Surv(time, status) ~ 1, data = adpkd.e)
#
#km_fit %>%
#  ggsurvplot(
#    data = adpkd.e,
#    fun = "pct",
#    risk.table = FALSE,
#    fontsize = 3,
#    surv.median.line = "hv",
#    ggtheme = theme_light(),
#    #palette = 
#    title = "KM",
#    surv.geom = smooth(),
#    legend.title = ""#,
#    #legend.labs = "levels"
#  )
#
#ggplot(adpkd.e, aes(time, status)) + geom_point() +
#  geom_smooth(method = "gam", formula = y ~ poly(x, 2),se=F
#              )

#-TEMPS ENTRE AVC ET DECES------ 

# delai entre événement et décès
adpkd.e$DED = adpkd.e$DDC.asd - adpkd.e$evdate.asd
mean(abs(adpkd.e$DED), na.rm = T)

table(adpkd.e$DDC.asd)
# 2016-01-11
require(lubridate)
a<-ymd(adpkd.e$DDC.asd)
adpkd.e$year <- year(a)
adpkd.e$month <- day(a)
adpkd.e$day <- month(a)

str(adpkd.e$month)

adpkd.e$month = as.numeric(adpkd.e$month)
`
adpkd.e <- adpkd.e %>%
  mutate(month2 = case_when(
    month == 1 ~ "01",
    month == 2 ~ "02",
    month == 3 ~ "03",
    month == 4 ~ "04",
    month == 5 ~ "05",
    month == 6 ~ "06",
    month == 7 ~ "07",
    month == 8 ~ "08",
    month == 9 ~ "09",
    month == 10 ~ "10",
    month == 11 ~ "11",
    month == 12 ~ "12"
  ))

adpkd.e <- adpkd.e %>% 
  mutate(day2 = case_when(
    day == 1 ~ "01"
  ))


adpkd.e$deathdate <- as.numeric(paste(adpkd.e$year, adpkd.e$month2, adpkd.e$day2, sep = ""))
str(adpkd.e$deathdate)
adpkd.e$deathdate2 = ymd(adpkd.e$deathdate)

# delai deces evenement 

adpkd.e$DED2 = adpkd.e$deathdate2 - adpkd.e$evdate.asd
str(adpkd.e$DED2)
mean(adpkd.e$DED2, na.rm = T)
adpkd.e$DED2n = as.numeric(adpkd.e$DED2)
summary(adpkd.e$DED2n, na.rm = T)
sd(adpkd.e$DED2n, na.rm = T)

# delai fin etude evenement 

adpkd.e$DEFE = adpkd.e$december - adpkd.e$evdate.asd
str(adpkd.e$DEFE)
adpkd.e$DEFEn = as.numeric(adpkd.e$DEFE)
summary(adpkd.e$DEFEn)
sd(adpkd.e$DEFEn, na.rm = T)

DEFE <- adpkd.e$DEFE
DED2 <- adpkd.e$DED2
adpkd.e$status01 = as.factor(adpkd.e$status)

DEFEf = if_else(adpkd.e$status == 0, DEFE, DED2)

km_fit <- survfit(Surv(as.numeric(DEFEf), status) ~ 1, data = adpkd.e)
plot(km_fit)


#-ALL DEATH------------------------------------------------------------------
#-ANALYSE DESCRIPTIVE DE LA POPULATION DECEDEE-------------------------------

table(adpkd$DEATH, useNA = "always")
#   0    1 
#1961  221 
adpkd.d <- adpkd[!(adpkd$DEATH=="0"),]
dim(adpkd.d)
# [1] 221 124

#-DESCRIPTIF DE LA POPULATON-------------------------------------------------

#adpkd.e <- adpkd[!(adpkd$EVENT=="0"),]
#dim(adpkd.e) # 53 124

table(adpkd.e$bmic)

adpkd.d <- adpkd.d %>% 
  mutate(bmic2 = case_when(
    bmic == "1" | bmic == "2" ~ "1",
    bmic == "3" ~ "2",
    bmic == "4" ~ "3"
  ))
table(adpkd.d$bmic2)
(prop.table(table(adpkd.d$bmic2)))*100

require(tableone)

variables.s <- c("ttt2","tabac2", "sex", "age", 
                #"L_ATC4.2", 
                "DGN_PAL", "atcd", "EVENT", "groupes6",
                 "TRANSP", "cardiovasc", "L_ATC4.g", 
                 "apkd01", "diabetes", "bmic2", "time", "status", "DEATH")

catvariables.s <- c("ttt2","tabac2", "sex", 
                    #"L_ATC4.2", 
                    "DGN_PAL", "atcd", "EVENT", "groupes6", 
                    "TRANSP", "cardiovasc", "L_ATC4.g", 
                    "apkd01", "diabetes", "bmic2", "status", "DEATH")


adpkd.des <- CreateTableOne(vars = variables.s, 
                            factorVars = catvariables.s,
                            data = adpkd.d,
                            includeNA = F)
print(adpkd.des, showAllLevels = TRUE, quote = F, nospaces = TRUE)



adpkd.avc = CreateTableOne(vars = variables.s, data = adpkd.d, factorVars = catvariables.s, 
                             test = F, includeNA = F, strata = "status")
print(adpkd.avc, showAllLevels = TRUE, quote = F, noSpaces = TRUE)

#-TESTS------------------------------------------

cs <- function(x,y){
  result = (chisq.test(x, adpkd.d$status, simulate.p.value = TRUE, B = 10000))
  return(result)
}

# tt2
chisq.test(adpkd.d$status, adpkd.d$ttt2, simulate.p.value = T, B = 10000)
cs(adpkd.d$ttt2)

# tabac2
cs(adpkd.d$tabac2)

# sex
cs(adpkd.d$sex)

# age
adpkd.d$statusN = as.numeric(as.character(adpkd.d$status))
t.test(adpkd.d$age, adpkd.d$statusN, alternative = "two.sided", paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)

# DGN_PAL

adpkd.d <- adpkd.d %>%
  mutate(diagnostic = case_when(
    DGN_PAL == "E0" ~ "0",
    DGN_PAL == "G45" ~ "1",
    DGN_PAL == "I60" ~ "2",
    DGN_PAL == "I61" ~ "3",
    DGN_PAL == "I63" ~ "4"
  ))
str(adpkd.d$diagnostic)
adpkd.d$diagnostic = as.factor(adpkd.d$diagnostic)

cs(adpkd.d$diagnostic)

require(rms)
surv5 <- cph(Surv(as.numeric(time), as.numeric(status)) ~ diagnostic, data = adpkd.d, x = T, y = T)
exp(coef(surv5))
exp(confint(surv5))
se <- predict(surv5, se.fit=TRUE)$se.fit 

# atcd

cs(adpkd.d$atcd)

# cause de deces groupe6

table(adpkd.d$groupes6)

adpkd.d <- adpkd.d %>%
  mutate(groupes6c = case_when(
    groupes6 == " " ~ "no",
    groupes6 == "I" ~ "I",
    groupes6 == "III" ~ "III",
    groupes6 == "V" ~ "V",
    groupes6 == "VI" ~ "VI"
  ))

cs(adpkd.d$groupes6c)

# TRANSP

cs(adpkd.d$TRANSP)

# cardiovasc
cs(adpkd.d$cardiovasc)

# L_ATC4.g
cs(adpkd.d$L_ATC4.g)

# diabetes 
cs(adpkd.d$diabetes)

# bmic
cs(adpkd.d$bmic2)

#-ALL NO CVA-----------------------------------------------------------------
adpkd.ne <- adpkd[!(adpkd$EVENT=="1"),]
dim(adpkd.ne) # 2129  124
dim(adpkd)
table(adpkd.ne$DEATH)
#0    1 
#1926  203 


adpkd.ne <- adpkd.ne %>% 
  mutate(bmic2 = case_when(
    bmic == "1" | bmic == "2" ~ "1",
    bmic == "3" ~ "2",
    bmic == "4" ~ "3"
  ))

adpkd.ne <- adpkd.ne %>%
  mutate(diagnostic = case_when(
    DGN_PAL == "G45" ~ "1",
    DGN_PAL == "I60" ~ "2",
    DGN_PAL == "I61" ~ "3", 
    DGN_PAL == "I63" ~ "4",
    DGN_PAL == "I64" ~ "5"
  ))
table(adpkd.ne$diagnostic, useNA = "always")

table(adpkd.ne$DGN_PAL)

adpkd.ne2 <- adpkd.ne[(adpkd.ne$DGN_PAL=="E0"),]
dim(adpkd.ne2)
# 2125  126

#-TABLEONE-------------------------------------------------------------------

require(tableone)

variables.s <- c("ttt2","tabac2", "sex", "age", 
                 "L_ATC4.2", "DGN_PAL", "atcd", "DEATH", "EVENT", "groupes6",
                 "TRANSP", "cardiovasc", "L_ATC4.g", 
                 "apkd01", "diabetes", "bmic2", "time", "status")

catvariables.s <- c("ttt2","tabac2", "sex", 
                    "L_ATC4.2", "DGN_PAL", "atcd", "DEATH", "EVENT", "groupes6", 
                    "TRANSP", "cardiovasc", "L_ATC4.g", 
                    "apkd01", "diabetes", "bmic2", "status")

adpkd.nocva <- CreateTableOne(vars = variables.s, 
                            factorVars = catvariables.s,
                            data = adpkd.ne,
                            includeNA = F)

print(adpkd.nocva, showAllLevels = TRUE, quote = F, nospaces = TRUE)

adpkdne.death = CreateTableOne(vars = variables.s, data = adpkd.ne, factorVars = catvariables.s, 
                             test = F, includeNA = F, strata = "DEATH")
print(adpkdne.death, showAllLevels = TRUE, quote = F, noSpaces = TRUE)

#-date de décès

table(adpkd.ne$DDC.asd)
# 2016-01-11
require(lubridate)
a<-ymd(adpkd.ne$DDC.asd)
adpkd.ne$year <- year(a)
adpkd.ne$month <- day(a)
adpkd.ne$day <- month(a)

str(adpkd.ne$month)

adpkd.ne$month = as.numeric(adpkd.ne$month)

adpkd.ne <- adpkd.ne %>%
  mutate(month2 = case_when(
    month == 1 ~ "01",
    month == 2 ~ "02",
    month == 3 ~ "03",
    month == 4 ~ "04",
    month == 5 ~ "05",
    month == 6 ~ "06",
    month == 7 ~ "07",
    month == 8 ~ "08",
    month == 9 ~ "09",
    month == 10 ~ "10",
    month == 11 ~ "11",
    month == 12 ~ "12"
  ))

adpkd.ne <- adpkd.ne %>% 
  mutate(day2 = case_when(
    day == 1 ~ "01"
  ))

adpkd.ne$deathdate <- as.numeric(paste(adpkd.ne$year, adpkd.ne$month2, adpkd.ne$day2, sep = ""))
str(adpkd.ne$deathdate)
adpkd.ne$deathdate2 = ymd(adpkd.ne$deathdate)

# for ddirt

b <- ymd(adpkd.ne$DDIRT.asd)
adpkd.ne$year3 <- year(b)
adpkd.ne$month3 <- day(b)
adpkd.ne$day3 <- month(b)

adpkd.ne$month3 = as.numeric(adpkd.ne$month3)

adpkd.ne <- adpkd.ne %>%
  mutate(month4 = case_when(
    month3 == 1 ~ "01",
    month3 == 2 ~ "02",
    month3 == 3 ~ "03",
    month3 == 4 ~ "04",
    month3 == 5 ~ "05",
    month3 == 6 ~ "06",
    month3 == 7 ~ "07",
    month3 == 8 ~ "08",
    month3 == 9 ~ "09",
    month3 == 10 ~ "10",
    month3 == 11 ~ "11",
    month3 == 12 ~ "12"
  ))

adpkd.ne <- adpkd.ne %>% 
  mutate(day4 = case_when(
    day3 == 1 ~ "01"
  ))

adpkd.ne$dddirt3 <- as.numeric(paste(adpkd.ne$year3, adpkd.ne$month4, adpkd.ne$day4, sep = ""))
str(adpkd.ne$dddirt3)
adpkd.ne$dddirt3 = ymd(adpkd.ne$dddirt3)


# follow up 
# date death = deathdate
# date inclusion = DDIRT.asd
# date de point = december 

adpkd.ne <- adpkd.ne %>%
  mutate(timene = case_when(
    DEATH == "0" ~ as.Date(december) - as.Date(dddirt3),
    DEATH == "1" ~ as.Date(deathdate2) - as.Date(dddirt3)
  ))

# time == timene
# status == DEATH

adpkd.ne$time <- as.numeric(adpkd.ne$timene)
adpkd.ne$status <- as.numeric(adpkd.ne$DEATH)

#-COX MODEL------------------------------------------

require("survival")
require("survminer")

# tt2
res.cox1 <- coxph(Surv(time, status) ~ ttt2, data = adpkd.ne)
test1 <- cox.zph(res.cox1)
#       chisq df    p
#ttt2    1.82  1 0.18
#GLOBAL  1.82  1 0.18
# proportional hazard 
ggcoxzph(test1)
summary(res.cox1)

# tabac2
res.cox2 <- coxph(Surv(time, status) ~ tabac2, data = adpkd.ne)
test2 <- cox.zph(res.cox2)
summary(res.cox2)

# sex
res.cox3 <- coxph(Surv(time, status) ~ sex, data = adpkd.ne)
test3 <- cox.zph(res.cox3)
summary(res.cox3)

# age
res.cox4 <- coxph(Surv(time, status) ~ age, data = adpkd.ne)
test4 <- cox.zph(res.cox4)
#ggcoxfunctional(Surv(time, status) ~ age + log(age) + sqrt(age), data = adpkd.ne)
# nonlinearity
summary(res.cox4)

# DGN_PAL
#
#adpkd.ne <- adpkd.ne %>%
#  mutate(diagnostic = case_when(
#    DGN_PAL == "G45" ~ "1",
#    DGN_PAL == "I60" ~ "2",
#    DGN_PAL == "I61" ~ "3", 
#    DGN_PAL == "I63" ~ "4",
#    DGN_PAL == "I64" ~ "5"
#  ))
#str(adpkd.ne$diagnostic)
#adpkd.e$diagnostic = as.factor(adpkd.ne$diagnostic)
#
#res.cox5 <- coxph(Surv(time, as.numeric(status)) ~ diagnostic, data = adpkd.e)
#test5 <- cox.zph(res.cox5)
#summary(res.cox5)
#
#require(rms)
#surv5 <- cph(Surv(as.numeric(time), as.numeric(status)) ~ diagnostic, data = adpkd.e, x = T, y = T)
#exp(coef(surv5))
#exp(confint(surv5))
#se <- predict(surv5, se.fit=TRUE)$se.fit 

# atcd

res.cox6 <- coxph(Surv(time, status) ~ atcd, data = adpkd.ne)
test6 <- cox.zph(res.cox6)
summary(res.cox6)

# TRANSP

res.cox7 <- coxph(Surv(time, status) ~ TRANSP, data = adpkd.ne)
test7 <- cox.zph(res.cox7)
summary(res.cox7)

# cardiovasc
res.cox8 <- coxph(Surv(time, status) ~ cardiovasc, data = adpkd.ne)
test8 <- cox.zph(res.cox8)
summary(res.cox8)

# L_ATC4.g
res.cox9 <- coxph(Surv(time, status) ~ L_ATC4.g, data = adpkd.ne)
test9 <- cox.zph(res.cox9)
summary(res.cox9)

# diabetes 
res.cox10 <- coxph(Surv(time, status) ~ diabetes, data = adpkd.ne)
test10 <- cox.zph(res.cox10)
summary(res.cox10)

# bmic
res.cox112 <- coxph(Surv(time, status) ~ bmic2, data = adpkd.ne)
test112 <- cox.zph(res.cox112)
summary(res.cox112)

#-ALL ADPKD+-----------------------------------------------------------------

dim(adpkd)
table(adpkd$EVENT)

# bmi----

table(adpkd$bmic)

adpkd <- adpkd %>% 
  mutate(bmic2 = case_when(
    bmic == "1" | bmic == "2" ~ "1",
    bmic == "3" ~ "2",
    bmic == "4" ~ "3"
  ))
table(adpkd$bmic2)

#-date de décès----

table(adpkd$DDC.asd)
# 2016-01-11
require(lubridate)
a<-ymd(adpkd$DDC.asd)
adpkd$year <- year(a)
adpkd$month <- day(a)
adpkd$day <- month(a)

str(adpkd$month)

adpkd$month = as.numeric(adpkd$month)

adpkd <- adpkd %>%
  mutate(month2 = case_when(
    month == 1 ~ "01",
    month == 2 ~ "02",
    month == 3 ~ "03",
    month == 4 ~ "04",
    month == 5 ~ "05",
    month == 6 ~ "06",
    month == 7 ~ "07",
    month == 8 ~ "08",
    month == 9 ~ "09",
    month == 10 ~ "10",
    month == 11 ~ "11",
    month == 12 ~ "12"
  ))

adpkd <- adpkd %>% 
  mutate(day2 = case_when(
    day == 1 ~ "01"
  ))

adpkd$deathdate <- as.numeric(paste(adpkd$year, adpkd$month2, adpkd$day2, sep = ""))
str(adpkd$deathdate)
adpkd$deathdate2 = ymd(adpkd$deathdate)

# for ddirt----

b <- ymd(adpkd$DDIRT.asd)
adpkd$year3 <- year(b)
adpkd$month3 <- day(b)
adpkd$day3 <- month(b)

adpkd$month3 = as.numeric(adpkd$month3)

adpkd <- adpkd %>%
  mutate(month4 = case_when(
    month3 == 1 ~ "01",
    month3 == 2 ~ "02",
    month3 == 3 ~ "03",
    month3 == 4 ~ "04",
    month3 == 5 ~ "05",
    month3 == 6 ~ "06",
    month3 == 7 ~ "07",
    month3 == 8 ~ "08",
    month3 == 9 ~ "09",
    month3 == 10 ~ "10",
    month3 == 11 ~ "11",
    month3 == 12 ~ "12"
  ))

adpkd <- adpkd %>% 
  mutate(day4 = case_when(
    day3 == 1 ~ "01"
  ))

adpkd$dddirt3 <- as.numeric(paste(adpkd$year3, adpkd$month4, adpkd$day4, sep = ""))
str(adpkd$dddirt3)
adpkd$dddirt3 = ymd(adpkd$dddirt3)


# follow up 
# date death = deathdate
# date inclusion = DDIRT.asd
# date de point = december 

adpkd <- adpkd %>%
  mutate(timene = case_when(
    DEATH == "0" ~ as.Date(december) - as.Date(dddirt3),
    DEATH == "1" ~ as.Date(deathdate2) - as.Date(dddirt3)
  ))

# time == timene
# status == DEATH

adpkd$time <- as.numeric(adpkd$timene)
adpkd$status <- as.numeric(adpkd$DEATH)

km_fit <- survfit(Surv(as.numeric(time), status) ~ EVENT, data = adpkd)
plot(km_fit)

coxdeath = coxph(Surv(time, status) ~ EVENT, data = adpkd)
summary(coxdeath)

# délai de survie moyenne pour les patients décédés 

adpkd %>%
  group_by(EVENT) %>%
  summarise(mean = mean(timene), sd(timene), median(timene))

# descriptif de la population totale----

require(tableone)

variables.s <- c("ttt2","tabac2", "sex", "age", 
                 "L_ATC4.2", "DGN_PAL", "atcd", "EVENT", "groupes6",
                 "TRANSP", "cardiovasc", "L_ATC4.g", 
                 "apkd01", "diabetes", "bmic2", "time", "status")

catvariables.s <- c("ttt2","tabac2", "sex", 
                    "L_ATC4.2", "DGN_PAL", "atcd", "EVENT", "groupes6", 
                    "TRANSP", "cardiovasc", "L_ATC4.g", 
                    "apkd01", "diabetes", "bmic2", "status")

adpkd.all <- CreateTableOne(vars = variables.s, 
                              factorVars = catvariables.s,
                              data = adpkd,
                              includeNA = F)

print(adpkd.all, showAllLevels = TRUE, quote = F, nospaces = TRUE)

adpkd2all = CreateTableOne(vars = variables.s, data = adpkd, factorVars = catvariables.s, 
                               test = F, includeNA = F, strata = "DEATH")
print(adpkd2all, showAllLevels = TRUE, quote = F, noSpaces = TRUE)

#-cox model for the whole population----

require("survival")
require("survminer")

# tt2
res.cox1 <- coxph(Surv(time, status) ~ ttt2, data = adpkd)
test1 <- cox.zph(res.cox1)
ggcoxzph(test1)
summary(res.cox1)

# tabac2
res.cox2 <- coxph(Surv(time, status) ~ tabac2, data = adpkd)
test2 <- cox.zph(res.cox2)
summary(res.cox2)

# sex
res.cox3 <- coxph(Surv(time, status) ~ sex, data = adpkd)
test3 <- cox.zph(res.cox3)
summary(res.cox3)

# age
res.cox4 <- coxph(Surv(time, status) ~ age, data = adpkd)
test4 <- cox.zph(res.cox4)
#ggcoxfunctional(Surv(time, status) ~ age + log(age) + sqrt(age), data = adpkd.e)
# nonlinearity
summary(res.cox4)

# DGN_PAL
table(adpkd$DGN_PAL, useNA = "always")

adpkd <- adpkd 


res.cox5 <- coxph(Surv(time, as.numeric(status)) ~ diagnostic, data = adpkd.e)
test5 <- cox.zph(res.cox5)
#           chisq df    p
#diagnostic  3.14  4 0.53
#GLOBAL      3.14  4 0.53
summary(res.cox5)

require(rms)
surv5 <- cph(Surv(as.numeric(time), as.numeric(status)) ~ diagnostic, data = adpkd.e, x = T, y = T)
exp(coef(surv5))
exp(confint(surv5))
se <- predict(surv5, se.fit=TRUE)$se.fit 

# atcd
res.cox6 <- coxph(Surv(time, status) ~ atcd, data = adpkd)
test6 <- cox.zph(res.cox6)
summary(res.cox6)

# TRANSP
res.cox7 <- coxph(Surv(time, status) ~ TRANSP, data = adpkd)
test7 <- cox.zph(res.cox7)
summary(res.cox7)

# cardiovasc
res.cox8 <- coxph(Surv(time, status) ~ cardiovasc, data = adpkd)
test8 <- cox.zph(res.cox8)
summary(res.cox8)

# L_ATC4.g
res.cox9 <- coxph(Surv(time, status) ~ L_ATC4.g, data = adpkd)
test9 <- cox.zph(res.cox9)
summary(res.cox9)

# diabetes 
res.cox10 <- coxph(Surv(time, status) ~ diabetes, data = adpkd)
test9 <- cox.zph(res.cox10)
summary(res.cox10)

# bmic
res.cox112 <- coxph(Surv(time, status) ~ bmic2, data = adpkd)
test112 <- cox.zph(res.cox112)
summary(res.cox112)

#-death survfit----

km_fitD <- survfit(Surv(time, status) ~ EVENT, data = adpkd)
plot(km_fitD)
