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

#################################################################################
#################################################################################
#################################################################################

# ADDING TRRATMENT TO BASE

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

is.data.table(treat.one) # FALSE
is.data.frame(treat.one) # TRUE

count(treat) # 473902
count(treat_one) # 405293

# ADD A COUNT NUMBER FOR EACH EVENT FOR EACH SUBJECT
# THE FOLLWING SQL FUNCTION WILL CREATE A NEW COULM CALLED "count"
# THE NAME OF THE NEW COLUMN/VARIABLE CAN BE CHANGED IN THE SQL FUNCION

treat_one = treat.one

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
table(rein_h$DGN_PAL.cc1)

# 2ND = WE ARE INTERESTED IN ALL HAEMORRAGIC CASES

rein_h <- rein_h %>%
  mutate(DGN_PALB.hae = case_when(
    DGN_PAL == "G450" | DGN_PAL == "G451" | DGN_PAL == "G452" | DGN_PAL == "G453" |
      DGN_PAL == "G454" | DGN_PAL == "G458" | DGN_PAL == "G459" ~ "0",
    DGN_PAL == "I630" | DGN_PAL == "I631" | DGN_PAL == "I632" | DGN_PAL == "I633" | DGN_PAL == "I634" |
      DGN_PAL == "I635" | DGN_PAL == "I636" | DGN_PAL == "I638" | DGN_PAL == "I639" ~ "0",
    DGN_PAL == "I64" ~ "0",
    DGN_PAL != "" ~ "1"
  ))
table(rein_h$DGN_PALB.hae)
rein_h$DGN_PALB.hae[is.na(rein_h$DGN_PALB.hae)] <- "0"

#47+126+36+18+23+364+575+90+64+138+521+309+557+5+434+737+322=4366

rein_h <- rein_h %>%
  mutate(DGN_PAL.cc2 = case_when(
    DGN_PALB.hae == "1" ~ DGN_PAL,
    DGN_PALB.hae == "0" ~ "missing"
  ))

# 3RD = WE ARE INTERESTED IN ALL ISCHEMIC CASES

rein_h <- rein_h %>%
  mutate(DGN_PALB.is = case_when(
    DGN_PAL == "I600" | DGN_PAL == "I601" | DGN_PAL == "I602" | DGN_PAL == "I603" | DGN_PAL == "I604" |
      DGN_PAL == "I605" | DGN_PAL == "I606" | DGN_PAL == "I607" | DGN_PAL == "I608" | 
      DGN_PAL == "I609" ~ "0",
    DGN_PAL == "I610" | DGN_PAL == "I611" | DGN_PAL == "I612" | DGN_PAL == "I613" | DGN_PAL == "I614" |
      DGN_PAL == "I615" | DGN_PAL == "I616" | DGN_PAL == "I618" | DGN_PAL == "I619" ~ "0", 
    DGN_PAL == "I620" | DGN_PAL == "I621" | DGN_PAL == "I629" ~ "0",
    DGN_PAL == "I64" ~ "0",
    DGN_PAL != "" ~ "1"
  ))
table(rein_h$DGN_PALB.is)
rein_h$DGN_PALB.is[is.na(rein_h$DGN_PALB.is)] <- "0"

#4+10+14+2+1+1+5+14+61+27+144+68+30+27+42+28+77+112+211+5+45+322=1250

rein_h <- rein_h %>%
  mutate(DGN_PAL.cc3 = case_when(
    DGN_PALB.is == "1" ~ DGN_PAL,
    DGN_PALB.is == "0" ~ "missing"
  ))

# SENSISTIVITY ANALYSIS = EXCLUSION OF SOME EVENTS 

rein_h <- rein_h %>% 
  mutate(DNG_PAL.sens = case_when(
    DGN_PAL == "G810" | DGN_PAL == "G8100" | DGN_PAL == "G8101" | DGN_PAL == "G8108" ~ "0",
    DGN_PAL == "H534" ~ "0",
    DGN_PAL == "R470" | DGN_PAL == "R4700" | DGN_PAL == "R4701" | DGN_PAL == "R4702" |
      DGN_PAL == "R4703" | DGN_PAL == "R471" | DGN_PAL == "R478" ~ "0",
    DGN_PAL != "" ~ "1"
  )) 
rein_h$DNG_PAL.sens[is.na(rein_h$DNG_PAL.sens)] <- "0"
table(rein_h$DNG_PAL.sens)

rein_h <- rein_h %>%
  mutate(DGN_PAL.cc4 = case_when(
    DNG_PAL.sens == "1" ~ DGN_PAL,
    DNG_PAL.sens == "0" ~ "missing"
  ))

#################################################################################

# TEST TO ELIMINATE THE DOUBLED LINES
# dgn_data <- rein_m3[,c("RREC_COD_ANO", "num_enq", "DGN_PAL", "DGN_PALB", "SOR_ANN", "SOR_MOI")]
# View(dgn_data)
dgn_data <- rein_h
names(dgn_data)
count(dgn_data)

# CREATE THE BASE WITH ONLY EVENTS 
# IN THIS DATABASE, ONE PATIENT CAN HAVE MORE THAN ONE EVENT 
# AND PATIENTS FROM THE ORIGINAL DATABASES (BUT WHO HAVE NO EVENT), COMPLETELY MISSING
dgn_complete<-dgn_data[!(dgn_data$DGN_PAL.cc1=="missing"),]
count(dgn_complete)

# FOR HAEMORRHAGIC EVENTS 
dgn_complete<-dgn_data[!(dgn_data$DGN_PAL.cc2=="missing"),]
# FOR ISCHEMIC EVENTS 
dgn_complete<-dgn_data[!(dgn_data$DGN_PAL.cc3=="missing"),]
# FOR SENSITIVITY ANALYSIS 
dgn_complete<-dgn_data[!(dgn_data$DGN_PAL.cc4=="missing"),]

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
names(rdb)

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
    DGN_PAL == "H534" ~ "H53",
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
      DGN_PAL == "R471" | DGN_PAL == "R478" ~ "R47"
  ))

table(rdb$DGN_PAL)
# E0      G45   G46   G81   H53   I60   I61   I63   I64    R47 
# 43030   360    16    23     3    47   245   832   101    90 

rdb$DGN_PAL[is.na(rdb$DGN_PAL)] <- "E0"

table(rdb$DGN_PAL)
#    E0   G45   G46   G81   H53   I60   I61   I63   I64   R47 
# 43309   360    16    23     3    47   245   832   101    90 

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

################################################################################

# DATE VARIABLES AS NUMERIC 

#rdb$DGRF.d = as.numeric(as.Date(rdb$DGRF, "%d/%m/%Y"))
rdb$DDC.d = as.numeric(as.Date(rdb$DDC, "%d/%m/%Y"))
rdb$evdate.d = as.numeric(as.Date(rdb$evdate, "%d/%m/%Y"))

# DATE VARIABLES AS DATE VARIABLES 

rdb$DGRF.asd = as.Date(rdb$DGRF, "%d/%m/%Y")
rdb$DDC.asd = as.Date(rdb$DDC, "%d/%m/%Y")
rdb$evdate.asd = as.Date(rdb$evdate, "%d/%m/%Y")

################################################################################

rdb <- rdb %>%
  mutate(epilogus = case_when(
    # DEAD PATIENTS 
    DEATH == "1" & (DDC.asd > evdate.asd) ~ FUP.E, 
    DEATH == "1" & (DDC.asd > DGRF.asd) & (DGRF.asd > evdate.asd) ~ FUP.E,
    DEATH == "1" & (DDC.asd > DGRF.asd) & (DGRF.asd < evdate.asd) ~ FUP.T,
    DEATH == "1" & EVENT == "0" ~ FUP.D, 
    # NOT DEAD PATIENTS
    DEATH == "0" & (evdate.asd > DGRF.asd) ~ FUP.E,
    DEATH == "0" & (evdate.asd < DGRF.asd) ~ FUP.T,
    DEATH == "0" & EVENT == "0" ~ FUP.19,
    DEATH == "0" & EVENT == "0" & TRANSP == "1" ~ FUP.T
   ))
table(rdb$epilogus)

#-------------------------------------------------------------------------------

rdb <- rdb %>% 
  mutate(epilogus.num = case_when(
    # DEAD PATIENTS 
    DEATH == "1" & (DDC.d > evdate.d) ~ FUP.E, 
    DEATH == "1" & (DDC.d > DGRF.d) & (DGRF.d > evdate.d) ~ FUP.E,
    DEATH == "1" & (DDC.d > DGRF.d) & (DGRF.d < evdate.d) ~ FUP.T,
    DEATH == "1" & EVENT == "0" ~ FUP.D, 
    # NOT DEAD PATIENTS
    DEATH == "0" & (evdate.d > DGRF.d) ~ FUP.E,
    DEATH == "0" & (evdate.d < DGRF.d) ~ FUP.T,
    DEATH == "0" & EVENT == "0" ~ FUP.19,
    DEATH == "0" & EVENT == "0" & TRANSP == "1" ~ FUP.T   
  ))
table(rdb$epilogus.num)

#-------------------------------------------------------------------------------

# misclassification issues 
# table(rdb$epilogus, rdb$epilogus.num)

deciles = quantile(rdb$epilogus, probs = seq(0.1, 1, by = 0.1), na.rm = T)
#  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
#  358  718  723  728 1084 1091 1449 1457 1817 2171 

deciles = quantile(rdb$epilogus.num, probs = seq(0.1, 1, by = 0.1), na.rm = T)
#  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
#  359  718  724  729 1085 1091 1450 1457 1817 2172 

str(rdb$epilogus)

rdb$epilogus.numb = as.numeric(rdb$epilogus)
rdb$epilogus.num.b = as.numeric(rdb$epilogus.num)

# --> lines
rdb <- rdb %>% 
  mutate(ep.class = case_when(
    epilogus.numb == 0 ~ "001",
    epilogus.numb > 0 & epilogus.numb < 358 ~ "01",
    epilogus.numb >= 358 & epilogus.numb < 718 ~ "02",
    epilogus.numb >= 718 & epilogus.numb < 723 ~ "03",
    epilogus.numb >= 723 & epilogus.numb < 728 ~ "04",
    epilogus.numb >= 728 & epilogus.numb < 1084 ~ "05",
    epilogus.numb >= 1084 & epilogus.numb < 1091 ~ "06",
    epilogus.numb >= 1091 & epilogus.numb < 1449 ~ "07",
    epilogus.numb >= 1449 & epilogus.numb < 1457 ~ "08",
    epilogus.numb >= 1457 & epilogus.numb < 1817 ~ "09",
    epilogus.numb >= 1817 & epilogus.numb < 2171 ~ "10"
  ))

# --> column
rdb <- rdb %>% 
  mutate(epnum.class = case_when(
    epilogus.num.b == 0 ~ "000",
    epilogus.num.b > 0 & epilogus.num.b < 358 ~ "01",
    epilogus.num.b >= 358 & epilogus.num.b < 718 ~ "02",
    epilogus.num.b >= 718 & epilogus.num.b < 724 ~ "03",
    epilogus.num.b >= 724 & epilogus.num.b < 729 ~ "04",
    epilogus.num.b >= 729 & epilogus.num.b < 1085 ~ "05",
    epilogus.num.b >= 1085 & epilogus.num.b < 1091 ~ "06",
    epilogus.num.b >= 1091 & epilogus.num.b < 1450 ~ "07",
    epilogus.num.b >= 1450 & epilogus.num.b < 1457 ~ "08",
    epilogus.num.b >= 1457 & epilogus.num.b < 1817 ~ "09",
    epilogus.num.b >= 1817 & epilogus.num.b < 2172 ~ "10"
  ))

table(rdb$ep.class, rdb$epnum.class)

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

################################################################################

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

################################################################################

# CARDIOVASCULAR DISEASE
# 1 = AT LEAST ONE CARDIOVASCULAR AFFECTION 
# 0 = NO CARDIOVASCULAR DISEASE
# CEREBRAL ISCHEMIA NOT TAKEN INTO ACCOUNT

rdb$cardiovasc = if_else(rdb$ICn + rdb$ICOROn + rdb$IDMn + rdb$RYTHMn + 
                           rdb$ANEVn > 0, "1", "0")

#-------------------------------------------------------------------------------

# DIALYSIS METHOD

rdb <- rdb %>% 
  mutate(dial = case_when(
    # HEM
    METHOn == "1" ~ "1", 
    # PERIT
    METHOn == "2" ~ "2",
  )) 

table(rdb$dial)

#-------------------------------------------------------------------------------

# APKD
rdb$apkd01 = if_else(rdb$nephgp == "APKD", "1", "0")
table(rdb$apkd01)
#     0     1 
# 42466  2560 

#-------------------------------------------------------------------------------

# DIABETES Y/N

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

################################################################################


