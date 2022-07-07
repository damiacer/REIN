# FOLDERS
getwd()
setwd("/Users/damianocerasuolo/Desktop/PhD/M2/DATABASES_REIN/csv_data") # ON MAC

#---------------------------------------------------------------------------------#

# PACKAGES
library("dplyr")
library("tidyverse")
library("expss")
library("here")
library("gapminder")

#---------------------------------------------------------------------------------#

# TREATMENT DATA (SNDS)     
treat <- read.csv2("snds_medic.csv", header = TRUE, na.string="NA")
count(treat)
dim(treat)
#View(treat)
names(treat)

#---------------------------------------------------------------------------------#

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

#---------------------------------------------------------------------------------#

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

#---------------------------------------------------------------------------------#

# EXLCUDE ADMINISTRATION BEFORE 2015
# THIS IS NOT NECESSARY ON THIS DATASET SINCE EXTRACTION STARTS ON JAN 1ST 2015

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

library(dplyr)

treat.one <- treat %>%
  group_by(NUM_ENQ, lubridate::date(delivrance)) %>%
  arrange(delivrance) %>%
  slice(1) %>%
  ungroup() %>%
  select(delivrance, L_ATC4, NUM_ENQ, DDIRT)

is.data.table(treat.one) # FALSE
is.data.frame(treat.one) # TRUE

count(treat) # 473902
count(treat.one) # 405293

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

#treat_line2 = filter(treat_oneclassifier2, count == 1)
#count(treat_line2) # 33706

#---------------------------------------------------------------------------------#

# TREATMENT AT THE EVENT OR AT THE BASELINE

table(treat_line$L_ATC4)

treat_line$atc = if_else(treat_line$L_ATC4 != "", "1", "O")
table(treat_line$atc)
