getwd()
setwd("P:/UBRC_M2/REYES/ANALYSIS/DATABASES/csv_data") # ON PC
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
rein <- read.csv2("rein_db.csv", header = TRUE, na.string="")
count(rein)
#View(rein)
names(rein)
head(rein)
str(rein$RREC_COD_ANO)

#-------------------------------------------------------------------------------

# HOSPITALISATION DATABSE (SNDS)
hosp <- read.csv2("snds_hospit.csv", header = TRUE, na.string="")
count(hosp)
#View(hosp)

#-------------------------------------------------------------------------------

# TREATMENT DATA (SNDS)
treat <- read.csv2("snds_medic.csv", header = TRUE, na.string="")
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
