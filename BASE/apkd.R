count(apkd)

names(apkd)

# FOR LABELS 
# (if the procedure ran in the "rein_multiplevents.R")
# var_lab(DATABASE$VARNAME)

# TRANSPLANTATION DATE
table(apkd$DGRF)

################################################################################

# NEW VARIABLES
# (some of these variables might be created in "rein_multievents.R")

# EVENT VARIABLE (GROUPING01)
m <- as.data.frame(apkd$DGN_PAL)
m[is.na(m)] <- "E0"
table(m)
apkd$grouping <- m
apkd$grouping01[apkd$grouping == "E0"]  <- 0
apkd$grouping01[apkd$grouping != "E0"]  <- 1
tabev <- table(apkd$grouping01)
prop.table(tabev)
apkd$grouping01 <- as.character(apkd$grouping01)

# VARIABLE KIDNEY TRANSPLANTATION TRANSPLANTATION YES/NO
apkd$DGRFd = as.Date(apkd$DGRF, "%d/%m/%Y")
t <- as.data.frame(apkd$DGRFd)
t[is.na(t)] <- "2000-01-01"
table(t)
# 2000-01-01 == 1317 
apkd$DGRFd2 <- t
apkd$DGRFd201[apkd$DGRFd2 == "2000-01-01"] <- 0 # NO TRANSPLANT
apkd$DGRFd201[apkd$DGRFd2 != "2000-01-01"] <- 1 # TRANSPLANTATION
table(apkd$DGRFd201)
str(apkd$DGRFd201) # NUMERIC

################################################################################

# TRANSPLANTATION DATE: DGRF
# VERIFY THAT EVENTS OCCUR BEFORE THE TRANSPLANTATION
# OTHERWISE EXCLUDE THOSE EVENTS

# DGFR: Date de greffe
# evdate: event date

# drop <- c("x","z")
# df = mydata[,!(names(mydata) %in% drop)]

apkd$eventafter = as.Date(apkd$evdate, "%d/%m/%Y") - as.Date(apkd$DGRF, "%d/%m/%Y")
# IF THIS QUANTITY IS NEGATIVE, THE EVENT OCCURS BEFORE THE TRANSPLANTATION
# WHEN POSITIVE, THE EVENT OCCURS AFTER THE KIDNEY TRANSPLATATION
# EVENTS AFTER THE TRANSPLATATION MUST BE ERASED FROM THE DATASET AS
table(apkd$eventafter)
str(apkd$eventafter)
apkd$eventafter = as.numeric(as.character(apkd$eventafter))

apkd$eventafter01 = ifelse(apkd$eventafter < 0 , 1, 0) 
# 1 = THE EVENT OCCURS before THE TRANSPLANTATION
# 0 = THE EVENT OCCUR after THE TRANSPLANTATION
# ONLY EVENTS before (1) THE TRANSPL WILL BE KEPT
table(apkd$eventafter01)
table(apkd$eventafter01, apkd$grouping01)

apkd$grouping01num = as.numeric(as.character(apkd$grouping01)) 

#-------------------------------------------------------------------------------

apkd$eventafter01 = as.factor(apkd$eventafter01)
apkd$grouping01 = as.factor(apkd$grouping01)

# NEW EVENT VARIABLE 
# THIS VARIABLE ACCOUNTS ONLY FOR EVENT BEFORE THE TRANSPLANTATION

apkd$hap2[apkd$eventafter01 == "1" & apkd$grouping01 == "1"] <- "1" #event
apkd$hap2[apkd$eventafter01 == "0" & apkd$grouping01 == "1"] <- "0" #no event
table(apkd$hap2)

################################################################################

#-----------------------------#
# THIS SECTION NEEDS REVISION #
#-----------------------------#

# GENERAL FUP
apkd$fu = as.Date(apkd$DATE_DERNOUV2019, "%d/%m/%Y") - as.Date(apkd$DDIRT, "%d/%m/%Y")
apkd$fu_n = as.numeric(as.character(apkd$fu))
mean(apkd$fu_n)
max(apkd$fu_n)
min(apkd$fu_n)

aggregate(fu_n ~ grouping01, apkd, mean)
# OR
tapply(apkd$fu_n, apkd$grouping01, mean)

# FUP UNTIL EVENT
apkd_devent <- apkd[!(apkd$grouping01=="0"),] # GROUPING 0 MEANS NO EVENT
count(apkd_devent)

# FOLLOW UP TIME TO EVENT FOR ALL ADPK SUBJECTS
# SOME EVENTS OCCUR BEFORE THE INCLUSION AND THE WHOLE FU IS THEN NEGATIVE 
# TAKE IT INTO ACCOUNT WHEN CALCULATING THE FOLLOW UP
str(apkd_devent$evdate)
apkd_devent$fue = as.Date(apkd_devent$evdate, "%d/%m/%Y") - as.Date(apkd_devent$DDIRT, "%d/%m/%Y")
table(apkd_devent$fue)
apkd_devent$fue_n = as.numeric(as.character(apkd_devent$fue))

# SOME EVENTS OCCUR BEFORE THE INCLUSION IN THE DATASET
# THE FOLLOWING CODE IDENTIFIES THESE EVENTS

apkd_devent$fue_before[apkd_devent$fue<0] <- "1" # EVENTS BEFORE INCLUSION: 171
apkd_devent$fue_before[apkd_devent$fue>=0] <- "0" # EVENTS AFTER INCLUSION: 261
table(apkd_devent$fue_before)

# DATA WITH ONLY EVENTS OCCURED AFFET THE INCLUSION
apkd_deventb <- apkd_devent[!(apkd_devent$fue_before=="1"),]

apkd_deventb$fue_afterinc = 
  as.Date(apkd_deventb$evdate, "%d/%m/%Y") - as.Date(apkd_deventb$DDIRT, "%d/%m/%Y")
apkd_deventb$fue_afterincn = as.numeric(as.character(apkd_deventb$fue_afterinc))
mean(apkd_deventb$fue_afterincn)
# 586.4943

#-------------------------------------------------------------------------------

# FOLLOW UP FOR THE WHOLE POPULATION
# THE FOLLOWING CODE RESUMES THE FOLLOW UP TIMES AS THEY SHOULD BE CALCULATED
# apkd$followup[apkd_devent$fue>=0] = apkd_deventb$fue_n
# apkd$followup[apkd_devent$fue<0] = apkd$fu_n
# apdk$followup[apkd$grouping01=="0"] = apkd$fu_n

##########################################
# FOLLOW UP ACCORDING TO TRANSPLANTATION #
##########################################

# EVENT DATE FOR PATIENTS WITH TRANSPLANTATION
# CREATE THE DATABASE FOR ONLY PEOPLE HAVING UNDERGONE A TRANSPLANTATION
# THE TRANSPLANTATION VARIABLE IS CREATED ABOVE, IN THE apkd DATA: DGRFd201
# 1 = transplantation == 1243
# 0 = no transplantation == 1317
# OVER 2560 PATIENTS IN THE DATABASE

apkdtranspl<-apkd[!(apkd$DGRFd201=="0"),]
count(apkdtranspl) #1243
# EVENT AFTER THE TRANSPLANTATION (0) OR BEFORE THE TRANSPLANTATION (1) : eventafter01
# PATIENTS WHOSE EVENT HAPPENS AFTER THE TRANSPLANTATION
ah = as.Date(apkdtranspl$DGRF, "%d/%m/%Y") - as.Date(apkdtranspl$DDIRT, "%d/%m/%Y")
# PATIENTES WHOSE EVENT HAPPENS BEFORE THE TRANSPLANTATION
bh = as.Date(apkdtranspl$evdate, "%d/%m/%Y") - as.Date(apkdtranspl$DDIRT, "%d/%m/%Y")
ch = as.Date(apkdtranspl$DATE_DERNOUV2019, "%d/%m/%Y") - as.Date(apkdtranspl$DDIRT, "%d/%m/%Y")
  ah = as.numeric(as.character(ah))
  bh = as.numeric(as.character(bh))
  ch = as.numeric(as.character(ch))

# THIS FOLLOW UP CONCERNS ONLY PATIENTES HAVING UNDERGONE A TRANSPLANTATION OR THE EVENT
derniertranspl = as.Date(apkdtranspl$DATE_DERNOUV2019, "%d/%m/%Y") - as.Date(apkdtranspl$DGRF, "%d/%m/%Y")
# IF SOME VALUES ARE POSITIVE, LAST DATE OF FOLLOW UP OCCURS AFTER THE TRANSPLANTATION
# IF ALL THE VALUES ARE NEGATIVE, THE LAST DATE OF FOLLOW UP IS THE TRANSPLANTATION OR BEFORE IT
# IN THIS DATABASE WE CONSIDER AS CENSORED, ALL THE SUBJECTS HAVING UNDERGONE A TRANSPLANTATION
derniertransplN = as.numeric(as.character(derniertranspl))
is.na(derniertransplN)
mean(derniertransplN #, na.rm = TRUE
     )
min(derniertransplN)
max(derniertransplN)
# ALL THE VALUES ARE POSITIVE OR EQUAL TO ZERO
# THE FUP WILL BE GIVEN BY TRANSPLANTATION-INCLUSION OR EVENT-INCLUSION

# table(apkdtranspl$eventafter01)
# 0   1 
# 47 107 
# 1 = THE EVENT OCCURS before THE TRANSPLANTATION
# 0 = THE EVENT OCCUR after THE TRANSPLANTATION
apkdtranspl$transfup = ifelse(apkdtranspl$eventafter01 == "0", ah, bh)
mean(apkdtranspl$transfup
     , na.rm = TRUE) # VALUES CAN BE:
                        # NEGATIVE=THE EVENT OCCURS BEFORE THE INCLUSION
                        # POSITIVE=THE EVENT OCCURS AFTER THE INCLUSION
                        # MISSING=THE EVENT HAS NOT OCCURRED IN THIS PATIENTS, THE FUP STOPS AT LAST DATE
min(apkdtranspl$transfup, na.rm = TRUE) 
max(apkdtranspl$transfup, na.rm = TRUE)

table(apkdtranspl$grouping01)
#    0    1 
# 1089  154 = 1243

apkdtranspl$transfup_overall = ifelse(apkdtranspl$grouping01 == "0", ch, apkdtranspl$transfup)
# THIS FUP TAKES INTO ACCOUNT THE SUBJECTS WITH NO EVENT AND THEN FOLLOWED UP TO THE LASTE DATE
table(apkdtranspl$transfup_overall)
str(apkdtranspl$transfup_overall) # NUMERIC
min(apkdtranspl$transfup_overall) # NEGATIVE VALUES INDICATE THAT THE EVENT OCCURS BEFORE THE INCLUSION

# TO EXCLUDE NEGATIVE VALUES -- EVENT BEFORE THE INCLUSION
apkdtranspl$evtransplanted_beforeIN = ifelse(apkdtranspl$transfup_overall < 0, 1, 0) 
# IF THE VALUE IS 1, THE EVENT OCCURS BEFORE THE INCLUSION
table(apkdtranspl$evtransplanted_beforeIN) # THERE ARE 68 EVENTS OCCURRING BEFORE THE INCLUSION
# USE THE grouping01 VARIABLE TO CREATE A VARIABLE THAT APPLIES ONLY TO PATIENTS PRESENTING AN EVENT
apkdtranspl$grouping01rec[apkdtranspl$grouping == "E0" 
                          | apkdtranspl$evtransplanted_beforeIN == "1"] <- "0" 
                          # THE VARIABLE WILL GIVE NO EVENT AFTER THE INCLUSION (NO EVENT AT ALL)
apkdtranspl$grouping01rec[apkdtranspl$grouping != "E0" 
                          & apkdtranspl$evtransplanted_beforeIN == "0"] <- "1" 
                          # EVENT AFTER INCLUSION
table(apkdtranspl$grouping01rec)

# FUP REVISED WITH MEDICAL TRACK EVENTS
apkdtranspl$transfup_overallC = ifelse(apkdtranspl$grouping01rec == "0", ch, apkdtranspl$transfup_overall)
mean(apkdtranspl$transfup_overallC, na.rm = TRUE)
min(apkdtranspl$transfup_overallC)

# EVENT FOR EVERYONE ELSE
# CREATE DATABASE FOR ALL THE PATIENTS (W/out TRANSPLANTATION)
apkdnotr<-apkd[!(apkd$DGRFd201=="1"),]
count(apkdnotr) #1317

a = as.Date(apkdnotr$DATE_DERNOUV2019, "%d/%m/%Y") - as.Date(apkdnotr$DDIRT, "%d/%m/%Y")
b = as.Date(apkdnotr$evdate, "%d/%m/%Y") - as.Date(apkdnotr$DDIRT, "%d/%m/%Y")
a = as.numeric(as.character(a))
b = as.numeric(as.character(b))

# IF GROUPING IS ZERO, THEN THE PATIENT HAS NO EVENT
# IF THE GROUPING IS ONE, THEN THE PATIENT HAS ONE EVENT
table(apkdnotr$grouping01)
#    0    1 
# 1039  278 = 1317

# FOLLOW TIME FOR EVENT AND NO-EVENT SUBJECTS 
# THIS FUP WORKS ONLY FOR PATIENTS WITHOUT TRANSPANTATION
apkdnotr$fup = ifelse(apkdnotr$grouping01 == "0", a, b)
mean(apkdnotr$fup)
# 475.1632
str(apkdnotr$fup)
min(apkdnotr$fup) # NEGATIVE VALUES INDICATE THAT SOME EVENTS HAPPENS BEFORE THE INCLUSION

apkdnotr$fup_beforeinc = ifelse(apkdnotr$fup < 0, 1, 0)
# IF THE VALUE IS LESS THAN ZERO (THEN "1" IN THE NEW VARIABLE CODING, 
# THE EVENT OCCURS BEFORE THE INCLUSION)

# CREATE THE VARIABLE TO EXCLUDE EVENTS BEFORE THE INCLUSION
apkdnotr$grouping01rec[apkdnotr$grouping == "E0" | apkdnotr$fup_beforeinc == "1"] <- "0" 
                  # NO EVENT AFTER THE INCLUSION
apkdnotr$grouping01rec[apkdnotr$grouping != "E0" & apkdnotr$fup_beforeinc == "0"] <- "1" 
                  # EVENT AFTER INCLUSION
table(apkdnotr$grouping01rec)

# CREATE THE FOLLOW-UP FOR THE POPULATION NOT HAVING UNDERGONE THE EVENT
apkdnotr$notrfup_overallC = ifelse(apkdnotr$grouping01rec == "0", a, b)
mean(apkdnotr$notrfup_overallC, na.rm = TRUE)
min(apkdnotr$notrfup_overallC)

# FOLLOW UP FOR ALL THE SUBJECTS INCLUDED
#  FUP FOR PPL W/ TRANSPLANTATION 
apkdtranspl$transfup_overallC
### var to identify transplantation
### apkd$DGRFd201 # NUMERIC
# FUP FOR PPL W/OUT TRANSPLANTATION
apkdnotr$notrfup_overallC

# CREATE ONE FUP FOR THE WHOLE POPULATION
apkd$glofup = ifelse(apkd$DGRFd201 == "0", apkdnotr$notrfup_overallC, apkdtranspl$transfup_overallC)

is.na(apkd$glofup)
mean(apkd$glofup)
min(apkd$glofup) 
  # THE PRESENCE OF NEGATIVE VALUES INDICATES THAT SOME EVENTS OCCURE BEFORE THE INCLUSION 
max(apkd$glofup)
median(apkd$glofup)

# apkd$fup_trackrec = ifelse(apkd$glofup < 0, "1", "0") # 1 = EVENT BEFORE THE INCLUSION
# table(apkd$fup_trackrec)
# if glofup is negative, the event occurs before the inclusion: the event is a medical track and not 
# the event. if glofup is negative, then the follow up should be:
# 1. to the last date of fup
# 2. to the date of dialysis
# COMMENT: ifelse function is only binary and we have here two different follow ups. 
# this procedure should be done previously in the database

# apkd$grouping01rec[apkd$grouping == "E0" | apkd$fup_trackrec == "1"] <- "0" # NO EVENT AFTER THE INCLUSION
# apkd$grouping01rec[apkd$grouping != "E0" & apkd$fup_trackrec == "0"] <- "1" # EVENT AFTER INCLUSION
# table(apkd$grouping01rec)

# DEFINITVE FOLLOW UP TAKING INTO ACCOUNT THE EVENT BEFORE/AFTER THE INCLUSION OF THE SUBJECTS
# apkd$followup = ifelse(apkd$grouping01rec == "0", a, b)
# mean(apkd$followup)
# 772.0238
# str(apkd$followup)

################################################################################

#####################
# SAVE THE DATABASE #
#####################

# more on saving and loading in R: 
# https://www.r-bloggers.com/2019/05/how-to-save-and-load-datasets-in-r-an-overview/

save(apkd, file = "apkd_fup.RData")
write.table(apkd, file = "apkd_fup.csv", sep = "\t", row.names = F)

################################################################################
################################################################################
################################################################################

# STUDY THE INCIDENCE
apkd$x = apkd$num_enq
apkd$y = apkd$glofup/(355.25/12) #FUP IN MONTHS
apkd$cen = apkd$grouping01


# GGPLOT OF THE INCIDENCE
# (COMMENT: IT IS ARTY ;))
library("ggplot2")

p <- ggplot(apkd, aes(x = x, y = y, color = cen)) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = y)) +
  geom_point(color = ifelse(apkd$cen == "1", "orange", "blue"), size = ifelse(apkd$cen == "1", 5,2)) +
  theme_bw() +
  coord_flip() +
  xlab("subject") +
  scale_y_continuous(name = "FUP", breaks = seq(0, 24, 3), limits = c(0, 24)) +
  ggtitle("Horizontal plot of follow-up time") +
  labs(color = "Status")
p

# INCIDENCE CALCULATION

# INCIDENCE / PERSON MONTH
options(scipen = 1, digits = 2)
inc_apkd <- sum(apkd$cen == "1") / sum(apkd$y)
# 0.0065 / person month

# INCIDENCE / PERSON MONT DURING THE FIRST 12 MONTHS
options(scipen = 1, digits = 2)
inc_apkd.dodici <- apkd %>%
  filter(y<=12) %>%
  summarise(n = n(), months = sum(y))

################################################################################

##########################################################################
# TRANSFORM DATE BEFORE THE FIRST INCLUSION (1/1/2015) IN MISSING VALUES #
##########################################################################

library("dplyr")
library("lubridate")

apkd$DDIRT.date = as.Date(apkd$DDIRT, "%d/%m/%Y")

hap.incid <- apkd %>% 
  select(evdate, num_enq, RREC_COD_ANO, DDIRT.date) %>%
  filter(evdate > DDIRT.date)
is.data.frame(hap.incid)
# View(hap.incid)
# count(hap.incid) # 1 = event = 261
# table(apkd$grouping01) # 1 (event) = 432

# CREATE THE EVENT FROM THE EVENT DATE
table(hap.incid$evdate)
hap.incid$evdate01 = as.numeric(hap.incid$evdate)
hap.incid$evdate01 = ifelse(hap.incid$evdate01 > 0, 1, 0) # 1=EVENT, 0=NO EVENT
table(hap.incid$evdate01)

apkd.dathap <- merge(apkd, hap.incid, by.x = "RREC_COD_ANO", by.y = "RREC_COD_ANO", 
                all.x = TRUE, all.y = FALSE)
# count(apkd.dathap) # 2560
# count(apkd) # 2560

# table(apkd.dathap$evdate.y) # VARIABLE DATE FOR EVENTS HAPPENING AFTER THE INCLUSION IN 2015
apkd.dathap <- as_tibble(apkd.dathap)
apkd.dathap <- apkd.dathap %>% rename(
  # new name = old name,
  "hapdate" = "evdate.y")

################################################################################

# CREATE THE NEW VARIABLE FOR DATE

hap01 <- as.data.frame(apkd.dathap$evdate01)
hap01[is.na(hap01)] <- "0"
table(hap01)
# hap01
# 0    1 
# 2299  261 == 2560

apkd.dathap$EVENTUM = hap01[["apkd.dathap$evdate01"]]
str(apkd.dathap$EVENTUM)

################################################################################

install.packages("incidence")
library("incidence")
# SEE https://repidemicsconsortium.org/incidence

# CREATE THE APKD DATABASE INCLUDING ONLY THE PATIENTS HAVING HAD THE EVENT 
# AFTER THE INCLUSION
# THE VARIABLE fup_trackrec CLASSES THE SUBJECTS AS
# 2399 = INCIDENT EVENT 
# 161 = PREVALENT EVENT
# apkd.incident <- apkd[!(apkd$fup_trackrec=="1"),]

# INCIDENCE BY 7-DAY INTERVALS
i.7 = incidence(apkd.dathap$hapdate, interval = 7)
i.7
plot(i.7)

# INCIDENCE BY MONTH INTERVALS
i.3043 = incidence(apkd.dathap$hapdate, interval = 30.4375) #365.25/12
plot(i.3043)

# INCIDENCE BY YEAR-DAY INTERVALS
i.365 = incidence(apkd.dathap$hapdate, interval = 365)
plot(i.365)

#-------------------------------------------------------------------------------

# FILTERING ACCORDING TO DATE

# duemilaquindici <- apkd.incident %>% 
#  select(evdate, num_enq, RREC_COD_ANO) %>%
#  filter(evdate > "2015-01-01")

# is.data.frame(duemilaquindici)
# sapply(duemilaquindici, class)
# count(duemilaquindici)
# table(apkd.incident$grouping01)
# str(duemilaquindici)

# THE STEPS MARKED WITH "##" ARE NECESSARY ONLY IF WE WANT TO CREATE A 
# BINARY VARIABLE IN THE NEW DATASET FOR EVENTS BEFORE/AFTER 2015
## duemilaquindici$evdateN = as.numeric(duemilaquindici$evdate)
## table(duemilaquindici$evdateN)
# is.na(duemilaquindici$evdateN)
# CREATE VARIABLE TO IDENTIFY ONLY EVENTS AFTER 2015
## duemilaquindici$evdate01 = ifelse(duemilaquindici$evdateN > 0, 1, 0)
## table(duemilaquindici$evdate01)
## count(duemilaquindici)
  
# THE FOLLOWING STEP ALLOWS TO ADD THE VARIABLE eve
# apkd15 <- merge(apkd.incident, duemilaquindici, by.x = "RREC_COD_ANO", by.y = "RREC_COD_ANO", 
#                all.x = TRUE, all.y = FALSE)
# count(apkd15)
# count(apkd)
# count(apkd.incident)

# THE NEW VARIABLE evdate01 (AVAILABLE ONLY IN THE apkd15 DATASET)
# ALLOWS TO SELECT ONLY EVENTS OCCURRING AFTER 2015

# table(apkd15$evdate.x) # ALL THE DATES
# table(apkd15$evdate.y) # DATES ONLY AFTER 2015

# INCIDENCE BY 7-DAY INTERVALS
# i.7.2015 = incidence(apkd15$evdate.y, interval = 7)
# i.7.2015
# plot(i.7.2015)

# INCIDENCE BY MONTH INTERVALS
# i.m.2015 = incidence(apkd15$evdate.y, interval = 30.4375) #365.25/12
# plot(i.m.2015)

# INCIDENCE BY YEAR-DAY INTERVALS
# i.365.2015 = incidence(apkd15$evdate.y, interval = 365)
# plot(i.365.2015)

################################################################################

# CURRENT SURVIVAL

install.packages("currentSurvival")
library("currentSurvival")

apkd.cml = apkd[,c("cen", "y"), drop(FALSE)]
apkd.cml$cen = as.numeric(as.character(apkd.cml$cen))
res <- cci(apkd)

#-------------------------------------------------------------------------------

# SURVIVAL BY SURVIVAL FOR apkd.incident

# USE THE DATASET "apkd.incident" TO INCLUDE ONLY EVENTS OCCURRED AFTER THE INCLUSION
# apkd.incident <- apkd[!(apkd$fup_trackrec=="1"),]

### TIME
apkd.dathap$time = apkd.dathap$glofup / (365.25/12)
apkd.dathap$time = as.numeric(apkd.dathap$time)
### STATUS
apkd.dathap$status = as.factor(apkd.dathap$EVENTUM)
str(apkd.dathap$status)

### KM
KM = survfit(Surv(time, status) ~ 1, data = apkd.dathap)
plot(KM)
plot(KM, main = "Survival", lwd = 2, ylab = "Survival", 
        xlab = "Time (months)", ylim = c(0,0.3)
       #, xlim=c(0,10)
        )

library("ggfortify")

autoplot(KM, surv.linetype = "dashed", surv.colour = "orange",
        censor.colour = "red", conf.int = "TRUE", censor.shape = "*")

library("survminer")

ggsurvplot(KM, data = apkd.dathap, check.names = FALSE)
