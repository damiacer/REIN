###########################
# OBSOLETE                #
###########################

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
hap.data2$transplantation01 = ifelse(hap.data2$transplantation01 > 0, 1, 0) #1 = TRANSPLANTATION AFTER EVENT
                                                                             #0 = TRANSPLANTATION BEFORE EVENT
                                                                             #1 == 107
table(hap.data2$transplantation01)

#-------------------------------------------------------------------------------

# BASES
count(apkd) # n=2560

# 1 EVENTS
APKD1 = merge(apkd, hap.data1, by.x = "RREC_COD_ANO", by.y = "RREC_COD_ANO",
              all.x = TRUE, all.y = FALSE)
count(APKD1) # n=2560

APKD2 = merge(APKD1, hap.data2, by.x = "RREC_COD_ANO", by.y = "RREC_COD_ANO",
              all.x = TRUE, all.y = FALSE)
count(APKD2) # n=2560

names(APKD2)

APKD.names = subset(APKD2, select = -c(num_enq.x, num_enq.y, 
                                       transplantation.d.x, transplantation.d.y, 
                                       event.d.x, event.d.y, 
                                       inclusion.d.x, inclusion.d.y))

names(APKD.names)

#-------------------------------------------------------------------------------

# NEW VARIABLES 

eventinclusion <- as.data.frame(APKD.names$event01)
eventinclusion[is.na(eventinclusion)] <- "0"
table(eventinclusion)

# eventinclusion
# 0    1 
# 2299  261 == 2560

eventtransp <- as.data.frame(APKD.names$transplantation01)
eventtransp[is.na(eventtransp)] <- "0"
table(eventtransp)

# eventtransp
# 0    1 
# 2453  107 
