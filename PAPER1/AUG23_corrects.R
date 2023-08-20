# august 22 corrections

# question 
# what is the delay between event and death - if death occurs?
# patients are censored at the event? 

# delai entre event and death (on the final data set)
# evdate
# DDC

#-DEATH----------------------------------------------------------------------
table(rdb$evdate, rdb$DDC)

require("lubridate")

rdb$death_event = as.Date(rdb$DDC, "%m/%d/%Y") - as.Date(rdb$evdate, "%m/%d/%Y")
table(rdb$death_event, useNA = "always")

# is death an event? 
rdb$death_event = as.numeric(rdb$death_event)
rdb$death_eventC <- rdb$death_event
rdb$death_eventC[is.na(rdb$death_eventC)] <- 9999
table(rdb$death_eventC, useNA = "always")

rdb <- rdb %>%
  mutate(death_eventC = case_when(
    death_eventC == 9999 ~ "not dead",
    death_eventC >= 0 ~ "dead"
   # death_eventC == 9999 ~ "not dead"
  ))

table(rdb$death_eventC, useNA = "always")

# delai_DC

rdb$delai_DCm <- rdb$delai_DC
str(rdb$delai_DCm)
rdb$delai_DCm = as.numeric(as.character(rdb$delai_DCm))
max(rdb$delai_DCm)

rdb <- rdb %>% 
  mutate(delai_DCbin = case_when(
    delai_DCm == 0 ~ "not dead (del)", 
    delai_DCm > 0 ~ "dead (del)"
  ))
str(rdb$delai_DCm)

table(rdb$delai_DCbin, rdb$death_eventC, useNA = "always")

# la variable delai dc n'est pas adapté pour reconnaitre le deces 

#-REINH-------------------------------------------------------------------------

dim(rein_h) # 52724 rows and 82 columns

rein_h = subset(rein_h, select = -c(delai_DC))

library("sqldf")
rein_hone <- sqldf("SELECT a.*, COUNT(*) count
       FROM rein_h a, rein_h b 
       WHERE a.num_enq = b.num_enq AND b.ROWID <= a.ROWID 
       GROUP BY a.ROWID"
)

#rein_honeC = subset(rein_hone, select = c(num_enq, count, RREC_COD_ANO))
#View(rein_honeC)

names(rein_hone)
str(rein_hone$count)
rein_hone$count = as.factor(rein_hone$count)
table(rein_hone$count)
View(rein_hone)

rein_hone <- rein_hone %>% 
  separate(num_enq, c(NA, "ID")) # TO RUN 
str(rein_hone$ID)
rein_hone$ID = as.numeric(as.character(rein_hone$ID))

rein_hone$IDm <- rein_hone$ID
str(rein_hone$IDm)
rein_hone$IDm[is.na(rein_hone$IDm)] <- "99999999"

rein_hone <- rein_hone %>%
  mutate(IDmiss = case_when(
    IDm == "99999999" ~ "missing",
    IDm != "99999999" ~ "notmissing"
  ))
table(rein_hone$IDmiss)

#the num_enq is missing for 2293
#missing notmissing 
#2293      50431 

rein_hcompl <-rein_hone[!(rein_hone$IDmiss=="missing"),]
dim(rein_hone) #52724-2293 missing num_enq patients
dim(rein_hcompl) #50431

str(rein_hcompl$count)
rein_hcompl$count = as.numeric(as.character(rein_hcompl$count))
table(rein_hcompl$count, )

