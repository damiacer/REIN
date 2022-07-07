###########################
# OBSOLETE                #
###########################


# TRANSFORM THE LAST FOLLOWUP DATE

require("lubridate")

names(random)

random$DDC.d = as.Date(random$DDC, "%d/%m/%Y")
random$DATE_DERNOUV2019.d = as.Date(random$DATE_DERNOUV2019, "%d/%m/%Y")
random$DDIRT.d = as.Date(random$DDIRT, "%d/%m/%Y")
random$DGRF.d = as.Date(random$DGRF, "%d/%m/%Y")
random$evdate.d = as.Date(random$evdate, "%d/%m/%Y")

random$date <- if_else(random$DEATH == "1", random$DDC.d, random$DATE_DERNOUV2019.d)
random$date <- as.Date(random$date, "%d/%m/%Y")

#-------------------------------------------------------------------------------

random$date2 <- if_else(random$event.inclusion == "event", random$evdate.d, random$date)
random$date2 <- as.Date(random$date2, "%d/%m/%Y")

#-------------------------------------------------------------------------------

random$date3 <- if_else(random$event.transpl == "eve-/tr+", random$DGRF.d, random$date2)
random$date3 <- as.Date(random$date3, "%d/%m/%Y")

#-------------------------------------------------------------------------------

print.date <- random[,c("DATE_DERNOUV2019.d", "DGRF.d", "DDC.d", "evdate.d", "DDIRT.d", "date3")]
write_csv2(print.date, "P:/UBRC_M2/REYES/ANALYSIS/DATABASES/csv_data/print_readerdate.csv")
