# TREATMENT DATABASE

count(treat)
names(treat)
View(treat)

table(treat$L_ATC4)

treat <- as_tibble(treat)
treat <- treat %>% rename(
  # new name = old name,
  "id" = "ï..NUM_ENQ")

treat$eskd <- as.Date(treat$DDIRT, format = "%m/%d/%y")

library("lubridate")
y_eskd <- day(treat$DDIRT)

table(y_eskd)

?as.Date
