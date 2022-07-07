# DATABASE: HOSPITALISATIONS

install.packages("data.table")
library("data.table")

View(hosp)
names(hosp)

hosp <- as_tibble(hosp)
hosp <- hosp %>% rename(
  # new name = old name,
  "id" = "ï..num_enq")

hosp_dup <- unique(hosp, by=id)
count(hosp_dup)
View(hosp_dup)

hosp$id2 <- toString(hosp$id)
str(hosp$id)

is.data.table(hosp_dup)
setDT(hosp_dup)
setDT(hosp)

hosp_dup[,SOR_ANN := NULL]

names(hosp_dup)
