# DATABASE: HOSPITALISATIONS

install.packages("data.table")
library("data.table")

View(hosp)
names(hosp)

hosp <- as_tibble(hosp)
hosp <- hosp %>% rename(
  # new name = old name,
  "id" = "ï..num_enq")

hosp_dup <- hosp

hosp$id <- as.character.factor(hosp$id)


hosp_dup <- unique(hosp_dup, by=SOR_ANN)
count(hosp_dup)
View(hosp_dup)

View(hosp)
