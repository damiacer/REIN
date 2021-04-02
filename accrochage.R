# THE CORRESPONDENCE DATABASE

View(rein_s)
names(rein_s)

rein_s <- as_tibble(rein_s)
rein_s <- rein_s %>% rename(
  # new name = old name,
  "id" = "ï..num_enq")