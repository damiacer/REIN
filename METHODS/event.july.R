# DEFINITION OF THE EVENT FOR THE REIN DATABASE (AFTER THE MERGE)

# FIRST STEP: WHEN DGN_PAL IS MISSING
rdb$DGN_PAL[is.na(rdb$DGN_PAL)] <- "E0"

rdb <- rdb %>% 
  mutate(DGN_PAL = case_when(
    # missing
    DGN_PAL == "E0" ~ "E0", 
    # embolie et thrombose artérielles
    DGN_PAL == "I740" | DGN_PAL == "I741" | DGN_PAL == "I742"  | DGN_PAL == "I743" | 
      DGN_PAL == "I744" | DGN_PAL == "I745" | DGN_PAL == "I748" ~ "E0",
    # accidentes ischémiques cérébraux transitoires et syndromes apparentés
    DGN_PAL == "G450" | DGN_PAL == "G451" | DGN_PAL == "G452" | DGN_PAL == "G453" | DGN_PAL == "G454" |
      DGN_PAL == "G458" | DGN_PAL == "G459" ~ "G45",
    DGN_PAL == "G460" | DGN_PAL == "G462" | DGN_PAL == "G463" | DGN_PAL == "G464" | DGN_PAL == "G465" |
      DGN_PAL == "G466" | DGN_PAL == "G467" | DGN_PAL == "G468" ~ "G46", 
    # hémiplégie
    DGN_PAL == "G810" | DGN_PAL == "G8100" | DGN_PAL == "G8101" | DGN_PAL == "G8108" ~ "G81",
    # anomalies du champ visuels
    DGN_PAL == "H534" ~ "H53",
    # hémorragie sous-arachnoïdienne
    DGN_PAL == "I600" | DGN_PAL == "I601" | DGN_PAL == "I602" | DGN_PAL == "I603" | DGN_PAL == "I604" |
      DGN_PAL == "I605" | DGN_PAL == "I606" | DGN_PAL == "I607" | DGN_PAL == "I608" | DGN_PAL == "I609" ~ "I60",
    # hémorragie intracérébrale
    DGN_PAL == "I610" | DGN_PAL == "I611" | DGN_PAL == "I612" | DGN_PAL == "I613" | DGN_PAL == "I614" |
      DGN_PAL == "I615" | DGN_PAL == "I616" | DGN_PAL == "I618" | DGN_PAL == "I619" ~ "I61",
    # infarctus cérébral 
    DGN_PAL == "I630" | DGN_PAL == "I631" | DGN_PAL == "I632" | DGN_PAL == "I633" | DGN_PAL == "I634" | 
      DGN_PAL == "I635" | DGN_PAL == "I636" | DGN_PAL == "I6308" | DGN_PAL == "I639" ~ "I63",
    # accident vasculaire cérébral, non précisé comme étant hémorragique ou par infarctus
    DGN_PAL == "I64" ~ "I64",
    # dysphasie et aphasie
    DGN_PAL == "R470" | DGN_PAL == "R4700" | DGN_PAL == "R4701" | DGN_PAL == "R4702" | DGN_PAL == "R4703" |
      DGN_PAL == "R471" | DGN_PAL == "R478" ~ "R47"
  ))
