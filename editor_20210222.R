getwd()
setwd("P:/UBRC_M2/REYES/ANALYSIS/DATABASES/csv_data") # ON PC

################################################################################

# PACKAGES

library("tidyverse")
library("dplyr")
library("expss")
library("here")
library("gapminder")

################################################################################

# SOURCES
## DATA MANAGEMENT ON TIDYVERSE
### https://dplyr.tidyverse.org/reference/index.html
### https://stats.idre.ucla.edu/stat/data/rdm/data_management_seminar.html
### https://stackoverflow.com/questions/28873057/sum-across-multiple-columns-with-dplyr

################################################################################

# DATABASE 1: REGISTRE REIN
## WARNING#1: DATABASE SAVED FROM SAS7DBAT. UFT-8 CODING NOT AVAILABLE.
## WARNING#2: SPECIAL CHARACTERS ARE NOT CORRECTLY DISPLAYED.
rein <- read.csv2("rein_db.csv", header = TRUE, na.string="")
count(rein)

#-------------------------------------------------------------------------------

# RENAME COLUMNS
rein <- as_tibble(rein)
rein <- rein %>% rename(
    # new name = old name,
    "prttturg" = "ï..URGn")

#-------------------------------------------------------------------------------

# ADD LABELS TO VARIABLES
rein = apply_labels(rein,
                    prttturg = "Premier traitement en urgence",
                    KTTINIn = "1ère séance d'hémodialyse réalisée avec cathéter",
                    EPOINIn = "Traitement par Erythropoietine",
                    liste_longue = "Regroupement détaillé néphropathie",
                    nephgp = "Regroupement en 8 classes néphropathie",
                    METHOn = "Traitement 3 classes",
                    techn = "Méthode de traitement",
                    MODALn = "Modalité de traitement",
                    VAVn = "Voie d'abord vasculaire",
                    traitement = "Traitement (concaténation TECHN et MODAL)",
                    PDS = "Poids",
                    TAIL = "Taille",
                    IRCn = "Insuffisance respiratoire chronique",          
                    O2n = "Oxygénothérapie",   
                    ICn = "Insuffisance cardiaque",
                    ICOROn = "Insuffisance coronarienne",
                    IDMn = "Infarctus du myocarde",
                    RYTHMn = "Troubles du rythme",
                    ANEVn = "Anevrysme de l'aorte abdominale",
                    AMIn = "Artérite des membres inférieurs",
                    AVCAITn = "Variable composite de AVC et AIT",
                    KCn = "Cancer évolutif",
                    VHBn = "Ag HBS positif",
                    VHCn = "PCR VHC positif", 
                    CIRHn = "Cirrhose",
                    VIHn = "VIH",
                    SIDAn = "SIDA",
                    HANDn = "Au moins un handicap",
                    AMPn = "Amputation membres inférieurs",
                    PLEGn = "Paraplégie/Hémiplégie",
                    CECITEn = "Cécité",
                    COMPORTn = "Troubles du comportement",
                    TYPDIABn = "Type de diabète",
                    STADICn = "Stade de l'insuffisance cardiaque",
                    STDAMIn = "Stade de l'artérite des membres inférieurs",
                    STDCIRHn = "Stade de la cirrrhose",
                    TABACn = "Statut tabagique 0-1-2",
                    bmi = "IMC",
                    tabac2 = "Statut tabagique 0-1",   
                    iresp = "Variable composite de O2 et IRC",   
                    sero = "Variable composite de VIH et SIDA",
                    coro = "Variable composite de ICORO et IDM",
                    foie = "VariablSe composite de CIRH, VHB, VHC",
                    comCV = "Nb de comorbidités  cardiovasc sur 6 chez les patients avec TOUTES LES VAR RENSEIGNEES",
                    comcvcl = "Au moins une comorbidité cardiovasculaire",
                    comcvcl2 = "Nb de comorbidités cardiovasculaires en 3 classes sur 6 comorbidités",
                    sex = "Sexe",
                    age = "Age à l'initiation du traitement de suppléance",    
                    ETAT_DERNOUV2019 = "Etat aux dernières nouvelles avant 31/12/2019",
                    delai_IRT = "Délai insuffisance rénale terminale",
                    delai_DC = "Délai décès",
                    delai_TX = "",
                    delai_SVR = "Délai de sevrage (par récupération de la fonc rénale, soit en fin d vie)",     
                    delai_PDV = "Délai de perdue de vue",       
                    delai_DERNOUV2019 = "Délai dernières nouvelles 2019",
                    groupes6 = "Regroupement causes de décès 6 groupes", 
                    categories18 = "Regroupement causes de décès 18 groupes",
                    groupes6_CA1 = "Regroupement causes de décès associée 1 en 6 groupes",
                    categories18_CA1 = "Regroupement causes de décès associée 1 en 18 groupes",
                    groupes6_CA2 = "Regroupement causes de décès associée 2 en 6 groupes",
                    categories18_CA2 = "Regroupement causes de décès associée 2 en 18 groupes",
                    MOTIF_An = "Motif d'arrêt de la dialyse",
                    CPKMEDn = "Fin de traitement pour complication médicale",
                    REFUSn = "Fin de traitement par refus du patient",
                    DDC = "Date de décès",
                    DINSCMED = "Date de la première inscription sur la liste transplantation",
                    DDIRT = "Date de l’insuffisance rénale terminale",
                    DGRF = "Date de greffe",          
                    DSVR = "Date de sevrage (par récupération de la fonc rénale, soit en fin d vie)",
                    DPDV = "Date de perdu de vue",
                    DATE_DERNOUV2019 = "Date de dernières nouvelles 2019",
                    RREC_COD_ANO = "Code anonym"
)

# RECALL LABELS (BY VAR NAME OR VAR POSITION)
var_lab(rein[11])

################################################################################

# DATABASE 2: TREATMENT CHANGE DURING THE FOLLOW-UP
switch <- read.csv2("rein_treatswitch.csv", header = TRUE, na.string="NA")
count(switch)

switch <- switch %>% rename(
    # new name = old name,
    "tmethod" = "ï..METHOn")

# switch %>% select(tmethod, DDTT, RREC_COD_ANO) %>% mutate(across(!tmethod, as.factor))
# switch$DDTT <- as.factor(switch$DDTT)
# switch$RREC_COD_ANO <- as.factor(switch$RREC_COD_ANO)

switch <- as_tibble(switch)
switch2 <- switch %>% pivot_wider(names_from = DDTT, values_from = tmethod, 
                                  values_fn = length,
                                  # WARNING#1: VALUES ARE NOT UNIQUELY IDENTIFIER 
                                  # WARNING#2 : OUTPUT WILL CONTAIN LIST-COL
                                  # NOTE: VALUES_FN WILL ONLY SUPPRESS THE WARNING MESSAGE
                                  # values_fn = list, 
                                  values_fill = 0)
count(switch2)
ncol(switch2)
sapply(switch2, class)
sapply(switch2, mode)

# all_dates <- factor(2:74) %>% print()
# as.numeric(as.character(all_dates))

#-------------------------------------------------------------------------------

# CREATE A VARIABLE FOR THE TREATMENT CHANGE

colsumfun <- function(df) {
                      require(dplyr)
                      y <- select_if(df, is_numeric)
                      rowSums(y, na.rm=T)
}

switch2$changes <- colsumfun(switch2)
table(switch2$changes)

#https://tidyr.tidyverse.org/reference/expand.html
switch2$all_dates <- factor(2:43329) 
switch2 %>% expand(all_dates)
