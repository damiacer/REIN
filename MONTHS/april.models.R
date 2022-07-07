# MODELS == 1st MODEL (whole population, ADPKD included)
# the second group of models will run specifically on the ADPKD population 
# WHOLE POPULATION
## INDEPENDENT OF DIABETES STATUS
## ONLY DIABETES(-) 

# HEMORRAGIC EVENT ONLY
## INDEPENDENT OF DIABETES 
## ONLY DIABETES(-)

# ISCHEMIC EVENT ONLY
## INDEPENDENT OF DIABETES
## ONLY DIABETES(-)

#--------------------------------------------------------------------------------

# 1st MODEL = ADPKD + OTHER 
# LIST OF VARIABLES 
#     * cardiovascular disease (cerebral excluded) ICn ICOROn IDMn RYTHMn ANEVn
#     * tobacco tabac2
#     * dialysis method METHOn OU MODALn OU techn
#     *** cerebrovascular event (are the events different in SNDS and REIN) AVCAITn
#     * ADPKD
#     * sex sex
#     * age age

# POPULATION 
#     * complete population
#     * population after the exclusion of diabetes

#--------------------------------------------------------------------------------

# CARDIOVASCULAR DISEASE
# 1 = AT LEAST ONE CARDIOVASCULAR AFFECTION 
# 0 = NO CARDIOVASCULAR DISEASE
# CEREBRAL ISCHEMIA NOT TAKEN INTO ACCOUNT

rdb$cardiovasc = if_else(rdb$ICn + rdb$ICOROn + rdb$IDMn + rdb$RYTHMn + 
                           rdb$ANEVn > 0, "1", "0")
table(rdb$cardiovasc)
#0     1 
#20944 19243 

#-----------------------------

# DIALYSIS METHOD

rdb <- rdb %>% 
  mutate(dial = case_when(
    # HEM
    METHOn == "1" ~ "1", 
    # PERIT
    METHOn == "2" ~ "2",
  )) 

table(rdb$dial)
#1 (HEM) 2 (PERIT)
#38725   4516 

#-----------------------------

# APKD

rdb$apkd01 = if_else(rdb$nephgp == "APKD", "1", "0")
table(rdb$apkd01)
#     0     1 
# 42466  2560 

# DIABETES Y/N

# IF 0 = NO DIABETES
# IF 1 OR 2 = DIABETES TYPE 1 OR 2
# 0     1     2 
# 22943  1112 19044 

rdb$diabetes = if_else(rdb$TYPDIABn > 0, "1", "0")
table(rdb$diabetes)
#0     1 
#22943 20156

rdb$diabetesMISS <- rdb$diabetes
rdb$diabetesMISS[is.na(rdb$diabetes)] <- "miss"
prop.table(table(rdb$diabetesMISS))*100
#                0                 1          missing 
# 22943 (50.955004) 20156 (44.765247)  1927 (4.279749) 

# NOTE : I WILL USE THIS VARIABLE TO RUN THE NEW ANALYSIS ON DB PATIENTS ONLY.
# THE PATIENTS WITH MISSING VALUES FOR DIABETES WILL BE THEN EXCLUDED

#################################################################################

# PACKAGE INSTALLATION
# AND DEFINITION OF TIME AND STATUS 

#install.packages("survminer")
library("survminer")
#install.packages("survival")
library("survival")

rdb$time = rdb$epilogus
rdb$time = as.numeric(as.character(rdb$time))
library("dplyr")
rdb$status = if_else(rdb$EVENTUM == "event", "1", "0")

rdb$status = as.factor(rdb$status)
str(rdb$status)
rdb$status = as.numeric(as.character(rdb$status))

#--------------------------------------------------------------------------------

str(rdb$cardiovasc)
rdb$cardiovasc <- as.factor(rdb$cardiovasc)
str(rdb$tabac2)
rdb$tabac2 <- as.factor(rdb$tabac2)
str(rdb$dial)
rdb$dial <- as.factor(rdb$dial)
str(rdb$apkd01)
str(rdb$diabetes)
rdb$diabetes <- as.factor(rdb$diabetes)
str(rdb$bmi)
rdb$bmi <- as.numeric(as.character(rdb$bmi))
str(rdb$sex)
rdb$sex <- as.factor(rdb$sex)
str(rdb$age) 
rdb$age <- as.numeric(as.character(rdb$age))

#################################################################################

# ADPKD DATABASE

dim(rdb)
table(rdb$apkd01)
apkd<-rdb[!(rdb$apkd01=="0"),]
dim(apkd)

#################################################################################

#############################################
###### ISCHEMIC AND HEMORRAGIC EVENTS #######
#############################################

#--------------------------------------------------------------------------------
# UNIVARIATE ANALYSIS 
#--------------------------------------------------------------------------------

dput(names(rdb))

#covariates <- c("URGn", "KTTINIn", "EPOINIn", 
#                "nephgp", "METHOn", "techn", "MODALn", "VAVn", "traitement", 
#                "IRCn", "O2n", "ICn", "ICOROn", "IDMn", "RYTHMn", 
#                "ANEVn", "AMIn", "AVCAITn", "KCn", "VHBn", "VHCn", "CIRHn", "VIHn", 
#                "SIDAn", "HANDn", "AMPn", "PLEGn", "CECITEn", "COMPORTn", 
#                 "TABACn", "bmi", "tabac2", 
#                "iresp", "sero", "coro", "foie", 
#                "apkd01", "TRANSP", "diabetes")

covariates <- c("cardiovasc", "tabac2", "dial", "apkd01", "AVCAITn", "sex", "age", 
                "tabac2", "diabetes", "bmi")

univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))

univ_models <- lapply(univ_formulas, function(x){coxph(x, data = rdb)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)

#--------------------------------------------------------------------------------
# MODEL SELECTION BASED ON THE AIC  
#--------------------------------------------------------------------------------
# more https://rstudio-pubs-static.s3.amazonaws.com/2053_9979149852a84e898fccb1acfd771ba4.html
# TO RUN THIS CODE, THE EVENT AND THE TIME HAVE TO BE ALREADY DEFINED 

## Create outcome variable, then survival object
rdb.cox <- within(rdb, {
  status <- status > 0
  survival.vector    <- Surv(time, status)
})

## Create vectors for outcome and predictors
outcome    <- c("survival.vector")
predictors <- c("cardiovasc",  "tabac2", "dial", "apkd01", "sex", "age", "diabetes", "bmi")
dataset    <- rdb.cox

# --- 

## Create list of models
list.of.models <- lapply(seq_along((predictors)), function(n) {
  
  left.hand.side  <- outcome
  right.hand.side <- apply(X = combn(predictors, n), MARGIN = 2, paste, collapse = " + ")
  
  paste(left.hand.side, right.hand.side, sep = "  ~  ")
})

## Convert to a vector
vector.of.models <- unlist(list.of.models)

## Fit coxph to all models
list.of.fits <- lapply(vector.of.models, function(x) {
  
  formula    <- as.formula(x)
  fit        <- coxph(formula, data = dataset)
  result.AIC <- extractAIC(fit)
  
  data.frame(num.predictors = result.AIC[1],
             AIC            = result.AIC[2],
             model          = x)
})

## Collapse to a data frame
result <- do.call(rbind, list.of.fits)


## Sort and print
#install.packages("doBy")
#library(doBy)
orderBy(~ AIC, result)
orderBy(~ num.predictors, result)

#--------------------------------------------------------------------------------
# MODELS
#--------------------------------------------------------------------------------

# ALL POPULATION MODEL 
coxWPOP <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                  apkd01 + diabetes + bmi +
                  sex + age, data = rdb)
summary(coxWPOP)
coxWPOP.prophaz <- cox.zph(coxWPOP)
library("survminer")
ggcoxzph(coxWPOP.prophaz)

#--------------------------------------------------------------------------------

# SPLINES
# NATURAL SPLINES
#
#coxWPOP.ns <- coxph(Surv(time, status) ~ #cardiovasc + tabac2 + dial + 
                                        #apkd01 + 
                                        #sex + 
#                      ns(age, df=3), data = rdb)

#---------------------------------------

# PSP SPLINES
# NOTES: linearity test H0: relation is linear; H1: relation is not linear

coxWPOP.spline <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                          apkd01 + diabetes + bmi + 
                          sex + pspline(age, df=5) #+ pspline(bmi, df=10)
                        , data = rdb)
coxWPOP.spline
extractAIC(coxWPOP.spline) # THE SECOND VALUE IS THE AIC
summary(coxWPOP.spline)

# check of the proportionality of the model with splines
coxz.splines1 <- cox.zph(coxWPOP.spline)
coxz.splines1
ggcoxzph(coxz.splines1)

# THE HR OUTPUT
# the %*% operator: https://stackoverflow.com/questions/22060515/the-r-operator
#is.vector(coxWPOP.spline$coefficients[-1])
#is.matrix(coxWPOP.spline$coefficients[-1])
#is.vector("pspline(rdb$age)")
#is.matrix("pspline(rdb$age)")
#agespline <- pspline(rdb$age)
#dim(agespline)
#dim(coxWPOP.spline$coefficients[-1])
#summary(coxWPOP.spline)
#coxWPOP.spline$HR <- exp(agespline %*% coxWPOP.spline$coefficients[-1] -
#                   sum(coxWPOP.spline$means[-1] * coxWPOP.spline$coefficients[-1]))
#library("ggplot2")
#ggplot(coxWPOP.spline, aes(x = age, y = HR)) + geom_line()


termplot <- termplot(coxWPOP.spline, term = 8, se = TRUE, col.term = 1, col.se = 1)

ptemp <- termplot(coxWPOP.spline, se = TRUE, plot = FALSE)
attributes(ptemp)
ptemp$age[1:10,]

    # 2nd FORMULA FOR THE TERMPLOT
    # reference: https://stackoverflow.com/questions/62621381/how-can-i-plot-a-cox-model-with-pspline-and-find-the-cutoff-in-r
    # library("survival")
    # library("tidyverse")
    coxWPOP.agesp <- coxph(Surv(time, status) ~ pspline(age, df=3), data = rdb)
    coxWPOP.agesp
    extractAIC(coxWPOP.agesp) # THE SECOND VALUE IS THE AIC
    summary(coxWPOP.agesp)
    
    termplot.img = termplot(coxWPOP.agesp, term = 1, se = TRUE, plot = FALSE)
    names(termplot.img)
    
    newdf = data.frame(termplot.img$age)
    plot(x = newdf$x, y = newdf$hr, type = "l", col = "red", #xlim = c(0, 90), ylim = c(0, 2), 
         xlab = "age",
         ylab = "HR for age with cox model using psplines")
    lines(x = newdf$x, y = newdf$lci, col = "blue")
    lines(x = newdf$x, y = newdf$uci, col = "blue")
    abline(h = 1, col = "gray")

# REDRAWING THE CURVE ON LOG SCALE WITH THE AGE 5À AS THE REFERENCE 
# (THAT MEANS THAT THE RISK IS = 1 FOR AGE 50)
# IN THE PROPORTIONAL HAZARD RISK MODEL WE CAN CHOOSE WHATEVER CENTER WE LIKE 

ageterm <- ptemp$age # the data frame
center <- with(ageterm, y[x==72])
ytemp <- ageterm$y + outer(ageterm$se, c(0, -1.96, 1.96), '*')
matplot(ageterm$x, exp(ytemp - center), log='y', type = 'l', lty = c(1,2,2), col = 1,
        xlab = "Age", ylab = "Relative event rate")

# how the center value is selected?  

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# ALL POPULATION (WPOP) INCLUDING DIABETES AS COVARIABLE (DIA)

coxWPOPDIA <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                     apkd01 + #AVCAITn +
                     + diabetes +
                     sex + age, data = rdb)
summary(coxWPOPDIA)

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# EXCLUDING DIABETES(+) POPULATION


#didb<-rdb[!(rdb$diabetesMISS=="1"),] # this will include missing values
didb<-rdb[(rdb$diabetesMISS=="0"),] # this will exclude missing value
dim(didb) # 24870  = 45026 - 20156

table(rdb$status)

### UNIVARIATE ANALYSIS
covariates <- c("cardiovasc", "tabac2", "dial", "apkd01", "sex", "age",  "bmi")

univ_models <- lapply(univ_formulas, function(x){coxph(x, data = didb)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)

#--------------------------------------

coxWPOPDNEG <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                       apkd01 + bmi + 
                       sex + pspline(age, df=5), data = didb)
summary(coxWPOPDNEG)
coxWPOPDNEG.z = cox.zph(coxWPOPDNEG)
plot(coxWPOPDNEG.z)

km_fit.wpdn <- survfit(Surv(time, status) ~ apkd01, data=didb)
plot(km_fit.wpdn, xlab="Days", main = 'Kaplan Meyer Plot')

# time interaction
coxWPOPDNEGt <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                       apkd01 + apkd01:time + bmi + 
                       sex + pspline(age, df=5), data = didb)
summary(coxWPOPDNEGt)
coxWPOPDNEGtz = cox.zph(coxWPOPDNEGt)
# time interaction doesn't work 

# step function

apkd.step <- coxph(Surv(time, status) ~ apkd01 + se, data = didb)
summary(apkd.step)

apkd.stepZ <- cox.zph(apkd.step)
plot(apkd.stepZ)
# 270 et 560

#################################################################################

#############################################
########## HEMORRAGIC EVENTS ONLY ###########
#############################################

#table(rdb$ischemic)
#0     1      --> 0 means no ischemic event = hemorragic event or no event at all
#43677  1349 

hemdb<-rdb[(rdb$ischemic=="0"),]
dim(hemdb)

#--------------------------------------------------------------------------------
# UNIVARIATE ANALYSIS 
#--------------------------------------------------------------------------------

dput(names(hemdb))

#covariates <- c("URGn", "KTTINIn", "EPOINIn", 
#                "nephgp", "METHOn", "techn", "MODALn", "VAVn", "traitement", 
#                "IRCn", "O2n", "ICn", "ICOROn", "IDMn", "RYTHMn", 
#                "ANEVn", "AMIn", "AVCAITn", "KCn", "VHBn", "VHCn", "CIRHn", "VIHn", 
#                "SIDAn", "HANDn", "AMPn", "PLEGn", "CECITEn", "COMPORTn", 
#                 "TABACn", "bmi", "tabac2", 
#                "iresp", "sero", "coro", "foie", 
#                "apkd01", "TRANSP", "diabetes")

covariates <- c("cardiovasc", "tabac2", "dial", "apkd01", "sex", "age", 
                "tabac2", "diabetes", "bmi")

univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))

univ_models <- lapply(univ_formulas, function(x){coxph(x, data = hemdb)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)

#--------------------------------------------------------------------------------
# MODELS
#--------------------------------------------------------------------------------

# ALL POPULATION MODEL (ONLY HEMORRAGIC EVENTS)

coxWPOP.HE <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                   apkd01 + bmi + diabetes +
                   sex + pspline(age, df = 5), data = hemdb)
summary(coxWPOP.HE)

coxWPOP.HE.z <- cox.zph(coxWPOP.HE)

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# ALL POPULATION (WPOP) (ONLY HEMORRAGIC EVENTS),
# INCLUDING DIABETES AS COVARIABLE (DIA)

coxWPOPDIA.HE <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                      apkd01 + #AVCAITn +
                      + diabetes +
                      sex + age, data = hemdb)
summary(coxWPOPDIA.HE)

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# EXCLUDING DIABETES(+) POPULATION

#didb<-rdb[!(rdb$diabetesMISS=="1"),]
hemdb.dneg<-hemdb[(hemdb$diabetesMISS=="0"),]
count(hemdb.dneg)

coxWPOP.HEDNEG <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                       apkd01 + bmi + #AVCAITn +
                       sex + pspline(age, df = 5), data = hemdb.dneg)
summary(coxWPOP.HEDNEG)

#################################################################################

#############################################
########### ISCHEMIC EVENTS ONLY ############
#############################################

table(rdb$hemorragic)
#    0     1 
#44612   414 

ischdb<-rdb[(rdb$hemorragic=="0"),]
dim(ischdb)

#--------------------------------------------------------------------------------
# UNIVARIATE ANALYSIS 
#--------------------------------------------------------------------------------

dput(names(ischdb))

#covariates <- c("URGn", "KTTINIn", "EPOINIn", 
#                "nephgp", "METHOn", "techn", "MODALn", "VAVn", "traitement", 
#                "IRCn", "O2n", "ICn", "ICOROn", "IDMn", "RYTHMn", 
#                "ANEVn", "AMIn", "AVCAITn", "KCn", "VHBn", "VHCn", "CIRHn", "VIHn", 
#                "SIDAn", "HANDn", "AMPn", "PLEGn", "CECITEn", "COMPORTn", 
#                 "TABACn", "bmi", "tabac2", 
#                "iresp", "sero", "coro", "foie", 
#                "apkd01", "TRANSP", "diabetes")

covariates <- c("cardiovasc", "tabac2", "dial", "apkd01", "sex", "age", 
                "diabetes", "bmi")

univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))

univ_models <- lapply(univ_formulas, function(x){coxph(x, data = ischdb)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)

#--------------------------------------------------------------------------------
# MODELS
#--------------------------------------------------------------------------------

# ALL POPULATION MODEL (ONLY HEMORRAGIC EVENTS)

coxWPOP.IS <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                      apkd01 + diabetes + #AVCAITn 
                      sex + pspline(age, df = 5), data = ischdb)
summary(coxWPOP.IS)

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# ALL POPULATION (WPOP) (ONLY HEMORRAGIC EVENTS),
# INCLUDING DIABETES AS COVARIABLE (DIA)

coxWPOPDIA.IS <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                         apkd01 + #AVCAITn +
                         + diabetes +
                         sex + age, data = ischdb)
summary(coxWPOPDIA.IS)

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# EXCLUDING DIABETES(+) POPULATION

#didb<-rdb[!(rdb$diabetesMISS=="1"),]
ischdb.dneg<-hemdb[(ischdb$diabetesMISS=="0"),]
count(ischdb.dneg)

coxWPOP.ISDNEG <- coxph(Surv(time, status) ~ age + cardiovasc + tabac2 + dial + 
                          apkd01 + sex, data = ischdb.dneg)
summary(coxWPOP.ISDNEG)
