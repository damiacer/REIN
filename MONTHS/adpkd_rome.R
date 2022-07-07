# ADPKD DATABASE

dim(rdb)
table(rdb$apkd01)
apkd<-rdb[!(rdb$apkd01=="0"),]
dim(apkd)

#--------------------------------------------------------------------------------
# UNIVARIATE ANALYSIS 
#--------------------------------------------------------------------------------

dput(names(apkd))

#covariates <- c("URGn", "KTTINIn", "EPOINIn", 
#                "nephgp", "METHOn", "techn", "MODALn", "VAVn", "traitement", 
#                "IRCn", "O2n", "ICn", "ICOROn", "IDMn", "RYTHMn", 
#                "ANEVn", "AMIn", "AVCAITn", "KCn", "VHBn", "VHCn", "CIRHn", "VIHn", 
#                "SIDAn", "HANDn", "AMPn", "PLEGn", "CECITEn", "COMPORTn", 
#                 "TABACn", "bmi", "tabac2", 
#                "iresp", "sero", "coro", "foie", 
#                "apkd01", "TRANSP", "diabetes")

covariates <- c("cardiovasc", "dial", "AVCAITn", "sex", "age", 
                "tabac2", "diabetes", "bmi")

univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))

univ_models <- lapply(univ_formulas, function(x){coxph(x, data = apkd)})
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
# MULTIVARIATE ANALYSIS 
#--------------------------------------------------------------------------------

############ WHOLE POP

apkd.whole <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                          diabetes + bmi + 
                          sex + pspline(age, df=5) #+ pspline(bmi, df=10)
                        , data = apkd)

extractAIC(apkd.whole) # THE SECOND VALUE IS THE AIC
summary(apkd.whole)
apkd.whole.z = cox.zph(apkd.whole)

#--------------------------------------------------------------------------------

apkd.nodiab<-apkd[(apkd$diabetesMISS=="0"),] # this will exclude missing value
dim(apkd.nodiab)
dim(apkd)

apkd.whole.nodiab <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                      bmi + 
                      sex + pspline(age, df=5) #+ pspline(bmi, df=10)
                    , data = apkd)

extractAIC(apkd.whole.nodiab) # THE SECOND VALUE IS THE AIC
summary(apkd.whole.nodiab)
apkd.whole.nodiab.z = cox.zph(apkd.whole.nodiab)

#--------------------------------------------------------------------------------

############ HEMORRAGIC EVENTS ONLY

h.apkd.hem<-apkd[(apkd$ischemic=="0"),]
dim(apkd.hem)

h.apkd <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                      diabetes + bmi + 
                      sex + age #pspline(age, df=5) #+ pspline(bmi, df=10)
                    , data = h.apkd.hem)

extractAIC(h.apkd) # THE SECOND VALUE IS THE AIC
summary(h.apkd)
h.apkd.z = cox.zph(h.apkd)

#--------------------------------------------------------------------------------

h.apkd.nodiab.data<-h.apkd.hem[(h.apkd.hem$diabetesMISS=="0"),] # this will exclude missing value
dim(h.apkd.nodiab.data)
dim(apkd)

h.apkd.nodiab <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                             bmi + 
                             sex + age #pspline(age, df=5) #+ pspline(bmi, df=10)
                           , data = h.apkd.nodiab.data)

extractAIC(h.apkd.nodiab) # THE SECOND VALUE IS THE AIC
summary(h.apkd.nodiab)
h.apkd.nodiab.z = cox.zph(h.apkd.nodiab)

#--------------------------------------------------------------------------------

############ ISCHEMIC EVENTS ONLY

i.apkd<-apkd[(apkd$hemorragic=="0"),]
dim(i.apkd)

i.apkd.mod <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                  diabetes + bmi + 
                  sex + age #pspline(age, df=5) #+ pspline(bmi, df=10)
                , data = i.apkd)

extractAIC(i.apkd.mod) # THE SECOND VALUE IS THE AIC
summary(i.apkd.mod)
i.apkd.mod.z = cox.zph(i.apkd.mod)

# w/ splines

iS.apkd.mod <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                      diabetes + bmi + 
                      sex + pspline(age, df=5) #+ pspline(bmi, df=10)
                    , data = i.apkd)

extractAIC(iS.apkd.mod) # THE SECOND VALUE IS THE AIC
summary(iS.apkd.mod)
iS.apkd.mod.z = cox.zph(iS.apkd.mod)

#--------------------------------------------------------------------------------

i.apkd.nodiab<-i.apkd[(i.apkd$diabetesMISS=="0"),] # this will exclude missing value
dim(i.apkd.nodiab.data)
dim(apkd)

i.apkd.nodiab.mod <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                         bmi + 
                         sex + age #pspline(age, df=5) #+ pspline(bmi, df=10)
                       , data = i.apkd)

extractAIC(i.apkd.nodiab.mod) # THE SECOND VALUE IS THE AIC
summary(i.apkd.nodiab.mod)
i.apkd.nodiab.z = cox.zph(i.apkd.nodiab.mod)

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# IMPUTATION WITH HMISC
#install.packages("Hmisc")
library("Hmisc")

time <- i.apkd$time
status <- i.apkd$status
cardiovasc <- i.apkd$cardiovasc
tabac2<- i.apkd$tabac2
dial <- i.apkd$dial
diabetes <- i.apkd$diabetes
sex <- i.apkd$sex
bmi <- i.apkd$bmi
age <- i.apkd$age
bmic <- as.factor(i.apkd$bmic)

#x <- cbind(time, status, cardiovasc, tabac2, dial, apkd01, diabetes, sex, bmi, age)
d <- data.frame(time, status, cardiovasc, tabac2, dial, diabetes, sex, bmi, 
                age, bmic)
n <- naclus(d)
# show na patterns
plot(n)
naplot(n)
# transcan
f2 <- transcan(~status + bmi + age + sex + tabac2 + diabetes,
               n.impute = 5, shrink = FALSE, data = d)
# fit
h <- fit.mult.impute(Surv(time, status) ~ bmi #+ bmic 
                     + tabac2 + cardiovasc + dial +
                       diabetes + sex +pspline(age, df = 5)
                     , coxph, f2, data = d)
summary(h)

