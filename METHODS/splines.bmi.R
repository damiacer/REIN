table(rdb$bmi)

str(rdb$bmi)
rdb$bmi.v <- rdb$bmi
rdb$bmi.v[is.na(rdb$bmi)] <- "0"
is.na(rdb$bmi.v)
str(rdb$bmi.v)
rdb$bmi.v = as.numeric(as.character(rdb$bmi.v))

rdb.bmi <- rdb[!(rdb$bmi.v=="0"),]
dim(rdb)
# 45026   110
dim(rdb.bmi)
# 38382   110

coxWPOP.bmi <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                   apkd01 + diabetes + bmi +
                   sex + pspline(age, df = 2), data = rdb.bmi)
summary(coxWPOP.bmi)
coxWPOP.bmi <- cox.zph(coxWPOP.bmi)

#--------------------------------------------------------------------------------
# BMI BY CLASSES

# < 18.5 UNDERWEIGHT
# 18.5-24.9 NORMAL WEIGHT
# 25.0-29.9 PRE-OBESITY
# 30.0-34.9 OBESITY CLASS I
# 35.0-39.9 OBESITY CLASS II
# > 40.0 OBESITY CLASS III

mean(rdb$bmi, na.rm = TRUE)

library(dplyr)
rdb <- rdb %>% 
  mutate(bmic = case_when(
    bmi < 18.5 ~ "1", 
    bmi >= 18.5 & bmi < 25 ~ "2", 
    bmi >= 25.0 & bmi < 30 ~ "3",
    bmi > 30 ~ "4",
  )) 

rdb$bmicm <- rdb$bmic
rdb$bmicm[is.na(rdb$bmic)] <- "99"
table(rdb$bmic)
#     1     2     3     4 
#  1618 15100 12249  9404 
table(rdb$bmicm)

#--------------------------------------------------------------------------------

coxWPOP.bmic <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                       apkd01 + diabetes +
                       sex + bmi + strata(bmic) + pspline(age, df = 5), data = rdb)
summary(coxWPOP.bmic)
coxWPOP.bmicz <- cox.zph(coxWPOP.bmic)
coxWPOP.bmicz

#--------------------------------------------------------------------------------

# BMI SMOOTH SPLINES 

coxWPOP.bmismooth <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                             apkd01 + diabetes +
                             sex + pspline(age, df = 5)
                           + pspline(bmi, df= 8), data = rdb)
summary(coxWPOP.bmismooth)
coxWPOP.bmismooth.zph <- cox.zph(coxWPOP.bmismooth)

#--------------------------------------------------------------------------------

# MISSING VALUES FOR THE VARIABLES INCLUDED IN THE MODEL

rdb.missing = subset(rdb, select = c(cardiovasc, tabac2, dial, apkd01, diabetes, sex, bmi, age))
dim(rdb.missing)

rdb.missing2 = subset(rdb, select = c(status, bmi))


# definition of the function to study the proportion of missing values
pMiss <- function(x){sum(is.na(x))/length(x)*100}

apply(rdb.missing,2,pMiss)  #columns
#cardiovasc     tabac2       dial     apkd01   diabetes        sex        bmi        age 
#10.747124  18.682539   3.964376   0.000000   4.279749   0.000000  14.755919   0.000000 
#apply(rdb.missing,1,pMiss) #lines

# is data missing at random?
install.packages("naniar")
library("naniar")
mcar_test(rdb.missing2)

#-------------------------

install.packages("mice")
library("mice")

install.packages("grid")
library("grid")
install.packages("VIM")
library("VIM")
library(VIM)

# DEFINING THE DATABASE FOR THE MISSING DATA IMPUTATION
rdb.IMP = subset(rdb, select = c(num_enq, cardiovasc, tabac2, dial, apkd01, sex, age, diabetes, bmi,
                    status, time))

rdb.IMP$cardiovasc <- as.factor(rdb.IMP$cardiovasc)
rdb.IMP$tabac2 <- as.factor(rdb.IMP$tabac2)
rdb.IMP$apkd01 <- as.factor(rdb.IMP$apkd01)
rdb.IMP$bmi <- as.numeric(as.character(rdb.IMP$bmi))
rdb.IMP$sex <- as.factor(rdb.IMP$sex)
rdb.IMP$age <- as.numeric(as.character(rdb.IMP$age))
rdb.IMP$diabetes <- as.factor(rdb.IMP$diabetes)
rdb.IMP$status <- as.factor(rdb.IMP$status)
rdb.IMP$time <- as.numeric(as.character(rdb.IMP$time))

md.pattern(rdb.IMP)
# for the smaller dataset
#       sex age dial diabetes  bmi      
#36356   1   1    1        1    1     0
#6420    1   1    1        1    0     1
#246     1   1    1        0    1     1
#219     1   1    1        0    0     2
#323     1   1    0        1    1     1
#1457    1   1    0        0    1     2
#5       1   1    0        0    0     3
#0   0 1785     1927 6644 10356

# for the dataset including all the predictors
#num_enq apkd01 sex age time status dial diabetes cardiovasc  bmi tabac2      
#28994       1      1   1   1    1      1    1        1          1    1      1     0
#4962        1      1   1   1    1      1    1        1          1    1      0     1
#4597        1      1   1   1    1      1    1        1          1    0      1     1
#1017        1      1   1   1    1      1    1        1          1    0      0     2
#1354        1      1   1   1    1      1    1        1          0    1      1     1
#681         1      1   1   1    1      1    1        1          0    1      0     2
#340         1      1   1   1    1      1    1        1          0    0      1     2
#421         1      1   1   1    1      1    1        1          0    0      0     3
#137         1      1   1   1    1      1    1        0          1    1      1     1
#39          1      1   1   1    1      1    1        0          1    1      0     2
#35          1      1   1   1    1      1    1        0          1    0      1     2
#14          1      1   1   1    1      1    1        0          1    0      0     3
#16          1      1   1   1    1      1    1        0          0    1      1     2
#53          1      1   1   1    1      1    1        0          0    1      0     3
#6           1      1   1   1    1      1    1        0          0    0      1     3
#163         1      1   1   1    1      1    1        0          0    0      0     4
#140         1      1   1   1    1      1    0        1          0    1      1     2
#172         1      1   1   1    1      1    0        1          0    1      0     3
#615         1      1   1   1    1      1    0        0          0    1      1     3
#816         1      1   1   1    1      1    0        0          0    1      0     4
#5           1      1   1   1    1      1    0        0          0    0      0     5
#310         1      1   1   1    1      0    1        1          1    1      1     1
#44          1      1   1   1    1      0    1        1          1    1      0     2
#34          1      1   1   1    1      0    1        1          1    0      1     2
#3           1      1   1   1    1      0    1        1          1    0      0     3
#8           1      1   1   1    1      0    1        1          0    1      1     2
#3           1      1   1   1    1      0    1        1          0    1      0     3
#4           1      1   1   1    1      0    1        1          0    0      1     3
#4           1      1   1   1    1      0    1        1          0    0      0     4
#1           1      1   1   1    1      0    1        0          1    1      1     2
#1           1      1   1   1    1      0    1        0          0    0      0     5
#7           1      1   1   1    1      0    0        1          0    1      1     3
#4           1      1   1   1    1      0    0        1          0    1      0     4
#16          1      1   1   1    1      0    0        0          0    1      1     4
#10          1      1   1   1    1      0    0        0          0    1      0     5
#0      0   0   0    0    449 1785     1927       4839 6644   8412 24056

aggr(rdb.IMP, delimiter = NULL, plot = TRUE)
#plot(x = rdb.missing, 
#     col = c("red"),
#     numbers = FALSE,
#     prop = FALSE, 
#     varheight = FALSE,
#     only.miss = FALSE, 
#     border = par("fg"),
#)

# The red box plot on the left shows the distribution of var X 
# the blue box plot shows the distribution of the remaining datapoints
# Likewhise for the red box plots at the bottom of the graph
# if our assumption of MCAR data is correct, then we expect the 
# red and blue box plots to be very similar

marginplot(rdb.IMP[c(4,3)])

# imputing data

temp.rdb.IMP <- mice(rdb.IMP,m=5,maxit=50,meth='pmm',seed=500)

# m=5 refers to the number of imputed datasets. Five is the default value
# meth='pmm' refers to the imputation method
# pmm predictive mean matching as imputation method
# methods(mice) for a list of the available imputation methods under 'mice'
summary(temp.rdb.IMP)

temp.rdb.IMP$imp$dial
temp.rdb.IMP$meth

# choose one of the complete data
# 1st option
completedData <- complete(temp.rdb.IMP
                         , "all") 
rdb.compl <- completedData
is.list(rdb.compl)


# second option
rdb.compl <- complete(temp.rdb.IMP,1) # it can be 1, 2, 3, 4 or 5

# data viz 
View(rdb.compl)
dim(rdb.compl)

# pooling the data 
temp.rdb.IMPm = as.mira(temp.rdb.IMP)
rdb.pool = pool(rdb.compl, dfcom = NULL, rule = "rubin1987")

dim(rdb.test)
dim(rdb)
str(rdb$bmi)
rdb$bmi <- as.numeric(as.character(rdb$bmi))
mean(rdb$bmi, na.rm = T)

rdb.test <- complete(rdb.compl, include = TRUE)
dim(rdb.test) 
View(rdb.test)
mean(rdb.test$bmi)

# is data MCAR? 

rdb.test2c = subset(rdb.compl, select = c(status, bmi))
mcar_test(rdb.compl)
mcar_test(rdb.test2c)

#------

coximpute = with(temp.rdb.IMP, coxph(Surv(time, status==1) ~ age + tabac2 + diabetes))
summary(coximpute)
summary(pool(coximpute))

#--------------------------------------------------------------------------------

# SPLINES AFTER IMPUTATION
# IMPUTED DATASET; temp.rdb.IMP (OR "rdb.compl" IF ONLY THE FIRST ONE IS TO IMPUTE)

rdb.compl$status = as.numeric(as.character(rdb.compl$status))
rdb.compl$age = as.numeric(as.character(rdb.compl$age))
coxWPOP.bmiS <- coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                        apkd01 + diabetes +
                        sex + pspline(bmi, df = 2) + pspline(age, df = 2), 
                        data = rdb.compl)
summary(coxWPOP.bmiS)
coxWPOP.bmicz <- cox.zph(coxWPOP.bmiS)
coxWPOP.bmicz

fit <- with(data = rdb.compl, exp = coxph(Surv, status) ~ bmi)
dfcom <- df.residual(fit)
summary(pool(fit))
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
install.packages("Hmisc")
library("Hmisc")

time <- rdb$time
status <- rdb$status
cardiovasc <- rdb$cardiovasc
tabac2<- rdb$tabac2
dial <- rdb$dial
apkd01 <- rdb$apkd01
diabetes <- rdb$diabetes
sex <- rdb$sex
bmi <- rdb$bmi
age <- rdb$age
bmic <- as.factor(rdb$bmic)

#x <- cbind(time, status, cardiovasc, tabac2, dial, apkd01, diabetes, sex, bmi, age)
d <- data.frame(time, status, cardiovasc, tabac2, dial, apkd01, diabetes, sex, bmi, age, bmic)
n <- naclus(d)
# show na patterns
plot(n)
naplot(n)
# transcan
f2 <- transcan(~status + bmi + age + sex,
              n.impute = 5, shrink = FALSE, data = d)
# fit
h <- fit.mult.impute(Surv(time, status) ~ bmi + bmic + tabac2 + cardiovasc + dial +
                       apkd01 + diabetes + sex + pspline(age, df = 5), coxph, f, data = d)
summary(h)

hs2 <- fit.mult.impute(Surv(time, status) ~ pspline(bmi, df = 5) + tabac2 + cardiovasc + dial +
                        apkd01 + diabetes + sex + pspline(age, df = 5), coxph, f2, data = d)
summary(hs)


#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
fit <- fit.mult.impute(coxph(Surv(time, status) ~ cardiovasc + tabac2 + dial + 
                               apkd01 + diabetes +
                               sex + pspline(bmi, df = 2) + pspline(age, df = 2),
                             data = rdb.compl))

#--------------------------------------------------------------------------------

# perform the nearest value imputation

install.packages("NNMIS")
library("NNMIS")
