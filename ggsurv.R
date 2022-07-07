getwd()
setwd("/Users/damianocerasuolo/Desktop/PhD/M2/DATABASES_REIN/csv_data")
getwd()

retest <- read.csv2("testreader.csv", header = TRUE, na.string = "NA")
is.data.frame(retest)
View(retest)
names(retest)
str(retest$time)
	max(retest$time)
str(retest$status)
	table(retest$status)

library("survival")
library("survminer")

retest$time2 = retest$time / (365.25/12)
str(retest$time2)

linelistsurv.by = survfit(Surv(time, status) ~ 1, data = retest)
ggsurvplot(linelistsurv.by, xlim = c(0, 1350), data = retest)

survall <- survminer::ggsurvplot(
  linelistsurv.by, 
  data = retest,          
  conf.int = TRUE,              # do not show confidence interval of KM estimates
  surv.scale = "percent",        # present probabilities in the y axis in %
  size = 0.8,
  #x.scale = 365.25,
  break.time.by = 60,            # present the time axis with an increment of 10 days
  xlab = "Follow-up days",
  ylab = "Survival Probability",
  xlim = c(0,1500),
  #ylim = c(0,100),
  pval = FALSE,                   # print p-value of Log-rank test 
  pval.method = FALSE,
  pval.coord = c(40,.91),        # print p-value at these plot coordinates
  risk.table = FALSE,                # print the risk table at bottom 
  legend.title = "overall",     # legend characteristics, "class1" and "class2" most of the time
  #legend.labs = c("class1","class2"),
  cumevents = TRUE,
  font.legend = 15, 
  fontsize = 3,
  palette = "Dark2",             # color palette 
  surv.median.line = "hv",       # draw horizontal and vertical lines to the median survivals
  ggtheme = theme_light(base_size=7),        # simplify plot background
  cumcensor = FALSE,
  censorshape = "|"
)

table(retest$time)
min(retest$time)
max(retest$time)

##########################################################################

retest.random <- read.csv2("testrandom.reader.csv", header = TRUE, na.string = "NA")
names(retest.random)

linelistsurv.by = survfit(Surv(time, status) ~ 1, data = retest.random)
ggsurvplot(linelistsurv.by, xlim = c(0, 1350), data = retest.random)

randomsurvminer <-survminer::ggsurvplot(
  linelistsurv.by, 
  data = retest.random,          
  conf.int = TRUE,              # do not show confidence interval of KM estimates
  surv.scale = "percent",        # present probabilities in the y axis in %
  size = 0.8,
  #x.scale = 365.25,
  break.time.by = 60,            # present the time axis with an increment of 10 days
  xlab = "Follow-up days",
  ylab = "Survival Probability",
  xlim = c(0,1550),
  ylim = c(0,1),
  pval = FALSE,                   # print p-value of Log-rank test 
  pval.method = FALSE,
  pval.coord = c(40,.91),        # print p-value at these plot coordinates
  risk.table = FALSE,                # print the risk table at bottom 
  legend.title = "overall",     # legend characteristics, "class1" and "class2" most of the time
  #legend.labs = c("class1","class2"),
  cumevents = TRUE,
  font.legend = 7, 
  fontsize = 2.1,
  palette = "Dark2",             # color palette 
  surv.median.line = "hv",       # draw horizontal and vertical lines to the median survivals
  ggtheme = theme_light(base_size=7)        # simplify plot background
  , 
  cumcensor = FALSE,
  censorshape = "|"
)

#randomsurvminer  <- ggpar(randomsurvminer, 
#font.main = c(10),
#font.x = c(10),
#font.y = c(10),
#font.caption = c(10),
#font.legend = c(10),
#font.tickslab = c(10))

table(retest.random$time)
min(retest.random$time)
max(retest.random$time)

##########################################################################

apkd.i <- read.csv2("APKDI.csv", header = TRUE, na.string = "NA")
names(apkd.i)
is.list(apkd.i)
#apkd.il = unlist(apkd.i)
names(apkd.i)
str(apkd.i$date3)
apkd.i$date3 = as.Date(apkd.i$date3)

install.packages("incidence")
library("incidence")

str(apkd.il$date3)
i.outcome <- incidence(apkd.i$date3)
plot(i.outcome)
