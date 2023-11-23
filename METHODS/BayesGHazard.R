# DO NOT RUN THIS: rm(list = ls())

# Required packages
library(survival)
library(spBayesSurv)
library(spBayes)

#-Functions------------------------------------------------------------------------------

# Power Generalised Weibull
# https://rpubs.com/FJRubio/PGW
# and
# https://rpubs.com/FJRubio/GHSimulacrum


# PGW Probability Density Function
dpgw <- function(t, sigma, nu, gamma, log = FALSE){
  val <- log(nu) - log(gamma) - nu*log(sigma) + (nu-1)*log(t) + 
    (1/gamma - 1)*log( 1 + (t/sigma)^nu ) + 
    ( 1 - ( 1 + (t/sigma)^nu )^(1/gamma) )
  if(log) return(val) else return(exp(val))
}

# PGW Survival Function
spgw <- function(t, sigma, nu, gamma, log.p = FALSE){
  val <- 1 - ( 1 + (t/sigma)^nu )^(1/gamma)
  if(log.p) return(val) else return(exp(val))
}

# PGW Hazard Function
hpgw <- function(t, sigma, nu, gamma, log = FALSE){
  val <- log(nu) - log(gamma) - nu*log(sigma) + (nu-1)*log(t) + 
    (1/gamma - 1)*log( 1 + (t/sigma)^nu )
  if(log) return(val) else return(exp(val))
}

# PGW Cumulative Hazard Function
chpgw <- function(t, sigma, nu, gamma){
  val <- -1 + ( 1 + (t/sigma)^nu )^(1/gamma)
  return(val) 
}

# Random Number Generation Function PGW distribution
rpgw <- function(n, sigma, nu, gamma){
  p <- runif(n)
  out <- sigma*(  ( 1 - log(1-p) )^gamma - 1 )^(1/nu)
  return(as.vector(out))
}

# Quantile Function
qpgw <- function(p, sigma, nu, gamma){
  out <- sigma*(  ( 1 - log(1-p) )^gamma - 1 )^(1/nu)
  return(out)
}

#-Data preparation-----------------------------------------------------------------------

rrdb = subset(rdb, select = c("time", "status", "age", "sex", "cardiovasc", "dial", "apkd01"))
rrdbfr = as.data.frame(rrdb)
lapply(rrdbfr[, 1:6], class)
#rrdbfr = na.omit(rrdbfr)
#rrdbfr = rrdbfr[sample(1:nrow(rrdbfr), size = 4000),]

# excluding patients with time = 0
rrdbfr <- rrdbfr[!(rrdbfr$time < 10),] #### PATIENTS WITH A FOLLOW UP < 10 = EXCLUDED

#-Normal time variable to test the model 
#rrdbfr$time = rnorm(4000, mean = 120, sd = 20)
#-End of the Normal time variable

hist(rrdbfr$time)
quantile(rrdbfr$time, probs = seq(0, 1, 0.1))
#0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 0    4  362  373  723  728 1083 1091 1450 1460 1825 
table(rrdbfr$time)
max(rrdbfr$time)
# creating a class time variable based on quantiles 

rrdbfr <- rrdbfr %>%
  mutate(timecl = case_when(
    time <= 364 ~ "1",
    time > 364 & time <= 727 ~ "2",
    time > 727 & time <= 1087 ~ "3",
    time > 1087 & time <= 1453 ~ "4", # ans
    time > 1453 ~ "5"
  ))
table(rrdbfr$timecl, useNA = "always")
#1     2     3     4     5  <NA> 
#4107 11935  7434  7921  7525     0 
table(rrdbfr$time, rrdbfr$timecl)

# create a normally distributed variable to replace the time
# the qunorms

#install.packages("dyngen")
require("dyngen")

q1 <- rnorm_bounded(38922, mean = 182, sd = 80, min = 10, max = 364)
q2 <- rnorm_bounded(38922, mean = 545, sd = 160, min = 364, max = 727)
q3 <- rnorm_bounded(38922, mean = 907.5, sd = 320, min = 728, max = 1087)
q4 <- rnorm_bounded(38922, mean = 1635.5, sd = 640, min = 1088, max = 1453)
q5 <- rnorm_bounded(38922, mean = 1639, sd = 1280, min = 1453, max = 1825)

# followup time 
# drop wrong colums if needed
# drop <- c("q1", "q2", "q3", "q4", "q5")
# rrdbfr = rrdbfr[,!(names(rrdbfr) %in% drop)]

rrdbfr <- rrdbfr %>%
  add_column(q1 = q1,
             q2 = q2,
             q3 = q3,
             q4 = q4,
             q5 = q5)

rrdbfr <- rrdbfr %>% 
  mutate(timenom = case_when(
    timecl == "1" ~ q1, 
    timecl == "2" ~ q2,
    timecl == "3" ~ q3,
    timecl == "4" ~ q4,
    timecl == "5" ~ q5
  ))
min(rrdbfr$timenom)
max(rrdbfr$timenom)
mean(rrdbfr$timenom)
min(rrdbfr$time)
max(rrdbfr$time)
mean(rrdbfr$time)

# convertion to factor
rrdbfr$timenom = as.integer(rrdbfr$timenom)
rrdbfr$status = as.integer(rrdbfr$status)
rrdbfr$age = as.integer(rrdbfr$age)
rrdbfr$sex = as.numeric(as.character(rrdbfr$sex))
rrdbfr$cardiovasc = as.numeric(as.character(rrdbfr$cardiovasc))
rrdbfr$dial = as.numeric(as.character(rrdbfr$dial))
rrdbfr$apkd01 = as.numeric(as.character(rrdbfr$apkd01))

n <- dim(rrdbfr)[1]  # number of individuals

# Design matrices
des <- as.matrix(cbind(scale(rrdbfr$age), rrdbfr$sex, rrdbfr$apkd01))
colnames(des) <- cbind("std age", "sex", "apkd01")
des_t <- as.matrix(cbind(scale(rrdbfr$age)))

hist(rrdbfr$timenom)

p0 <- dim(des_t)[2]
p1 <- dim(des)[2]

# Required quantities
status <- as.logical(rrdbfr$status)
times <- as.vector(rrdbfr$timenom)/365.24 # in years
times.obs <- times[status]

#-Posterior-sampling---------------------------------------------------------------------
# Log posterior
# Reparameterised (all positive parameters are transformed into the real line using logs)

log.post <- function(par){
  ae0 <- exp(par[1]); be0 <- exp(par[2]);  ce0 <- exp(par[3]); alpha <- par[4:(3+p0)]; beta <- par[(4+p0):(3+p0+p1)]
  exp.x.alpha <- as.vector(exp(des_t%*%alpha))
  exp.x.beta <- as.vector(exp(des%*%beta))
  exp.x.beta.dif <- as.vector(exp( des%*%beta - des_t%*%alpha ))
  exp.x.alpha.obs <- exp.x.alpha[status]
  exp.x.beta.obs <- exp.x.beta[status]
  haz0 <-  hpgw(times.obs*exp.x.alpha.obs,ae0,be0,ce0)*exp.x.beta.obs
  log.lik <-  sum(log(haz0)) - sum(chpgw(times*exp.x.alpha,ae0,be0,ce0)*exp.x.beta.dif)
  log.prior <- dgamma(ae0, 0.001, 0.001) + dgamma(be0, 0.001, 0.001) + dgamma(ce0, 0.001, 0.001) +
    sum(dnorm(alpha, 0, 10, log = TRUE)) + sum(dnorm(beta, 0, 10, log = TRUE))
  return(log.lik + log.prior)
}

# initial point
inits <- runif(3+p0+p1)

n.batch <- 5500
batch.length <- 10

# Running an adaptive Metropolis within Gibbs sampler
set.seed(20304)
chain <- adaptMetropGibbs(ltd=log.post, starting=inits, accept.rate=0.44, batch=n.batch, 
                          batch.length=batch.length, report=50000, verbose=FALSE)$p.theta.samples

## Burning and thinning the chain 
burn <- 5000
thin <- 50
NS <- n.batch*batch.length
index <- seq(burn,NS,thin)


# Chain after burn in and thinning
chainp <- chain[index,]
chainp[,1:3] <- exp(chainp[,1:3])

# Posterior samples (after thinning and burn-in period)
apply(chainp,2,summary)

# Trace plots (after thinning and burn-in period)
par(mfrow = c(3,3))
for(i in 1:ncol(chainp)) plot(chainp[,i], type = "l", ylab = paste("Parameter",i))
par(mfrow=c(1,1))

# Histograms (after thinning and burn-in period)
par(mfrow = c(3,3))
for(i in 1:ncol(chainp)) hist(chainp[,i], probability = T, 
                              main = paste("Parameter",i), xlab = paste("Parameter",i))
par(mfrow=c(1,1))

# Estimated marginal population survival function using NMC posterior samples
NMC = 1000

# Marginal survival function
msurv <- Vectorize(function(t){
  temp <- vector()
  for(j in 1:NMC){
    exp.x.alphap <- as.vector(exp(des_t%*%chainp[j,3]))
    exp.x.beta.difp <- as.vector(exp( des%*%chainp[j,(3+p0):(2+p0+p1)] - des_t%*%chainp[j,3] ))
    temp[j] <- mean(exp(- chpgw(t*exp.x.alphap, chainp[j,1], chainp[j,2], chainp[j,3])*exp.x.beta.difp)) 
  }
  
  return(mean(temp))
})

# Comparison with the Kaplan Meier estimator

plot(survfit(Surv(times, status) ~ 1, data = rrdbfr), 
     xlab = "Time", 
     ylab = "Marginal survival probability")
curve(msurv,0,14, ylim = c(0,1), lty = 1, lwd = 2, add = T, col = "blue")
legend("topright",  legend = c("GH Model", "Kaplan-Meier"), col = c("blue","black"), lwd = c(2,2) )

