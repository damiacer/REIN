library(survival)
#install.packages("spBayesSurv")
library(spBayesSurv)
#install.packages("spBayes")
library(spBayes)

#----------------------------------------------------------------------------------------
# Power Generalised Weibull
# https://rpubs.com/FJRubio/PGW

# BAYESIAN GH
# https://rpubs.com/FJRubio/BGH
#----------------------------------------------------------------------------------------

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

#####################################################################################
# Data preparation

#----------------------------------------------------------------------------------------------
# Example: Leukemia data
#----------------------------------------------------------------------------------------------

rbd.s = subset(rdb, select = c("time", "status", "age", "sex", "cardiovasc", "dial", "apkd01"))
head(rbd.s)
rbd.sdf = as.data.frame(rbd.s)

data(LeukSurv)
?LeukSurv
head(LeukSurv)

n <- dim(LeukSurv)[1]  # number of individuals: 1043
n <- dim(rbd.sdf)[1]

# Design matrices
#des <- as.matrix(cbind(scale(LeukSurv$age), LeukSurv$sex, scale(LeukSurv$wbc), scale(LeukSurv$tpi) ))
# vars in the cox model cardiovasc + tabac2 + dial + apkd01 + bmic + sex + age
des <- as.matrix(cbind(scale(rbd.sdf$age), rbd.sdf$sex, rbd.sdf$cardiovasc, 
                       rbd.sdf$dial, rbd.sdf$apkd01))
# colnames(des) <- cbind("std age", "sex", "wbc", "tpi")
colnames(des) <- cbind("std age", "sex", "cardiovas", "dial", "apkd01")
# des_t <- as.matrix(cbind(scale(LeukSurv$age)))
des_t <- as.matrix(cbind(scale(rbd.sdf$age)))

#hist(LeukSurv$time)
hist(rbd.sdf$time)

p0 <- dim(des_t)[2]
p1 <- dim(des)[2]

# Required quantities
status <- as.logical(rbd.sdf$status)
times <- as.vector(rbd.sdf$time)/365.24 # in years
times.obs <- times[status]

######################################################################################
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
set.seed(1234)
chain <- adaptMetropGibbs(ltd=log.post, starting=inits, accept.rate=0.44, batch=n.batch, 
                          batch.length=batch.length, report=100, verbose=FALSE)$p.theta.samples

# # Burning and thinning the chain 
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

plot(survfit(Surv(times, status) ~ 1, data = LeukSurv), 
     xlab = "Time", 
     ylab = "Marginal survival probability")
curve(msurv,0,14, ylim = c(0,1), lty = 1, lwd = 2, add = T, col = "blue")
legend("topright",  legend = c("GH Model", "Kaplan-Meier"), col = c("blue","black"), lwd = c(2,2) )

#----------------------------------------------------------------------------------------



