# rstanarm

install.packages("rstanarm")
library("rstanarm")

install.packages("devtools")
library("devtools")
devtools::install_github("stan-dev/rstanarm", build_vignettes = FALSE)

# MODEL

mod1 <- stan_surv(formula = Surv(time, status) ~ 1,
                  data = reinc2.f,
                  chains = CHAINS,
                  cores = CORES,
                  seed = SEED,
                  iter = ITER)

# BayesSurvival


install.packages("BayesSurvival")
library("BayesSurvival")
BayesSurv(
          reinc2.f,
          time = "time",
          event = "status",
          prior = c("Independent"),
          K = ceiling((dim(reinc2.f)[1]/log(dim(reinc2.f)[1]))^(1/2)),
          time.max = max(reinc2.f[[time]]),
          alpha = 0.05,
          N = 1000,
          alpha.dep = 1,
          alpha0.dep = 1.5,
          beta0.dep = 1,
          alpha.indep = 1,
          surv.factor = 10,
          surv.epsilon = 1e-10
          )
