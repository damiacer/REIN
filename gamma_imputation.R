install.packages("InformativeCensoring")
library("InformativeCensoring")

gammaRE <- rdb[1:500,]
head(gammaRE)

set.seed(21051986)

# specifying gamma_i
# gamma_i = gamma[i] x gamma.factor

gammaRE$basegamma <- 1
gammaRE$basegamma[]
