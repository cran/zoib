# example 1: GasolineYield
library(zoib)
data("GasolineYield", package = "zoib")

# zoib: fixed
eg.fixed <- zoib(yield ~ temp + as.factor(batch)| 1, data=GasolineYield,
                 joint = FALSE,  random = 0, EUID = 1:nrow(GasolineYield),
                 zero.inflation = FALSE, one.inflation = FALSE,
                 n.iter = 11, n.thin = 2, n.burn=1, seeds=c(1,2,3),n.chain=3)
sample1 <- eg.fixed$coeff
#dic.samples(eg.fixed $MCMC.model, n.iter=51,thin=2,n.burn=1)

# zoib: random
eg.random <- zoib(yield ~ temp | 1 | 1, data=GasolineYield,
                  joint = FALSE, random=1, EUID=GasolineYield$batch,
                  zero.inflation = FALSE, one.inflation = FALSE,
                  n.iter=11, n.thin=2, n.burn=1)
sample2 <- eg.random$coeff

