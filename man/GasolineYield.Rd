\name{GasolineYield}
\alias{GasolineYield}
\docType{data}
\title{
Gasoline Yields Data
}
\description{
Proportion of crude oil converted to gasoline after distillation and fractionation.
}

\usage{data(GasolineYield)}
\format{
  A data frame with 32 observations on the following 3 variables.
  \describe{
  \item{\code{yield}}{a numeric vector; proportion of crude oil converted to gasoline after distillation and fractionation.}
  \item{\code{temp}}{a numeric vector: temperature (F) at which 100\% gasoline has vaporized.}
  \item{\code{batch}}{crude oil batch ID.}}
}
\details{
The Gasoline Yields Data is a subset of the data GasolineYield in R package "betareg". Refer to the detailed description in R package "betareg".
}

\references{
Liu, F. and Kong, Y. (2015). ZOIB:  an R Package for Bayesian Inferences in Beta and Zero One Inflated Beta Regression Models, submitted

Liu, F. and Li, Q. (2014) A Bayesian Model for Joint Analysis of Multivariate  Repeated Measures and  Time to Event Data in Crossover Trials, Statistical Methods  in Medical Research, doi: 10.1177/0962280213519594

Cribari-Neto F. and Zeileis A. (2010), Beta Regression in R. Journal of Statistical Software, 34(2):1-24
}

\examples{
  \dontrun{
  library(zoib)
  data("GasolineYield", package = "zoib")
  
  #################################################
	#  fixed effects zoib with 
  #  batch as a 10-level qualitative variable
  ################################################

  eg.fixed <- zoib(yield ~ temp + as.factor(batch)| 1, data=GasolineYield,
                 joint = FALSE,  random = 0, EUID = 1:nrow(d),
                 zero.inflation = FALSE, one.inflation = FALSE,
                 n.iter = 1100, n.thin = 5, n.burn=100)
  # yieds 400 posterior draws (200 per chain) on the parameters of interest
  
  # posterior draws of the regression coefficients
  coeff <- mcmc(eg.fixed$coef)
	traceplot(coeff)
	autocorr.plot(coeff, lag.max=5)
	check.psrf(coeff)
	summ1 <- summary(coeff)
     
  # Design Matrix: Xb, Xd, Xb0, Xb1
  eg.fixed$Xb; eg.fixed$Xd;  eg.fixed$Xb0; eg.fixed$Xb1  
  
  # Posterior predictive values of Y
  ypred = rbind(eg.fixed$ypred[[1]],eg.fixed$ypred[[2]])
  post.mean= apply(ypred,2,mean); 
  plot(GasolineYield$yield, post.mean, col='blue',pch=2); 
  abline(0,1,col='red')
  
  ######################################################
  #  mixed effects zoib with batch as a random variable
  #####################################################
  eg.random <- zoib(yield ~ temp | 1 | 1, data=GasolineYield,
                  joint = FALSE, random=1, EUID=GasolineYield$batch,
                  zero.inflation = FALSE, one.inflation = FALSE,
                  n.iter=3200, n.thin=15, n.burn=200)

  # check convergence on the regression coefficients
  sample2 <- eg.random$coeff
  traceplot(sample2)
  autocorr.plot(sample2) 
  check.psrf(coeff)
  summary(mcmc(sample2))

  # predicted y 
  ypred = rbind(eg.random$ypred[[1]],eg.random$ypred[[2]])
  post.mean= apply(ypred,2,mean); 
  plot(GasolineYield$yield, post.mean, col='blue',pch=2); 
  abline(0,1,col='red')
	}
}