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
This is a subset of the data GasolineYield in R package "betareg". Refer to the detailed description in R package "betareg".
}

\references{
Liu, F. and Kong, Y. (2014). ZOIB:  an R Package for Bayesian Inferences in Zero One Inflated Beta Regression Model, submitted

Liu, F. and Li, Q. (2014) A Bayesian Model for Joint Analysis of Multivariate  Repeated Measures and  Time to Event Data in Crossover Trials, Statistical Methods  in Medical Research, doi: 10.1177/0962280213519594

Cribari-Neto F. and Zeileis A. (2010), Beta Regression in R. Journal of Statistical Software, 34(2):1-24
}

\examples{
  #\dontrun{
  data("GasolineYield", package = "zoib")
  GasolineYield$batch <- as.factor(GasolineYield$batch)
  
	# fixed effects zoib with batch treated as a 10-level qualitative variable
	eg.fixed <- zoib(yield ~ temp + as.factor(batch)| 1, data=GasolineYield,
  joint = FALSE,  random = 0, EUID = 1:nrow(GasolineYield),
  zero.inflation = FALSE, one.inflation = FALSE,
  n.iter = 100, n.thin = 5)
	post.sample<- eg.fixed$oripara 
	traceplot(post.sample)
	autocorr.plot(post.sample)
	gelman.diag(post.sample)
	summary(post.sample)
	#}
}