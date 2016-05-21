\name{zoib-package}
\alias{zoib-package}

\docType{package}
\title{
Bayesian Inference for Beta Regression and Zero-or-One-Inflated Beta Regression Models
}

\description{
beta regression and zero/one inflated beta regression on data with [0,1]-bounded responses. Bayesian inferences of the model are obtained via Markov Chain Monte Carlo sampling.

\tabular{ll}{
Package: \tab zoib\cr
Type: \tab Package\cr
Version: \tab 1.3.4\cr
Date: \tab 2016-05-20\cr
License: \tab GPL (>= 3)\cr
}
}

\details{
zoib accommodates boundary inflations at 0 or 1 of the response variables. It models clustered and correlated responses by introducing random components into the linear predictors of the link functions. The inferences from the models are based in the Bayesian framework via the Markov Chain Monte Carlo (MCMC) approaches.  

The main function zoib() generates posterior samples on the regression coefficients, the design matrices in the link functions of the zoib regressions models, and predicted responses. It also produces a MCMC  model object(JAGS) and posterior samples of the model parameters in the "mcmc" format. The DIC of the implemented Bayesian model can be calculated using dic.samples(JAGS.object) available in package rjags for model comparison purposes. Convergence of MCMC chains can be checked using functions traceplot(), autocorr.plot() and gelman.diag() available in package coda on the posterior draws from the MCMC algorithms. zoib also contains a function check.psrf() that checks whether the multivariate psrf value can be calculated for multi-dimensional variables, provides  box plots and summary statistics on multiple univariate psrf values. Posterior summary of the parameters can be obtained using summary(). 
}

\author{
Fang Liu (\email{fang.liu.131@nd.edu}), with contributions from Yunchuan Kong  

Maintainer: Fang Liu (\email{fang.liu.131@nd.edu})
}
\references{
Liu, F. and Li, Q. (2014). A Bayesian Model for Joint Analysis of Multivariate 
Repeated Measures and Time to Event Data in Crossover Trials, Statistical Methods in Medical Research, doi: 10.1177/0962280213519594

Liu, F. and Kong, Y. (2015). ZOIB:  an R Package for Bayesian Inferences in Beta and Zero One Inflated Beta Regression Models, The R Journal, 7(2):34-51
}

\seealso{
betareg, rjags, coda
}

\keyword{ package 
}

