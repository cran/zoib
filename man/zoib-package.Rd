\name{zoib-package}
\alias{zoib-package}

\docType{package}
\title{
Bayesian Inference for Beta Regression and Zero-or-One-Inflated Beta Regression Models
}

\description{
beta regression and zero/one inflated beta regression on data where the responses are [0,1]-bounded. Bayesian inferences of the model are obtained via the Markov Chain Monte Carlo approach implemented in JAGS.

\tabular{ll}{
Package: \tab zoib\cr
Type: \tab Package\cr
Version: \tab 1.3.1\cr
Date: \tab 2015-05-22\cr
License: \tab GPL (>= 3)\cr
}
}

\details{
zoib accommodates boundary inflations at 0 or 1 of the response variables. It models clustered and correlated responses by introducing random components into the linear predictors of the link functions. The inferences from the models are based in the Bayesian framework via the Markov Chain Monte Carlo (MCMC) approaches.  

The main function zoib() generates posterior draws on the regression coefficients, the design matrices in the link functions of the zoib regressions models, and predicted response variables. It also produces a MCMC (JAGS) model object and the posterior samples of model parameters in the "mcmc" format. The DIC of a Bayesian model can be calculated using dic.samples(JAGS.object) available in package rjags for model comparison purposes. Convergence of MCMC chains can be checked using traceplot(), autocorr.plot() and gelman.diag() available in package coda. Posterior summary of the parameters can be obtained by  summary(). zoib also contains a function check.psrf() that checks whether the multivariate psrf value can be calculated for multi-dimensional variables, provides  box plots and summary statistics on multiple univariate psrf values.
}

\author{
Fang Liu (\email{fang.liu.131@nd.edu}), with contributions from Yunchuan Kong (\email{ykong@cuhk.edu.hk})  

Maintainer: Fang Liu (\email{fang.liu.131@nd.edu})
}
\references{
Liu, F. and Li, Q. (2014). A Bayesian Model for Joint Analysis of Multivariate 
Repeated Measures and Time to Event Data in Crossover Trials, Statistical Methods in Medical Research, doi: 10.1177/0962280213519594

Liu, F. and Kong, Y. (2015). ZOIB:  an R Package for Bayesian Inferences in Beta and Zero One Inflated Beta Regression Models, The R Journal, Volume 7/2
}

\seealso{
betareg, rjags, coda
}

\keyword{ package 
}

