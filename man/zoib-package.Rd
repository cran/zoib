\name{zoib-package}
\alias{zoib-package}

\docType{package}
\title{
Bayesian Inference For Zero/One-Inflated Beta Regression Model
}
\description{
The package fits a zero/one inflated beta (zoib) regression model to data sets where the response variables take values from  (0,1), [0,1), (0,1] or [0,1]. The inference of the model is obtained in the Bayesian framework via the Markov Chain Monte Carlo approach implemented in JAGS
}
\details{
\tabular{ll}{
Package: \tab zoib\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-04-21\cr
License: \tab GPL (>= 3)\cr
}
Package zoib accommodates boundary inflations at 0 or 1 of the response variables. It models clustered and correlated responses by introducing random components into the linear predictors of the link functions. The inferences from the models are based in the Bayesian framework via the Markov Chain Monte Carlo approaches.  

The main function zoib() produces a MCMC (JAGS) model objec and the posterior samples of model parameters in the "mcmc" format. The DIC of a Bayesian model can be calculated using dic.samples(JAGS.object) available in package rjags for model comparison purposes. Convergence of MCMC chains can be checked using traceplot(), autocorr.plot() and gelman.diag() available in package coda. Posterior summary of the parameters can be obtained by  summary(). The package also contains a function check.psrf( ) that checks whether the multivariate psrf value can be calculated for multi-dimensional variables, provides  box plots and summary statistics on multiple univariate psrf values.
}

\author{
Fang Liu (fang.liu.131@nd.edu) 
Yunchuan Kong (s1155014473@cuhk.edu.hk)  

Maintainer: Fang Liu (fang.liu.131@nd.edu)
}
\references{
Liu, F. and Li, Q. (2014). A Bayesian Model for Joint Analysis of Multivariate 
Repeated Measures and Time to Event Data in Crossover Trials, Statistical Methods in Medical Research, doi: 10.1177/0962280213519594

Liu, F. and Kong, Y. (2014). ZOIB:  an R Package for Bayesian Inference for Zero/One Inflated Beta Regression Model, submitted
}

\seealso{
betareg, rjags, coda
}

\keyword{ package 
}

