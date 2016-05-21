\name{fixed}
\alias{fixed}

\title{
Fixed-effects beta regression with no inflation at 0 or 1 
}
\description{
Internal function to be called by function zoib; Fits a fixed-effects beta regression to a response variable bounded within (0, 1).
}
\usage{
fixed(y, n, xmu.1, p.xmu, xsum.1, p.xsum, prior1, prec.int, prec.DN, lambda.L1,
lambda.L2, lambda.ARD, link, n.chain,inits, seed)
}
\arguments{
  \item{y}{
   A univariate response variable taking value from (0, 1).
}
  \item{n}{
   Number of rows in the data set.
}
  \item{xmu.1}{
   Design matrix associated with the  fixed effects in linear predictor of g(mean of the beta piece), where g() is link function.
}
  \item{p.xmu}{
   Number of columns in xmu.1.
}
  \item{xsum.1}{
   Design matrix associated with the fixed effects in linear predictor of log(dispersion parameter of the beta piece).
}
  \item{p.xsum}{
   Number of columns in xsum.1.
}
  \item{prior1}{
   Internally generated variable (a vector of dimension 4).  Prior choice for the regression coefficients in each of the 4 linear predictors of the 4 link functions.
}
  \item{prec.int}{
  The precision parameter in the prior distributions (diffuse normal) of the intercepts in the linear predictors.
}
  \item{prec.DN}{
  The precision parmeter in the prior distributions of the regression coefficients in the linear predictors if the diffuse normal prior is chosen.
}
  \item{lambda.ARD}{
  The scale parameter in the prior distributions of the regression coefficients in the linear predictors if the ARD prior is chosen.
}
  \item{lambda.L1}{
  The scale parameter in the prior distributions of the regression coefficients in the linear predictors if the L1-like prior is chosen.
}
  \item{lambda.L2}{
  The scale parameter in the prior distributions of the regression coefficients in the linear predictors if the L2-like prior is chosen.
}
  \item{link}{
   Internally generated variable containing the information on the choice of link functions for the mean of the beta piece.
}
  \item{n.chain}{
   Number of chains for the MCMC sampling.
}
  \item{inits}{
  initial parameter for model parameters.
}
  \item{seed}{
  seeds for results reproducibility
}
}


\value{
  Internal function. Returned values are used internally
}


\author{
Fang Liu (fang.liu.131@nd.edu)
}

\seealso{
  See Also as \code{\link{zoib}}
}

\keyword{
  internal
}

