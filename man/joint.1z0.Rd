\name{joint.1z0}
\alias{joint.1z0}

\title{
Jointly modelling of multiple variables taking values from [0,1) when there is a single random variable in the linear predictors of the link functions
}
\description{
Internal function to be called by function zoib. Jointly models multiple [0,1)-bounded variables with inflation at 0  when there is a single random variable in  the linear predictors of the link function
}

\usage{
joint.1z0(y, n, q, xmu.1, p.xmu, xsum.1, p.xsum, x0.1, p.x0, inflate0, rid, EUID, 
nEU, prior1, prior2, prior.beta, prior.Sigma, prec.int, prec.DN, lambda.L1, 
lambda.L2, lambda.ARD, scale.unif, scale.halft, link, n.chain,inits, seed)
}
\arguments{
  \item{y}{
  >=2 response variables taking value from [0, 1).
}
  \item{n}{
  Number of rows in the data set.
}
  \item{q}{
	Number of response variables
}
   \item{xmu.1}{
   Design matrix associated with the fixed effects in the linear predictor of g(mean of the beta piece), where g() is a link function
}
  \item{p.xmu}{
   Number of columns in xmu.1.
}
  \item{xsum.1}{
   Design matrix associated with fixed effects in linear predictor of the log(dispersion parameter of the beta piece).
}
  \item{p.xsum}{
   Number of columns in xsum.1.
}
  \item{x0.1}{
   Design matrix associated with the fixed effects in the linear predictor of g(Pr(y=0)), where g() is a link function.
}
  \item{p.x0}{
   Number of columns in x0.1.
}
 \item{inflate0}{  
  A vector containing information on which response variables have inflation at 0.
}
  \item{rid}{
  A vector containing information on which linear predictor of a random component.
}
  \item{EUID}{
	Listing of the experimental unit ID for each row of the data set
}
  \item{nEU}{
  Number of experimental units 
}
  \item{prior1}{
  A vector containing information on the prior for the regression coefficients in each of the 4 linear predictors of the 4 link functions.
}
  \item{prior2}{
  A matrix containing information on the priorfor the covariance structure of the random variables.
}
  \item{prior.beta}{
   Prior choice for the regression coefficients in each of the 4 link functions (a vector of dim = 4).
}
  \item{prior.Sigma}{
  Prior choice for the Covariance structure of the random variables.
}
  \item{prec.int}{
  The precision in the prior distributions (diffuse normal) of the intercepts in the linear predictors.
}
  \item{prec.DN}{
  The precision in the prior distributions of the regression coefficients in the linear predictors if diffuse normal prior is chosen.
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
  \item{scale.unif}{
  The upper bound of the uniform distribution for the standard deviation of each random variable
}
  \item{scale.halft}{
	The scale parameter of the half-Cauchy distribution for the standard deviation of each random variable
}
 \item{link}{
  A vector containing the information on the choice of link function for the mean of the beta piece.
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