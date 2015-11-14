\name{zoib}
\alias{zoib}

\title{
 Bayesian Inference for Zero/One Inflated Beta Regression
}
\description{
Description: zoib fits a zero/one inflated beta regression model and obtains the Bayesian Inference for the model via the Markov Chain Monte Carlo approach implemented in JAGS.
}
\usage{
zoib(model, data, n.response=1, zero.inflation = TRUE, one.inflation = TRUE, 
     joint = TRUE, random = 0, EUID, link.mu = "logit", link.x0 = "logit",
     link.x1 = "logit", prec.int = matrix(1e-3, n.response, 4), 
     prior.beta = matrix("DN", n.response, 4), prec.DN =  matrix(1e-3, n.response, 4),
     lambda.L2 =  matrix(1e-3, n.response, 4), lambda.L1 =  matrix(1e-3, n.response, 4),
     lambda.ARD =  matrix(1e-3, n.response, 4), prior.Sigma = "VC.unif",  
     scale.unif = 20,scale.halfcauchy = 20, 
     n.chain = 2, n.iter = 5000, n.burn =200, n.thin = 2, inits=NULL)
}
\arguments{
  \item{model}{
  Symbolic description of the model in the format of formula, such as y ~ x, y1 | y2 ~ x1+x2, 
  or y1 ~ x | z
}
  \item{data}{
  Data to be analyzed; arguments controlling formula processing via model.frame.
}
  \item{n.response}{
  number of response variables. Default is 1.
}
  \item{zero.inflation}{
  A vector of dimensional q (the number of response variables) that contains q values of TRUE or FALSE on whether each of the response variable has inflation at zero.
}
  \item{one.inflation}{
  A vector of dimensional q (the number of response variables) that contains q values of TRUE or FALSE on whether each of the response variable has inflation at one.
}
  \item{joint}{
	Whether to jointly model response variables if n.response >=2. Default is FALSE.
}
\item{random}{
	Whether the zoib model has a random component and, if so, which linear predictor. Default is 0 (no random component). 
}
  \item{EUID}{
	Listing of the experimental unit ID for each row of the data set.
}
  \item{link.mu}{
  Link function for the mean of the beta piece of the zoib model. Choices are "logit" (default), "probit" and "cloglog".
}
  \item{link.x0}{
  Link function for Pr(y=0). Choices are "logit" (default), "probit" and "cloglog".
}
  \item{link.x1}{
  Link function for Pr(y=1 | y>0). Choices are "logit" (default), "probit" and "cloglog".
}
\item{prec.int}{
   Precision parameter of the prior distributions (diffuse normal) of the intercepts in the linear predictors. Default is 0.001 in all four link functions for all response variables.
} 
  \item{prior.beta}{
  Prior choice for the regression coefficients other than the intercepts in each of the 4 link functions. Default is "diffuse normal" in all 4 link functions for all response variables. Refer to "details" for more information.
}
  \item{prec.DN}{
  Precision parameters of the normal distribution if the diffuse normal is chosen as the prior distributions of the regression coefficients in all 4 linear predictors for all response variables. Default precision is 0.001.
}
  \item{lambda.L1}{
  Scale parameter of the prior distributions of the regression coefficients in the linear predictors for all response variables if the L1-like prior is chosen.  Refer to the Liu and Kong (2015) and Bae and Mallick (2004) for details.
}
  \item{lambda.L2}{
  Scale parameter of the prior distributions of the regression coefficients in the linear predictors for all response variables if the L2-like prior is chosen.  Refer to the Liu and Kong (2015) and Bae and Mallick (2004) for details.
}
  \item{lambda.ARD}{
  Scale parameter in the prior distributions of the regression coefficients in the linear predictors for all response variables if the ARD prior is chosen. Refer to the Liu and Kong (2015) and Bae and Mallick (2004) for details.
}
  \item{prior.Sigma}{
	Prior choice for the variance (if there is a single random variable) or the covariance structure (if there are mutliple  random variables) of the random variables. The default is "VC.unif". When there is a single random variable, choose from "VC.unif" and "VC.halfcauchy". Refer to "details" for more information.
}
  \item{scale.unif}{
	Upper bound of the uniform distribution as the prior for the standard deviation of each random variable (default = 20).
}
  \item{scale.halfcauchy}{
	Scale parameter of the half-Cauchy distribution as the prior for the standard deviation of each random variable (default = 20).
}
  \item{n.chain}{
  Number of Markov chains from which posterior samples will be drawn (>=1; default = 2).
}
  \item{n.iter}{
  Number of iterations per chain in the MCMC sampling (default = 5000).
}
  \item{n.burn}{
  Burning in period of the MCMC chains (default = 200).
}
  \item{n.thin}{
	Thinning period of the MCMC chains after the burn-in (default = 5).
}  
  \item{inits}{
  optional specification of initial values for regression coefficients and variance/covariance parameters in the form of a list  (see initialization below). If omitted, initial values will be generated automatically.
}
}

\details{
*************
model
**************

When there are multiple response variables y's, each y should be separated by "|", such as "y1 | y2 | y3"  on the left hand side (LHS) of the formula. On the right side of the formula, it could include up to 5 parts in the following order: 1) fixed-effect variables xb in the link function of the mean of the beta piece; 2) fixed-effect variables  xd in the link function of the sum of the two shape parameters of the beta piece; 3) fixed-effect variables  x0 in the link function of Pr(y=0); 4) fixed-effect variables  x1 in the link function of Pr(y=1|y>0); 5) random-effect variables z. xb and xd should always be specified, even if xd contains only an intercept. If there is no zero inflation in any of the y's, then the x0 part can be omitted, similarly with x1 and the random effects part z. For example, if there are 3 response variables and 2 independent variables (xx1, xx2), and none of the y's has zero inflation, then model y1 | y2 | y3 ~ xx1 + xx2 | 1 | xx1 | xx2 implies xb = (1, xx1, xx2), xd = 1 (intercept), x0 = NULL, x1 = xx1, z = (1, xx2). If y1 has 0 inflation, and y3 has 1 inflation, and there is no random effect, model y1 | y2 | y3 ~ xx1 + xx2 | xx1 | xx1 | xx2 implies xb = (1, xx1, xx2), xd = (1, xx1), x0 = xx1, x1 = xx1. Refer to  \code{\link{Formula}} in package \pkg{\link{Formula}} for more details on the specification of terms in formula such as interaction terms, etc.

***********
random
***********\cr
Specifies whether the zoib model has any random components and which linear predictor(s) have it if so. Denote the four link functions by 

	Eqn 1. g(mu) = xb*b, where g is the link function (logit, probit or cloglog), and mu is the mean of the Beta distribution\cr 
	Eqn 2. log(eta) = xd*d, where eta is the dispersion parameter of the Beta distribution \cr
	Eqn 3. g(p0) = x0*b0, where g is the link function (logit, probit or cloglog), and p0 = Pr(y=0)\cr
	Eqn 4. g(p1) = x1*b1, where g is the link function (logit, probit or cloglog), and p1 = Pr(y=1|y>0)\cr

	then random = \cr
  0: no random effect (default); \cr
	1: only the linear predictor in eqn 1 has a random component; that is,	g(mu) = xb*b +z*gamma	\cr
	2: only the linear predictor in eqn 2 has a random component; that is,	log(eta) = xd*d+z*gamma \cr
	3: only the linear predictor in eqn 3 has a random component; that is,	g(p0) = x0*b0 +z*gamma \cr
	4: only the linear predictor in eqn 4 has a random component; that is, g(p1) = x1*b1+z*gamma \cr 
	12:  the linear predictors in equations 1 and 2 contain random component z*gamma \cr 
	13:  the linear predictors in equations 1 and 3 contain random component z*gamma \cr  
	14:  the linear predictors in equations 1 and 4 contain random component z*gamma  \cr  
	23:  the linear predictors in equations 2 and 3 contain random component z*gamma \cr  
	24:  the linear predictors in equations 2 and 4 contain random component z*gamma \cr   
	34:  the linear predictors in equations 3 and 4 contain random component z*gamma \cr   
	123:  the linear predictors in equations 1, 2 and 3 contain random component z*gamma\cr    
	134:  the linear predictors in equations 1, 3 and 4 contain random component z*gamma\cr   
	124:  the linear predictors in equations 1, 2 and 4 contain random component z*gamma\cr    
	1234:  he linear predictors in all equations contain random component z*gamma

***************
prior.beta
***************\cr
  1. "DN": Diffuse Normal  \cr
  2. "L1": L1-like shrinkage prior \cr
  3. "L2": L2-like shrinkage prior \cr  
  4. "ARD": ARD-like shrinkage prior 

**************
prior.Sigma
**************\cr
	1. "VC.unif": Sigma is diagonal, the prior for the standard deviation of each random variable follows an independent uniform distribution; \cr  
	2. "VC.halfcauchy": Sigma is diagonal, the prior for the standard deviation of each random variable follows an independent half-Cauchy distribution; \cr  
	3. "UN.unif": Sigma is of full parameterization, the prior for the standard deviation of each random variable follows an independent uniform distribution; The correlation correlations among the random effects\cr   
	4. "UN.halfcauchy". Sigma is of full parameterization, the prior for the standard deviation of each random variable follows an independent half-Cauchy distribution.
  
**************
initialization
**************\cr
specified in the following parameter order:\cr
inits = list(list(b=, d=, b0=, b1=, sigma=, R=),list(b=, d=, b0=, b1=, sigma=, R=), ...)\cr
each inner list contains the starting values for one MCMC chain. If initial values are only specified  for a subset of the parameters in a model, NULL should be used for the rest. The notations of b, d, b0, b1 are the same as in the "random" section above, with each specified in a matrix with dimention (nx+1)*ny, where ny is the number of response variables, and nx is number of regression coefficients corresponding to the respective x's (+1 refers to intercept). sigma is a vector containing the standard deviation of each random component, and R contains the lower triangle of the correlation matrix. For example, in a 3x3 correlation matrix, R is specified as c(1,r21,1,r31,r32,1).
}

\value{
\item{model}{the zoib model}
\item{MCMC.model}{the MCMC (JAGS) model object, from which samples can be drawn and DIC can be calculated}
\item{coeff}{posterior samples of regression coefficients from the zoib model}
\item{Xb}{design matrix in the link function for modeling the mean of the beta regression}
\item{Xd}{design matrix in the link function for modeling the sum of the two parameters of the beta regression}
\item{X0}{design matrix in the link function for modeling Pr(y=0)}
\item{X1}{design matrix in the link function for modeling Pr(y=1|y>0)}
\item{yobs}{observed Y}
\item{ypred}{posterior predictive samples of Y}
\item{resid}{residual}
\item{resid.std}{standardized residual}
}



\references{
Liu, F. and Kong, Y. (2015). ZOIB:  an R Package for Bayesian Inferences in Beta and Zero One Inflated Beta Regression Models, The R Journal, Volume 7/2 

Liu, F. and Li, Q. (2014) A Bayesian Model for Joint Analysis of Multivariate  Repeated Measures and Time to Event Data in Crossover Trials, Statistical Methods  in Medical Research, doi: 10.1177/0962280213519594
}

\author{
Fang Liu (\email{fang.liu.131@nd.edu})
}

\examples{
  #\dontrun{
    # refer to data sets GasolineYield, BiRepeated, and AlcoholUse in package zoib
    # for examples on fixed effect models, mixed effects models, joint modeling 
    # of bivariate beta variables with repeated measures, and modelling clustered 
    # beta variables with inflation at 0 using zoib
  #}
}
