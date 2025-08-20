\name{zoib}
\alias{zoib}

\title{
 Bayesian Inference for Zero/One Inflated Beta Regression
}
\description{
Description: zoib fits a zero/one inflated beta regression model and obtains the Bayesian Inference for the model via the Markov Chain Monte Carlo approach implemented in JAGS.
}
\usage{
zoib(model, data, n.response=1, joint = TRUE,
     zero.inflation = TRUE, one.inflation = TRUE, 
     random = 0, EUID, link.mu = "logit", link.x0 = "logit",
     link.x1 = "logit", prec.int = matrix(1e-3, n.response, 4), 
     prior.beta = matrix("DN", n.response, 4), 
     prec.DN =  matrix(1e-3, n.response, 4),
     lambda.L2 =  matrix(1e-3, n.response, 4), 
     lambda.L1 =  matrix(1e-3, n.response, 4),
     lambda.ARD =  matrix(1e-3, n.response, 4), prior.Sigma = "VC.unif",  
     scale.unif = 20,scale.halfcauchy = 20,     
     n.chain = 2, n.iter = 5000, n.burn =200, n.thin = 2, inits=NULL, seeds=NULL)
}
\arguments{
  \item{model}{
  Symbolic description of the model in the format of formula, such as y ~ x, y1 | y2 ~ x1+x2, 
  or y1 ~ x | z. Refer to "details" for more information.
}
  \item{data}{
  Data to be analyzed. 
}
  \item{n.response}{
  Number of response variables. Default is 1.
}
  \item{joint}{
  Whether to jointly model multiple responses if n.response >=2. Default is TRUE.
}
  \item{zero.inflation}{
  A vector that contains n.response values of TRUE or FALSE on whether each of the response variables has inflation at zero. Default is TRUE.
}
  \item{one.inflation}{
  A vector that contains n.response values of TRUE or FALSE on whether each of the response variables has inflation at one. Default is TRUE.
}
  \item{random}{
	Sepecifys which linear predictor(s) or link function(s) contain a random component. Default is 0 (no random component). Refer to "details" for more information.
}
  \item{EUID}{
	Listing of the experimental unit ID in each row of the data set.
}
  \item{link.mu}{
  Link function for the mean of the beta distribution piece of the zoib model. Choices are "logit" (default), "probit" and "cloglog".
}
  \item{link.x0}{
  Link function for Pr(y=0). Choices are "logit" (default), "probit" and "cloglog".
}
  \item{link.x1}{
  Link function for Pr(y=1 | y>0). Choices are "logit" (default), "probit" and "cloglog".
}
\item{prec.int}{
   Precision parameter of the prior distribution (diffuse normal) of the intercept in the linear predictor of each link function. Default is 0.001 in all 4 link functions for all response variables.
} 
  \item{prior.beta}{
  Prior choice for the regression coefficients other than the intercepts in each of the 4 link functions. Default is "diffuse normal" in all 4 link functions for all response variables. Refer to "details" for more information.
}
  \item{prec.DN}{
  Precision parameters of the normal distribution if the diffuse normal prior is chosen as the prior distributions of the regression coefficients in all 4 linear predictors for all response variables. Default precision is 0.001.
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
	Prior choice for the variance or the variance-covariance in the case of a single random variable and multiple random variables, respectively. The default is "VC.unif". When there is a single random variable, choose from "VC.unif" and "VC.halfcauchy". Refer to "details" for more information.
}
  \item{scale.unif}{
	Upper bound of the uniform prior for the standard deviation of each random variable  if prior.Sigma="VC.unif" is specified. Default is 20.
}
  \item{scale.halfcauchy}{
	Scale parameter of the half-Cauchy prior for the standard deviation of each random variable if prior.Sigma="VC.halfCauchy" is specified.  Default is 20.
}
  \item{n.chain}{
  Number of Markov chains from which posterior samples will be drawn (>=1; default = 2).
}
  \item{n.iter}{
  Number of iterations per chain in the MCMC sampling (default = 5000) before burning-in and thinning.
}
  \item{n.burn}{
  Burning in period of the MCMC chains (default = 200).
}
  \item{n.thin}{
	Thinning period of the MCMC chains after the burn-in (default = 5).
}  
  \item{inits}{
  optional specification of initial values for regression coefficients and variance/covariance parameters in the form of a list  (see initialization below). If omitted, initial values will be generated automatically. Refer to "details" for more information.
}
 \item{seeds}{
  a vector of dimension n.chain that contains seeds for the initial values and the random number generators of the MCMC chains, if users wish to make the output from the model reproducible.
}  
}

\details{
**************
model
**************\cr
When there are multiple responses y, each y should be separated by "|", such as "y1 | y2 | y3"  on the left hand side of the formula. On the right side of the formula, it can include up to 5 parts in the following order: xb | xd | x0 | x1 | z, where xb represents the fixed-effects covariates/factors in the link function of the mean of the beta distribution, xd represents  the fixed-effects covariates/factors in the link function of the sum of the two shape parameters of the beta distribution, x0 represents the fixed-effect covariates/factors in the link function of Pr(y=0), x1 represents the fixed-effect covariates/factors in the link function of Pr(y=1|y>0), and z represents the random-effect covariates/factors. The current version of the package only accomodates z being the same across all the link functions that have a random component. xb and xd should always be specified, even if xd contains only an intercept. If there is no zero inflation in any of the y's, then the x0 part can be omitted, and zoib automatically adjusts the order of the rest of the X specifications; similarly for the x1 part and the random effects part z. 

For example, if there are 3 response variables and 2 independent variables (xx1, xx2), and none of the y's have zero inflation, and all have one inflation, then specification of  y1 | y2 | y3 ~ xx1 + xx2 | 1 | xx1 | xx2, together with zero.inflation = c(F,F,F) and one.inflation = c(T,T,T), implies xb = (1, xx1, xx2), xd = 1 (intercept), x0 = NULL, x1 = (1, xx1) for all y's, z = (1, xx2). If y1 has inflation at 0, and y3 has inflation at 1, and there is no random effect, model y1 | y2 | y3 ~ xx1 + xx2 | xx1 | xx1 | xx2, together with zero.inflation = c(T,F,F) and one.inflation=c(F,F,T), implies xb = (1, xx1, xx2), xd = (1, xx1), x0 = (1, xx1) for y1, x1 = (1, xx2) for y3.  Refer to   \code{\link[Formula:as.Formula]{as.Formula}()} for more details on the specification of terms in formula such as interaction terms and categorical x's, etc.

*************
random
*************\cr
Denote the four link functions by 

	Eqn 1. g(mu) = xb*b, where g is the link function (logit, probit or cloglog), and mu is the mean of the Beta distribution\cr 
	Eqn 2. log(eta) = xd*d, where eta is the sum of the two shape parameters of the Beta distribution \cr
	Eqn 3. g(p0) = x0*b0, where g is the link function (logit, probit or cloglog), and p0 = Pr(y=0)\cr
	Eqn 4. g(p1) = x1*b1, where g is the link function (logit, probit or cloglog), and p1 = Pr(y=1|y>0)\cr
	then random = \cr
  0: no random effect (default); \cr
	1: only the linear predictor in Eqn 1 has a random component; that is,	g(mu) = xb*b +z*gamma	\cr
	2: only the linear predictor in Eqn 2 has a random component; that is,	log(eta) = xd*d+z*gamma \cr
	3: only the linear predictor in Eqn 3 has a random component; that is,	g(p0) = x0*b0 +z*gamma \cr
	4: only the linear predictor in Eqn 4 has a random component; that is,  g(p1) = x1*b1+z*gamma \cr 
	12: the linear predictors in Eqns 1 and 2 contain random component z*gamma \cr 
	13: the linear predictors in Eqns 1 and 3 contain random component z*gamma \cr  
	14: the linear predictors in Eqns 1 and 4 contain random component z*gamma  \cr  
	23: the linear predictors in Eqns 2 and 3 contain random component z*gamma \cr  
	24: the linear predictors in Eqns 2 and 4 contain random component z*gamma \cr   
	34: the linear predictors in Eqns 3 and 4 contain random component z*gamma \cr   
	123: the linear predictors in Eqns 1, 2 and 3 contain random component z*gamma\cr    
	134: the linear predictors in Eqns 1, 3 and 4 contain random component z*gamma\cr   
	124: the linear predictors in Eqn 1, 2 and 4 contain random component z*gamma\cr    
	1234: the linear predictors in all Eqns contain random component z*gamma

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
	1. "VC.unif": Sigma is diagonal, the prior for the standard deviation of each random variable follows a uniform distribution; \cr  
	2. "VC.halfcauchy": Sigma is diagonal, the prior for the standard deviation of each random variable follows a half-Cauchy distribution  with a large scale parameter; \cr  
	3. "UN.unif": Sigma is fully parameterized, the prior for the standard deviation of each random variable follows a uniform distribution; each element in the correlation matrix follows an independent unif(0,1) distribution with necessary constraints to ensure positive definiteness. \cr   
	4. "UN.halfcauchy". Sigma is fully parameterized, the prior for the standard deviation of each random variable follows an half-Cauchy distribution; each element in the correlation matrix follows an independent unif(0,1) distribution with necessary constraints to ensure positive definiteness.
  
  
**************
inits
**************\cr
If supplied, should be in the following parameter order:\cr
inits = list(list(b=, d=, b0=, b1=, sigma=, R=),list(b=, d=, b0=, b1=, sigma=, R=), ...)\cr
The notations b, d, b0, b1 are the same as in the "random" section above, with each specified in a matrix format of dimension (nx+1)*n.response, where nx is number of regression coefficients corresponding to the respective x's (+1 because of the intercept). sigma is a vector containing the standard deviation of each random component, and R contains the lowe.r triangle of the correlation matrix. For example, in a 3x3 correlation matrix, R is specified as c(1,r21,1,r31,r32,1). Each inner list contains the starting values for one MCMC chain. If initial values are specified only for a subset of the parameters in a model, NULL should be used for the rest of the unspecified parameters (whose initial values will be generated automatically by the function).
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
Liu, F. and Kong, Y. (2015). ZOIB:  an R Package for Bayesian Inferences in Beta and Zero One Inflated Beta Regression Models, The R Journal, 7(2):34-51 

Liu, F. and Li, Q. (2014) A Bayesian Model for Joint Analysis of Multivariate  Repeated Measures and Time to Event Data in Crossover Trials, Statistical Methods  in Medical Research, doi: 10.1177/0962280213519594

Liu, F. and Eugenio, E.(2016). A review and comparison of Bayesian and likelihood-based inferences in beta regression and zero-or-one-inflated beta regression,
atistical Methods in Medical Research, DOI: 10.1177/0962280216650699
}

\author{
Fang Liu (\email{fang.liu.131@nd.edu})
}

\examples{
  \dontrun{
    #refer to data sets GasolineYield, BiRepeated, and AlcoholUse in package zoib
    #for examples on fixed effect models, mixed effects models, joint modeling 
    #of bivariate beta variables with repeated measures, and modelling clustered 
    #beta variables with inflation at 0 using zoib
  }
}
