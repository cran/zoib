\name{zoib}
\alias{zoib}

\title{
 Bayesian Inference for Zero/One Inflated Beta Regression
}
\description{
Description: zoib fits a zero/one inflated beta regression model and obtains the Bayesian Inference for the model via the Markov Chain Monte Carlo approach implemented in JAGS.
}
\usage{
zoib(model, data, zero.inflation = TRUE, one.inflation = TRUE, joint = TRUE, 
random = 0, EUID, link.mu = "logit", link.x0 = "logit", link.x1 = "logit", 
prior.beta = rep("DN",4), prec.int = 0.001, prec.DN = 0.001, lambda.L2 = 0.001,
lambda.L1 = 0.001, lambda.ARD = 0.001, prior.Sigma = "VC.halft", scale.unif = 20,
scale.halft = 20, n.chain = 2, n.iter = 5000, n.thin = 2)
}
\arguments{
  \item{model}{
  Symbolic description of the model in the format of formula, such as y ~ x, y1|y2 ~ x1+x2, or y1 ~ x | z; refer to "Details" for more information.
}
  \item{data}{
  Data set to be analyzed; arguments controlling formula processing via model.frame.
}
  \item{zero.inflation}{
  A vector of dimensional q (the number of response variables) that contains q values of TRUE or FALSE on whether each of the response varaible has inflation at zero.
}
  \item{one.inflation}{
  A vector of dimensional q (the number of response variables) that contains q values of TRUE or FALSE on whether each of the response varaible has inflation at one.
}
  \item{joint}{
	Whether to jointly model response variables if q>=2. Default is FALSE.
}
\item{random}{
	Whether the zoib model has a random component and, if so, which linear predictor. Default is 0 (no random componen).  Refer to "details" for more infomration.
}
  \item{EUID}{
	Listing of the the experimental unit ID for each row of the data set.
}
  \item{link.mu}{
  Link function for the mean of the beta piece of the zoib model. Choices are "logit" (default), "probit" and "cloglog".
}
  \item{link.x0}{
  Link function for Pr(y=0). Choices are "logit" (default), "probit" and "cloglog".
}
  \item{link.x1}{
  Link function for Pr(y=1). Choices are "logit" (default), "probit" and "cloglog".
}
  \item{prior.beta}{
  Prior choice for the regression coefficients other than the intercepts in each of the 4 link functions (a vector of dim = 4). Default is rep("DN",4) (DN stands for "diffuse normal"). Refer to "details" for more infomration.
}
  \item{prec.int}{
   Precision parameter of the prior distributions (diffuse normal) of the intercepts in the linear predictors. Default is 0.001.
}
  \item{prec.DN}{
  Precision parameter of the normal distribution if the diffuse normal is chosen as the prior distributions of the regression coefficients in the linear predictors. Default precision is 0.001.
}
  \item{lambda.L1}{
  Scale parameter of the prior distributions of the regression coefficients in the linear predictors if the L1-like prior is chosen.  Refer to the Liu and Kong (2014) and Bae and Mallick (2004) for details.
}
  \item{lambda.L2}{
  Scale parameter of the prior distributions of the regression coefficients in the linear predictors if the L2-like prior is chosen.  Refer to the Liu and Kong (2014) and Bae and Mallick (2004) for details.
}
  \item{lambda.ARD}{
  Scale parameter in the prior distributions of the regression coefficients in the linear predictors  if the ARD prior is chosen. Refer to the Liu and Kong (2014) and Bae and Mallick (2004) for details.
}
  \item{prior.Sigma}{
	Prior choice for the variance (if there is a single random variable) or the covariance structure (if there are mutliple  random varibales) of the random variables. The default is "VC.halft". When there is a single random variable, choose from "VC.unif" and "VC.halft". Refer to "details" for more infomration.
}
  \item{scale.unif}{
	Upper bound of the uniform distribution as the prior for the standard deviation of each random variable (default = 20).
}
  \item{scale.halft}{
	Scale parameter of the half-Cauchy distribution as the prior for the standard deviation of eachrandom variable (default = 20).
}
  \item{n.chain}{
  Number of Markov chains from which posterior samples will be drawn (>=1; default = 2).
}
  \item{n.iter}{
  Number of iterations per chain in the MCMC sampling (default = 5000).
}
  \item{n.thin}{
	Thinning period of the MCMC chains (default = 5).
}
}

\details{
*************

model

**************

When there are multiple response variables y's, each y should be separated with with "|", such as "y1 | y2 | y3""  on the left hand side (LHS) of the formula. On the right side of the formula, it could include as up to 5 parts, in the following order: 1) fixed-effect variables xmu in the link function of the mean of the beta piece; 2) fixed-effect variables  xsum in the link funciton of the sum of the two shape parameters of the beta piece; 3) fixed-effect variables  x0 in the link function of Prob(y=0); 4) fixed-effect variables  x1 in the link function of Prob(y=1); 5) random-effects variables z. The first two part xmu and xsum should always be specified, even when xsum is just an intercept. If there is no zero inflation in any of the y's, then the x0 part can be omitted, similarly with the x1 part, and the random effects part z.
 
For example, if there are 3 response variables and 2 indepedent variables (xx1, xx2), and none of the y's has zero inflation, then the model y1 | y2 | y3 ~ xx1 + xx2 | 1 | xx1 | xx2 implies xmu = (1, xx1, xx2), xsum = 1 (intercept), x0 = NULL, x1 = xx1, z = (1, xx2). If y1 has 0-inflation, and y3 has 1 inflation, and there is no random effect, the model y1 | y2 | y3 ~ xx1 + xx2 | xx1 | xx1 | xx2 implies xmu = (1, xx1, xx2), xsum = (1 ,xx1), x0 = xx1, x1 = xx1.

Refer to  \code{\link{Formula}} in package \pkg{\link{Formula}} for more details on the specification of terms in formula such as interaction terms, etc.


***********

random

***********

Whether the zoib model has a random component and, if so, which linear predictor. Default is 0 (no random componen). Denote the four link functions by 

	Eqn 1. g(mu) = xmu*beta1, where g is the link function (logit, probit or cloglog), and mu is the mean of the beta piece
  
	Eqn 2. log(eta) = xsum*beta2, where eta is the dispersion parameter of the beta piece
  
	Eqn 3. g(p0) = x0*beta3, where g is the link function (logit, probit or cloglog), and p0 is the Pr(y=0)
  
	Eqn 4. g(p1) = x1*beta4, where g is the link function (logit, probit or cloglog), and p1 is the Pr(y=1)

	random = 
	0: no random effect; 

	1: only the linear predictor in eqn 1 has a random component:
	g(mu) = xmu*beta1 +z*gamma
	
	2: only the linear predictor in eqn 2 has a random component:
	log(eta) = xsum*beta2+z*gamma

	3: only the linear predictor in eqn 3 has a random component:
	g(p0) = x0*beta3 +z*gamma

	4: only the  linear predictor in eqn 4 has a random component:
	logit(p1) = x1*beta4+z*gamma

	12:  the linear predictors in equations 1 and 2 contain the random component z*gamma
  
	13:  the linear predictors in equations 1 and 3 contain the random component z*gamma
  
	14:  the linear predictors in equations 1 and 4 contain the random component z*gamma
  
	23:  the linear predictors in equations 2 and 3 contain the random component z*gamma
  
	24:  the linear predictors in equations 2 and 4 contain the random component z*gamma
  
	34:  the linear predictors in equations 3 and 4 contain the random component z*gamma
  
	123:  the linear predictors in equations 1, 2 and 3 contain the random component z*gamma
  
	134:  the linear predictors in equations 1, 3 and 4 contain the random component z*gamma
  
	124:  the linear predictors in equations 1, 2 and 4 contain the random component z*gamma
  
	1234:  he linear predictors in all equations contain the random component z*gamma

***************

prior.beta

***************

  1. "DN": Diffuse Normal
  
  2. "L1": L1-like  shrinkage prior 
  
  3. "L2": L2-like  shrinkage prior 
  
  4. "ARD": ARD-like  shrinkage prior 


**************

prior.Sigma

**************

	1. "VC.unif": Sigma is diagnonal, the prior for the standard deviation of each random variable follows an indepedent uniform distribution; 
  
	2. "VC.thalf": Sigma is diagnonal, the prior for the standard deviation of each random variable follows an indepedent half-t distribution with degree of freedom 1 (half-Cauchy); 
  
	3. "UN.unif": Sigma is of full parameterization, the prior for the standard deviation of each random variable follows an indepedent uniform distribution; The correlation correlations among the random effects 
  
	4. "UN.thalf". Sigma is of full parameterization, the prior for the standard deviation of each random variable follows an indepedent half-t distribution with degree of freedom 1 (half-Cauchy); 	 	 

}

\value{
\item{model}{a MCMC (JAGS) model object, from which samples can be drawn and DIC can be calculated}
\item{oripara}{posterior samples of the raw parameters, including regression coefficient from the linear predictors  and variance parameteres as originally coded in the model.}
}


\references{
Liu, F. and Kong, Y. (2014). zoib:  a R Package for Bayesian Inferences for zero/one Inflated Beta Regression Model, submitted

Liu, F. and Li, Q. (2014) A Bayesian Model for Joint Analysis of Multivariate  Repeated Measures and Time to Event Data in Crossover Trials, Statistical Methods  in Medical Research, doi: 10.1177/0962280213519594

Bae, K. and Mallick, B. K. (2004), Gene selection using a two-level hierarchical Bayesian model, Bioinformatics, 20(18): 3423-3430
}
\author{
Fang Liu (fang.liu.131@nd.edu)
}

\examples{
  \dontrun{
  ##### eg1
	data("GasolineYield", package = "zoib")
  GasolineYield$batch <- as.factor(GasolineYield$batch)
  
	# fixed effects zoib with batch treated as a 10-level categorical variable
	eg.fixed <- zoib(yield ~ temp + as.factor(batch)| 1, data=GasolineYield,
      joint = FALSE,  random = 0, EUID = 1:nrow(GasolineYield),
      zero.inflation = FALSE, one.inflation = FALSE,
      n.iter = 1000, n.thin = 5)
	sample1 <- eg.fixed$oripara 
	traceplot(sample1); 
	autocorr.plot(sample1); 
	gelman.diag(sample1)
	sample1.c1 <- sample1[[1]][10:200,]
	sample1.c2 <- sample1[[2]][10:200,]
	sample12 <- rbind(sample1.c1, sample1.c2)
	summ1 <- summary(mcmc(sample12))

	# mixed effect modles with batch treated as a random variable
	eg.random <- zoib(yield ~ temp | 1 | 1, data=GasolineYield,
        joint = FALSE, random=1, EUID=GasolineYield$batch,
        zero.inflation = FALSE, one.inflation = FALSE,
        n.iter=2000, n.thin=10)
	sample2 <- eg.random$oripara 
	traceplot(sample2); 
	autocorr.plot(sample2); 
  gelman.diag(sample2)
	sample2.c1<- sample2[[1]][10:200,]
	sample2.c2<- sample2[[2]][10:200,]
	sample12 <- rbind(sample2.c1, sample2.c2)
	summ2 <- summary(mcmc(sample12))
  
  
  ### eg2: joint modeling of bivariate beta variables with repeated measures
	data("BiRepeated", package = "zoib")
	eg2 <- zoib(y1|y2 ~ x|1|x, data= BiRepeated,
          random=1, EUID= BiRepeated$id,
          zero.inflation = FALSE, one.inflation = FALSE,
          prior.Sigma = "UN.unif", n.iter=4000, n.thin=10)
	sample3 <- eg2$oripara
	sample3.c1 <- sample3[[1]][101:400,]
	sample3.c2 <- sample3[[2]][101:400,]
	sample3 <- mcmc.list(mcmc(sample3.c1),mcmc(sample3.c2))
	summary(sample3) 
  
  
  ##### eg3: modelling with clustered beta variables with inflation at 0
	data("AlcoholUse", package = "zoib")
	AlcoholUse$Grade <- as.factor(AlcoholUse$Grade)

  eg3 <- zoib(Percentage ~ Grade+Days+Gender|1|Grade+Days+Gender|1,
        data = AlcoholUse, random = 1, EUID= AlcoholUse$County,
        zero.inflation = TRUE, one.inflation = FALSE, joint = FALSE, 
        n.iter=4000, n.thin=20)  
	sample4 <- eg3$oripara 
	sample4.c1<- sample4[[1]][10:200,]
	sample4.c2<- sample4[[2]][10:200,]
	sample4 <- mcmc.list(as.mcmc(sample4.c1),as.mcmc(sample4.c2))
	summ <- summary(mcmc(sample4))
  }
}
