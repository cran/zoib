\name{AlcoholUse}
\alias{AlcoholUse}
\docType{data}
\title{
California County-level Teenager  Monthly Alcohol Use data
}

\description{
AlcoholUse contains the county-level monthly alcohol use data from students in California in years 2008 to 2010. The data can be downloaded at http://www.kidsdata.org. The data has information on the percentage of public school students in grades 7, 9, and 11 in five buckets of days (0, 1-2, 3-9, 10-19, 20-30) in which they drank alcohol in the past 30 days. zoib is applied to examining whether the proportions of alhocol use in the past month are different across gender, grade, days of drinking.
}

\usage{
data(AlcoholUse)
}
\format{
A data frame with 1675 observations on the following 6 variables.
  \describe{
  \item{\code{County}}{a factor with 56 levels/clusters/counties.}
	\item{\code{Grade}}{a numeric/factor vector: 7, 9, 11 grades.}
  \item{\code{Days}}{a factor with levels  \code{0} \code{[1,2]} \code{[3,9]} \code{[10,19]} \code{[20,30]}.}
  \item{\code{MedDays}}{a numeric vector: the med point of each of the 5 day buckets.}
  \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}.}
  \item{\code{Percentage}}{a numeric vector; Proportions of public school students in 5 buckets of days in which they drank alcohol in the past 30 days}.}
}

\source{
http://www.kidsdata.org
}

\keyword{datasets}

\references{
Liu, F. and Kong, Y. (2015). ZOIB:  an R Package for Bayesian Inferences in Beta and Zero One Inflated Beta Regression Models, submitted

Liu, F. and Li, Q. (2014) A Bayesian Model for Joint Analysis of Multivariate  Repeated Measures and  Time to Event Data in Crossover Trials, Statistical Methods  in Medical Research, doi: 10.1177/0962280213519594
}

\examples{
  \dontrun{
  ##### eg3: modelling with clustered beta variables with inflation at 0
  data("AlcoholUse", package = "zoib")
  AlcoholUse$Grade <- as.factor(AlcoholUse$Grade)

  post.obj <- zoib(Percentage ~ Grade+Days+Gender+Grade:Days+
              Grade:Gender+Days:Gender|1|Grade+Days+Gender+Grade:Days+
              Grade:Gender+Days:Gender|1, data = AlcoholUse, random = 1, 
              EUID= AlcoholUse$County, zero.inflation = TRUE, one.inflation = FALSE, 
               joint = FALSE, n.iter=5000, n.thin=20, n.burn=1000) 
	coeff <- post.obj$coeff
  traceplot(coeff)
  autocorr.plot(coeff) 
  check.psrf(coeff)
	summ <- summary(coeff))
  
  post.mean= apply(rbind(post.obj$ypred[[1]],post.obj$ypred[[2]]), 2, mean); 
  plot(AlcoholUse$Percentage,post.mean); abline(0,1,col='red')
  }
}


