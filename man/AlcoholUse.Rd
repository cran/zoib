\name{AlcoholUse}
\alias{AlcoholUse}
\docType{data}
\title{
California County-level Teenager  Monthly Alcohol Use data
}

\description{
AlcoholUse contains the county-level monthly alcohol use data from students in California in years 2008 to 2010. The data can be downloaded at http://www.kidsdata.org. The data has information on the percentage of public school students in grades 7, 9, and 11 in five buckets of days (0, 1-2, 3-9, 10-19, 20-30) in which they drank alcohol in the past 30 days. Since the percentages across the five days categories add up to 1 given a combination of County, Grade and Gender. Data points associated with 0 are completely redundant and are not in AlcoholUse. zoib is applied to examining whether the proportions of alcohocol use in the past month are different across gender, grade, days of drinking.
}

\usage{
data(AlcoholUse)
}
\format{
A data frame with 1340 observations on the following 6 variables.
  \describe{
  \item{\code{County}}{a factor with 56 levels/clusters/counties.}
	\item{\code{Grade}}{a numeric/factor vector: 7, 9, 11 grades.}
  \item{\code{Days}}{a factor with levels  \code{0} \code{[1,2]} \code{[3,9]} \code{[10,19]} \code{[20,30]}.}
  \item{\code{MedDays}}{a numeric vector: the med point of each of the 4 day buckets.}
  \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}.}
  \item{\code{Percentage}}{a numeric vector; Proportions of public school students in 4 buckets of days in which they drank alcohol in the past 30 days per Gender, Grader, and County}.}
}

\source{
http://www.kidsdata.org
}

\keyword{datasets
}

\examples{
  \dontrun{
  ##### eg3: modelling with clustered beta variables with inflation at 0
    library(zoib)
    data("AlcoholUse", package = "zoib")
    eg3 <- zoib(Percentage ~ Grade*Gender+MedDays|1|Grade*Gender+MedDays|1,
                data = AlcoholUse, random = 1, EUID= AlcoholUse$County,
                zero.inflation = TRUE,  one.inflation = FALSE, joint = FALSE, 
                n.iter=5000, n.thin=20, n.burn=1000)  
    sample1 <- eg3$coeff
    summary(sample1)
    
    # check convergence on the regression coefficients
    traceplot(sample1); 
    autocorr.plot(sample1);
    check.psrf(sample1)
    
    # plot posterior mean of y vs. observed y to check on goodness of fit.
    ypred = rbind(eg3$ypred[[1]],eg3$ypred[[2]])
    post.mean= apply(ypred,2,mean); 
    par(mfrow=c(1,1),mar=c(4,4,0.5,0.5))
    plot(AlcoholUse$Percentage, post.mean, xlim=c(0,0.4),ylim=c(0,0.4), 
         col='blue', xlab='Observed y', ylab='Predicted y', main="")
    abline(0,1,col='red')
  }
}


