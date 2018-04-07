\name{BiRepeated}
\alias{BiRepeated}
\docType{data}
\title{
Data from a correlated bivariate beta distribution with repeated measures
}
\description{
BiRepeated is a simulated data set with bi-Beta variables y1 and y2 on 200 independent cases. Both y1 and y2 are repeatedly measured at a given set of covariate values x = (0.1, 0.2, 0.3, 0.4, 0.5, 0.6). Joint zoib modeling is applied to examine the effect of x on y1 and y2 simultaneously.
}
\usage{data(BiRepeated)}
\format{
  A data frame with 200 independent cases, from which 6 measurements are taken on 2 response variables.
  \describe{
    \item{\code{id}}{id of the 200 cases.}
    \item{\code{y1}}{value of one beta variable (6 measurements per case) ranged from 0 to 1.}
    \item{\code{y2}}{value of the other beta variables (6 measurements per case) ranged from 0 to 1.}
    \item{\code{x}}{numerical; explanatory variable.}
  }
}


\keyword{datasets
}



\examples{
  \dontrun{
  library(zoib)
  data("BiRepeated", package = "zoib")
  eg2 <- zoib(y1|y2 ~ x|1|x, data= BiRepeated, random=1,n.response=2,
              EUID= BiRepeated$id, joint=TRUE,zero.inflation = FALSE,
              one.inflation = FALSE, prior.Sigma = "VC.unif",  			
              n.iter=7000,n.thin=25,n.burn=2000)
  coeff <- eg2$coeff
  summary(coeff)
  
  ### check convergence
  traceplot(coeff); 
  autocorr.plot(coeff); 
  check.psrf(coeff)
  
  ### plot posterior mean of y vs. observed y to check on goodness of fit.
  n= nrow(BiRepeated)
  ypred1 = rbind(eg2$ypred[[1]][,1:n],eg2$ypred[[2]][,1:n])
  ypred2 = rbind(eg2$ypred[[1]][,(n+1):(2*n)],eg2$ypred[[2]][,(n+1):(2*n)])
  post.mean1 = apply(ypred1,2,mean); 
  post.mean2 = apply(ypred2,2,mean); 
  
  plot(BiRepeated$y1, post.mean1, xlim=c(0,1),ylim=c(0,1), col='green2',
       pch=2,xlab='Observed y', ylab='Predicted y', main="")
  points(BiRepeated$y2,post.mean2,col='purple')
  abline(0,1,col='red')
  legend(0.1,0.9,col=c('green2','purple'),c("y1","y2"),pch=c(2,1))
  }
}
