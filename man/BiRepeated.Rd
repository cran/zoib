\name{BiRepeated}
\alias{BiRepeated}
\docType{data}
\title{
Data from a correlated bivariate beta distribution with repeated measures
}
\description{
A simulated data set with two correlated Beta variables y1 and y2 on 100 independent cases. Both y1 and y2 are repeatedly measured at a given set of covariate values x = (0, 0.1, 0.2, 0.3, 0.4, 0.5). Joint zoib modeling is applied to examine the effect of x on y1 and y2.
}
\usage{data(BiRepeated)}
\format{
  A data frame with 100 independent cases, from which 6 measurements are taken on 2 response variables.
  \describe{
    \item{\code{id}}{id of the 100 cases.}
    \item{\code{y1}}{value of one beta variable (6 measurements per case) ranged from 0 to 1.}
    \item{\code{y2}}{value of the other beta variables (6 measurements per case) ranged from 0 to 1.}
    \item{\code{x}}{numerical; explanatory varaible.}
  }
}


\keyword{datasets}


\references{
Liu, F. and Kong, Y. (2015). ZOIB:  an R Package for Bayesian Inferences in Beta and Zero One Inflated Beta Regression Models, submitted

Liu, F. and Li, Q. (2014) A Bayesian Model for Joint Analysis of Multivariate  Repeated Measures and  Time to Event Data in Crossover Trials, Statistical Methods  in Medical Research, doi: 10.1177/0962280213519594
}


\examples{
  \dontrun{
  library(zoib)
  data("BiRepeated", package = "zoib")
  eg2 <- zoib(y1|y2 ~ x|1|x, data= BiRepeated,  
              random=1, EUID= BiRepeated$id, joint=TRUE,
              zero.inflation = FALSE, one.inflation = FALSE,  			
              prior.Sigma = "UN.unif",n.iter=5000, n.thin=20, n.burn=1000)
  coeff <- eg2$coeff
  traceplot(coeff); 
  autocorr.plot(coeff); 
  check.psrf(coeff)
  summary(coeff)

  n<- nrow(BiRepeated)
  K<- length(BiRepeated$x)
  post.mean1<- apply(rbind(eg2$ypred[[1]][,1:n],eg2$ypred[[2]][,1:n]),2,mean)
  post.mean2<- apply(rbind(eg2$ypred[[1]][,(n+1):(2*n)], eg2$ypred[[2]][,(n+1):(2*n)]),2,mean) 
  
  library(modeest)
  for(i in 1:(n*K) 
    post.mode1[i] <- mlv(rbind(eg2$ypred[[1]][,i],eg2$ypred[[2]][,i]),method = "hsm")$M 
  for(i in (n*K+1):(2*n*K)) 
    post.mode2[i-n*K+1] <- mlv(rbind(eg2$ypred[[1]][,i],eg2$ypred[[2]][,i]),method = "hsm")$M 
    
  plot(BiRepeated$y1, post.mean1, xlim=c(0,1),ylim=c(0,1), col='green2')
  points(BiRepeated$y2,post.mean2,col='purple')
  points(BiRepeated$y2,post.mode1,col='purple',pch=2)
  points(BiRepeated$y2,post.mode2,col='purple',pch=2)
  abline(0,1,col='red')
  }
}
