\name{BiRepeated}
\alias{BiRepeated}
\docType{data}
\title{
Data from a correlated bivariate beta distribution with repeated measures
}
\description{
A simulated data set with two correlated Beta variables y1 and y2 on 50 independent cases. Both y1 and y2 are repeatedly measured at a given set of covariate values x = (0, 0.1, 0.2, 0.3, 0.4,0.5). Joint zoib modeling is applied examine the effect of x on each of the two beta variables.
}
\usage{data(BiRepeated)}
\format{
  A data frame with 50 independent cases, on which 6 meassurements are taken on 2 response variables.
  \describe{
    \item{\code{id}}{id of the 50 cases.}
    \item{\code{y1}}{value of one beta variable (6 measurements per case) ranged from 0 to 1.}
    \item{\code{y2}}{value of the other beta variables (6 measurements per case) ranged from 0 to 1.}
    \item{\code{x}}{numerical; explanatory varaible.}
  }
}


\keyword{datasets}

\examples{
  \dontrun{
  data("BiRepeated", package = "zoib")
	post.obj <- zoib(y1|y2 ~ x|1|x, data= BiRepeated,
  random=1, EUID= BiRepeated$id, 
  zero.inflation = FALSE, one.inflation = FALSE,
  prior.Sigma = "UN.unif", n.iter=4000, n.thin=20)
	post.sample <- post.obj$oripara
	post.sample.c1 <- post.sample[[1]][51:200,]
	post.sample.c2 <- post.sample[[2]][51:200,]
	post.sample <- mcmc.list(mcmc(post.sample.c1),mcmc(post.sample.c2))
	summary(post.sample)
  }
}
