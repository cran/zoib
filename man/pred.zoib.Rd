\name{pred.zoib}
\alias{pred.zoib}

\title{
posterior predictive samples of Y for given new X
}

\description{
generate posterior predictive samples of Y for a given set of new X, and produce posterior summary on the posterior predictive samples
}

\usage{
pred.zoib(object, xnew, summary=TRUE)
}


\arguments{
\item{object}{
the object output from funtion zoib}
\item{xnew}{
a set of new X at each the posterior predictive values are calculated. xnew should be of the same type and format as the X in the original data where the zoib model is fitted}
\item{summary}{
if TRUE (the default), a basic summary on each posterior predictive value, including mean, SD, min, max, med, 2.5\% and 97.5\% quantiles, are provided.}
}



\author{
Fang Liu (fang.liu.131@nd.edu)
}

\details{ 
xnew should be in the format of data.frame and should of the same type and have the same variables as the X in the original data where the zoib model is fitted. See the example below.
}

\examples{
\dontrun{
  data("GasolineYield")
  eg1 <- zoib(yield ~ temp + as.factor(batch)| 1, data=GasolineYield,
                 joint = FALSE,  random = 0, EUID = 1:nrow(d),
                 zero.inflation = FALSE, one.inflation = FALSE,
                 n.iter = 1600, n.thin = 2, n.burn=100, seeds=c(1,2),n.chain=2)
  xnew <- data.frame(temp = c(205, 218), batch = factor(c(1, 2), levels = 1:10))
  ypred <- pred.zoib(eg1, xnew)
  
  data("BiRepeated")
  eg2 <- zoib(y1|y2 ~ x|1|x, data= BiRepeated, n.response=2,
            random=1, EUID= BiRepeated$id,
            zero.inflation = FALSE, one.inflation = FALSE,				
            prior.Sigma = "VC.unif", n.iter=2100, n.thin=10, n.burn=100)
  xnew<- data.frame(x=BiRepeated[1:6,4])
  pred.zoib(eg2,xnew)
	}
}
