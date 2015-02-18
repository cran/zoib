\name{paraplot}
\alias{paraplot}

\title{
visual display of the posterior inferences of the parameters from a zoib model
}

\description{
paraplot plots the posterior mode, mean, or median with Bayesian credible intervals for the parameters from a zoib model. plot.para can accommodate up to 4 groups (models) of inferences. 
}

\usage{
paraplot(para1, para2=NULL, para3=NULL, para4=NULL,
          tickx=NULL, jitter=NULL, pch = 1:4, col=1:4, 
          legpos=NULL, legtext, annotate=FALSE)
}


\arguments{
\item{para1}{a data frame or matrix with posterior inferences from approach (model) 1.}
\item{para2}{a data frame or matrix with posterior inferences from approach (model) 4; specify para2 only needed.}
\item{para3}{a data frame or matrix with posterior inferences from approach (model) 3; specify para3 only needed.}
\item{para4}{a data frame or matrix with posterior inferences from approach (model) 2; specify para4 only needed.}
\item{tickx}{customization of the tickx of the x-axis.}
\item{jitter}{jittering distance between the plotting positions of different groups to enhance plot readability.}
\item{pch}{point types for different groups.}
\item{col}{colors for different types.}
\item{legpos}{the positions of legend in the format of c(x,y).}
\item{legtext}{contents of the legtext, such as the names of the approaches.}
\item{annotate}{whether to label parameters.}
}


\details{
Each of the applicabale para objects contains 3 columns. The 1st column is the point estimate (posterior mean, median or mode), the 2nd and 3rd columns contains the lower and upper bounds of the Bayesian credible intervals. The rows represent the parameters from an approach (model). It is not necessary that all para objects have the same set of parameters.  
}



\author{
Fang Liu (fang.liu.131@nd.edu)
}


\examples{
  \dontrun{
  set.seed(12) 
  x=rnorm(4); para1 = cbind(x, x-1,x+1); rownames(para1) = c("a","b","c","d")
  x=rnorm(4)+1; para2 = cbind(x, x-1,x+1); rownames(para2) = c("a","b","e","f")
  x=rnorm(3)+2; para3 = cbind(x, x-1,x+1); rownames(para3) = c("a","b","d")
  paraplot(para1, para2, para3, para4=NULL, legpos=c(-1.5,6),
            legtext=c("model 1","model 2","model 3"),annotate=TRUE)
  paraplot(para1, legpos=c(-2,3), legtext="m1", annotate=TRUE)
	}
}
