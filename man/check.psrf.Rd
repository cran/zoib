\name{check.psrf}
\alias{check.psrf}

\title{
Convergence Check for Markov Chain Monte Carlo simulations via Potential Scale Reduction Factor
}

\description{
Computes and summarizes univariate potential scale reduction factor; checks whether the multivariate potential scale reduction factor can be calculated.
}

\usage{
check.psrf(post1 = NULL, post2 = NULL, post3 = NULL, post4 = NULL, post5 = NULL, 
save=FALSE)
}


\arguments{
\item{post1}{Monte Carlo Posterior draws (data frame) from the 1st Markov Chain.}
\item{post2}{Monte Carlo Posterior draws (data frame) from the 2nd Markov Chain.}
\item{post3}{Monte Carlo Posterior draws (data frame) from the 3rd Markov Chain.}
\item{post4}{Monte Carlo Posterior draws (data frame) from the 4th Markov Chain.}
\item{post5}{Monte Carlo Posterior draws (data frame) from the 5th Markov Chain.}
\item{save}{A logical argument: whether the psrf plots are to be saved as a pdf file}
}

\details{
Each argument is a dataframe, with each column representing a parameter from the model, and each row presenting a posterior draw on the parameters. The command takes up to 5 chains though it is not necessary to have 5 chain; but at least 2 chains are necessary.
}

\value{
The function outputs 
  \item{"psrf.pdf"}{A pdf file containing plots from gelman.plot( ).}
  \item{"psrf_boxplot.pdf"}{A pdf file containing a box plot of the univaraite psrf values, and a box plot of the 95\% upper bound of the univaraite psrf values from all model parameters.}
  \item{"psrf.txt"}{a text file containin the univaraite psrf values and 95\% confidence interval from all model parameters.}
  \item{psrf.s}{univaraite psrf values and the 95\% confidence interval from all model parameters.}
  \item{psrf.m}{multivariate psrf if the covariance matrix of the parameters are positive definite.}
  \item{psrf.s.summ}{the summary of the univariate psrf across parameter.}
}


\author{
Fang Liu (fang.liu.131@nd.edu)
}

\seealso{
 \code{\link{zoib}}
}

\examples{
  \dontrun{
	post1= data.frame(cbind(rnorm(400,0,1), rbeta(400,2,3)))
	post2= data.frame(cbind(rnorm(400,0,1), rbeta(400,2,3)))
	check.psrf(post1,post2)
	}
}
