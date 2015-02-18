\name{check.psrf}
\alias{check.psrf}

\title{
Convergence Check for Markov Chain Monte Carlo simulations via Potential Scale Reduction Factor
}

\description{
check.psrf computes and summarizes univariate potential scale reduction factor. It also checks whether the multivariate potential scale reduction factor can be calculated.
}

\usage{
check.psrf(post1 = NULL, post2 = NULL, post3 = NULL, post4 = NULL, post5 = NULL)
}


\arguments{
\item{post1}{an mcmc.list with posterior samples from all Markov chains, or a data frame containing the draws from the 1st Markov Chain.}
\item{post2}{Monte Carlo Posterior draws (data frame) from the 2nd Markov Chain, if the post1 is a data frame; specify post2 only needed}
\item{post3}{Monte Carlo Posterior draws (data frame) from the 3rd Markov Chain, if the post1 is a data frame; specify post3 only needed}
\item{post4}{Monte Carlo Posterior draws (data frame) from the 4th Markov Chain, if the post1 is a data frame; specify post4 only needed}
\item{post5}{Monte Carlo Posterior draws (data frame) from the 5th Markov Chain, if the post1 is a data frame; specify post5 only needed}
}

\details{
The posterior samples from each chain are stored in a data frame, with columns representing parameters from the model, and rows presenting posterior draws on the parameters. If the input post1 is a data frame contains the draws from one chain, then check.psrf can take up to 5 chains though it is not necessary to have 5 chain; but at least 2 chains are necessary.
}

\value{
The function outputs 
  \item{psrf.s}{univaraite psrf values and the 95\% confidence interval from all model parameters.}
  \item{psrf.m}{multivariate psrf if the covariance matrix of the parameters are positive definite.}
  \item{psrf.s.summ}{the summary of the univariate psrf across parameter.}
}


\author{
Fang Liu (fang.liu.131@nd.edu)
}


\examples{
  \dontrun{
	post1= data.frame(cbind(rnorm(400,0,1), rbeta(400,2,3)))
	post2= data.frame(cbind(rnorm(400,0,1), rbeta(400,2,3)))
	check.psrf(post1,post2)
	}
}
