%                               -*- Mode: Rd -*- 
% prob.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:31 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Fri Dec 22 10:56:11 2006
% Update Count    : 17
% Status          : Unknown, Use with caution!
% 

\name{prob}
\alias{prob}
\alias{prob.network}
\alias{prob.node}
\alias{localprob}
\alias{localprob<-}
\alias{localprior}
\alias{localposterior}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Local probability distributions}

\description{Methods for accessing or changing the local probability
  distributions and for accessing the local prior and posterior distributions
}

\usage{
prob(x,df,...)

\method{prob}{node} (x,df,nw,...)
\method{prob}{network} (x,df,...)

localprob(nw)
value <- localprob(nw,name)

localprior(node)
localposterior(node)

}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{an object of class \code{node} or \code{network}.}
  \item{df}{a data frame, where the columns define the variables. A
    continuous variable should have type \code{numeric} and discrete varibles
    should have type \code{\link[base]{factor}}.}
  \item{nw}{an object of class \code{\link{network}}.}
  \item{node}{an object of class \code{\link{node}}.}
  \item{name}{a string, which gives the node name.}
  \item{\dots}{additional arguments for specific methods.}
}

\details{
  The \code{prob} methods add local
  probability distributions to
  each node. If the node is continuous, this is a
  numeric vector with 
  the conditional variance and the conditional regression coefficients
  arising from a regression on the continuous parents, using data. If
  the node has discrete parents, \code{prob} is a matrix with a row
  for each configuration of the discrete parents. If the node is
  discrete, \code{prob} is a multiway array which gives the conditional
  probability distribution for each configuration of the discrete
  parents. The generated \code{prob} can be replaced to match the prior
  information available.

  \code{localprob} returns the probability distribution
  for each node in the network.

  In a learned network, the local prior and posterior can be accessed
  for each node using \code{localprior} and \code{localposterior}.
  
  }
\references{
  Further information about \bold{deal} can be found at:\cr
  \url{http://www.math.aau.dk/~dethlef/novo/deal}.
}

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.aau.dk}, \cr
  Claus Dethlefsen \email{cld@rn.dk}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\keyword{models}
