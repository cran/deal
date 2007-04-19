%                               -*- Mode: Rd -*- 
% simulation.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:04:28 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Fri Dec 22 10:56:11 2006
% Update Count    : 25
% Status          : Unknown, Use with caution!
% 

\name{rnetwork}
\alias{rnetwork}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Simulation of data sets with a given dependency structure}

\description{
  Given a network with nodes having the \code{simprob} property,
  \code{rnetwork} simulates
a data set.  }

\usage{
rnetwork(nw, n=24, file="") 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{an object of class \code{\link{network}}, where each node
    has the property \code{simprob} (see \code{\link{makesimprob}}).}
  \item{n}{an integer, which gives the number of cases to simulate.}
  \item{file}{a string. If non-empty, the data set is stored there.}
}

\details{
  The variables are simulated one at a time in an order that ensures
  that the parents of the node have already been simulated. For discrete
  variables a multinomial distribution is used and for continuous
  variables, a Gaussian distribution is used, according to the
  \code{simprob} property in each node.
}

\value{
  A data frame with one row per case. If a file name is given, a file is
  created with the data set.
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

\examples{
A  <- factor(NA,levels=paste("A",1:2,sep=""))
B  <- factor(NA,levels=paste("B",1:3,sep=""))
c1 <- NA
c2 <- NA
df <- data.frame(A,B,c1,c2)

nw <- network(df,doprob=FALSE) # doprob must be FALSE
nw <- makesimprob(nw)          # create simprob properties

set.seed(944) 
sim <- rnetwork(nw,n=100)    # create simulated data frame
}

\keyword{models}
