%                               -*- Mode: Rd -*- 
% simulation.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:04:28 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon Sep 16 18:48:54 2002
% Update Count    : 8
% Status          : Unknown, Use with caution!
% 

\name{simulation}
\alias{simulation}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Simulation of datasets with a given (in)dependency structure}

\description{
  Given a network with nodes having the 'simprob'-attribute, simulates a dataset
}

\usage{
simulation(nw, n=24, file="") 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{A network where each node have the attribute \code{simprob} (see
    \code{\link{makesimprob}}.}
  \item{n}{The number of cases to simulate.}
  \item{file}{If a filename is given, the dataset is stored there.}
}

\details{
  The variables are simulated one at a time in an order that ensures
  that the parents of the node have already been simulated. For discrete
  variables a multinomial distribution is used and for continuous
  variables, a normal distribution is used, according to the
  probabilities or parameters given in \code{simprob}.
}

\value{
  A dataframe with one row per case. If a filename is given, a file is
  created with the dataset.
}

\references{
  Further information about \bold{Deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}


\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\examples{
A  <- factor(NA,levels=paste("A",1:2,sep=""))
B  <- factor(NA,levels=paste("B",1:3,sep=""))
c1 <- NA
c2 <- NA
df <- data.frame(A,B,c1,c2)

nw <- network(df,specifygraph=FALSE,doprob=FALSE) # doprob must be False
nw <- makesimprob(nw)                     # create simprob attributes

set.seed(944) 
sim <- simulation(nw,n=100)               # create simulated dataframe
}

\keyword{models}
