%                               -*- Mode: Rd -*- 
% makesimprob.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:02:48 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Wed Sep 18 08:45:35 2002
% Update Count    : 14
% Status          : Unknown, Use with caution!
% 

\name{makesimprob}
\alias{makesimprob}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Make a suggestion for simulation probabilities}

\description{Creates local probability distributions reflecting the
  graph of the network. These are attached as a \code{simprob} attribute
  to each node in the network and can be edited and used for
  \code{\link{simulation}}. 
}

\usage{
makesimprob(nw,
                        s2=function(idx,cf) {
                          cf <- as.vector(cf)
                          xs <- (1:length(cf))
                          log(xs%*%cf+1)
                        },
                        m0=function(idx,cf) {
                          cf <- as.vector(cf)
                          xs <- (1:length(cf))^2
                          .69*(xs%*%cf)
                          },
                        m1=function(idx,cf) {
                          cf <- as.vector(cf)
                          xs <- (1:length(cf))*10
                          idx*(cf%*%xs)
                          })
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{The network.}
  \item{s2}{Function that returns the variance as a function of the node
    index and the configuration of the discrete variables.}
  \item{m0}{Function that returns the intercept  as a function of the node
    index and the configuration of the discrete variables..}
  \item{m1}{Function that returns the regression coefficients as a function of the node
    index and the configuration of the discrete variables..}
}

\details{
  For each node, the local \code{simprob} is determined. If the node is
  discrete, the probability distribution is uniform (and thus not
  reflecting the dependence in the graph as it should). If the node is
  continuous, one mean and variance is attached per configuration of the
  discrete parents. The mean depends on the continuos parents and is the
  regression coefficients determined by the functions \code{m0}
  (intercept) and \code{m1} (regression coefficients). The variance is
  determined by the function \code{s2}.
}

\value{The network where each node has attached the attribute
  \code{simprob}.}

\references{
  Further information about \bold{Deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}

\seealso{\code{\link{simulation}}}

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~


\keyword{models}
