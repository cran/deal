%                               -*- Mode: Rd -*- 
% nwequal.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:42 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Thu Jul 24 10:10:22 2003
% Update Count    : 10
% Status          : Unknown, Use with caution!
% 

\name{nwequal}
\alias{nwequal}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Test if the graphs of two networks are equal}

\description{Given two network objects, the nodes in the nodelists are
  compared by examining if their parents are the same.
}

\usage{
nwequal(nw1,nw2) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw1}{an object of class \code{\link{network}}}
   \item{nw2}{an object of class \code{\link{network}}}
}

\details{The networks are assumed to have the same number of nodes and
  the nodes are assumed to represent the same stochastic
  variables. Sequentially the parents of the nodes are compared and if they
  differ, \code{FALSE} is returned. 
}

\value{a logical. \code{TRUE} if the network graphs are equal,
  \code{FALSE} otherwise.} 

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

\references{
  Further information about \bold{deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~


\keyword{iplot}
