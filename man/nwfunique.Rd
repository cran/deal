%                               -*- Mode: Rd -*- 
% nwfunique.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:51 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon Sep 16 16:00:15 2002
% Update Count    : 3
% Status          : Unknown, Use with caution!
% 

\name{nwfunique}
\alias{nwfunique}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Thin a list of networks}

\description{Throw away doubles or even equivalent networks from list.
}

\usage{
nwfunique(nwf,equi=FALSE,timetrace=FALSE,epsilon=1e-12)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nwf}{Networkfamily}
   \item{equi}{If T, also equivalent networks are thrown out (ie if
     their score is within epsilon from another network)}
   \item{timetrace}{For timing purposes.}
   \item{epsilon}{Closeness of equivalent networks.}
}



\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}
\references{
  Further information about \bold{Deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~


\keyword{iplot}
