%                               -*- Mode: Rd -*- 
% nwfunique.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:51 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Tue Jan  7 11:49:08 2003
% Update Count    : 5
% Status          : Unknown, Use with caution!
% 

\name{nwfunique}
\alias{nwfunique}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Makes a network family unique.}

\description{Makes a network family unique, i.e.\ removes networks that are equal or equivalent to networks already in the network family.
}

\usage{
nwfunique(nwf,equi=FALSE,timetrace=FALSE,epsilon=1e-12)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nwf}{Network family.}
   \item{equi}{If \code{TRUE}, also equivalent networks are thrown out
(i.e.\ if
     their score is within epsilon from another network).}
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
