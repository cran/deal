%                               -*- Mode: Rd -*- 
% nwfsort.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:47 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Fri Jul 25 14:35:47 2003
% Update Count    : 7
% Status          : Unknown, Use with caution!
% 

\name{nwfsort}
\alias{nwfsort}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Sorts a list of networks}

\description{According to the \code{score} property of the networks in
  a network family, the networks are sorted and the relative score,
i.e.\ the score of a network relative to the
  highest score, is attached to each network as the \code{relscore}
  property. 
}

\usage{
nwfsort(nwf)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nwf}{an object of class \code{networkfamily}.}
}


\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~
\references{
  Further information about \bold{deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}


\keyword{iplot}
