%                               -*- Mode: Rd -*- 
% nwfsort.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:47 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Sun Nov 14 18:01:12 2004
% Update Count    : 8
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
  Susanne Gammelgaard Bøttcher \email{alma@math.aau.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.aau.dk}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~
\references{
  Further information about \bold{deal} can be found at:\cr
  \url{http://www.math.aau.dk/novo/deal}.
}


\keyword{iplot}
