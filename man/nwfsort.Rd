%                               -*- Mode: Rd -*- 
% nwfsort.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:47 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Thu Dec 04 13:17:12 2008
% Update Count    : 12
% Status          : Unknown, Use with caution!
% 

\encoding{latin1}
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
  Susanne Gammelgaard Bottcher \email{alma@math.aau.dk}, \cr
  Claus Dethlefsen \email{cld@rn.dk}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~
\references{
  Further information about \bold{deal} can be found at:\cr
  \url{http://www.math.aau.dk/~dethlef/novo/deal}.
}


\keyword{iplot}
