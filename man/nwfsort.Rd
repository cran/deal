%                               -*- Mode: Rd -*- 
% nwfsort.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:47 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Tue May 28 14:02:02 2002
% Update Count    : 3
% Status          : Unknown, Use with caution!
% 

\name{nwfsort}
\alias{nwfsort}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Sort a list of networks}

\description{According to the \code{score} attribute of the networks in
  a networkfamily, the networks are sorted and the relative score to the
  highest score is attached to each network as the \code{relscore}
  attribute. 
}

\usage{
nwfsort(nwf)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nwf}{Networkfamily}
}


\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~
\references{
  Further information about \bold{Deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}


\keyword{iplot}
