%                               -*- Mode: Rd -*- 
% score.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:31 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Thu Dec 04 13:08:17 2008
% Update Count    : 10
% Status          : Unknown, Use with caution!
% 

\name{score}
\alias{score}
\alias{score.network}
\alias{score.node}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Network score}

\encoding{latin1}

\description{Accessor for the score from a node or network
}


\usage{
score(x,...)

\method{score}{node}    (x,...)
\method{score}{network} (x,...)

}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{an object of class \code{node} or \code{network}.}
  \item{\dots}{additional arguments for specific methods.}
}

\value{For networks, the log network score is returned. For nodes, the
  contribution to the log network score is returned.
}
% \references{
%   Further information about \bold{deal} can be found at:\cr
%   \url{http://www.math.aau.dk/~dethlef/novo/deal}.
% }

\author{
  Susanne Gammelgaard Bottcher, \cr
  Claus Dethlefsen \email{rpackage.deal@gmail.com}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\keyword{models}
