%                               -*- Mode: Rd -*- 
% nwfunique.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:51 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Wed Jan 07 09:18:38 2004
% Update Count    : 15
% Status          : Unknown, Use with caution!
% 

\name{unique.networkfamily}
\alias{unique.networkfamily}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Makes a network family unique.}

\description{Removes networks that are equal or equivalent to networks already in the network family.
}

\usage{
\method{unique}{networkfamily}(x,incomparables=FALSE,equi=FALSE,timetrace=FALSE,epsilon=1e-12,...)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{an object of class \code{networkfamily}.}
  \item{incomparables}{a logical, but has no effect.}
   \item{equi}{a logical. If \code{TRUE}, also equivalent networks are
     thrown out 
(\emph{i.e.} if
     their score is within \code{epsilon} from another network).}
   \item{timetrace}{a logical. If \code{TRUE}, prints some timing
    information on the screen.} 
   \item{epsilon}{a numeric, which measures how close network scores are
     allowed to be from each other to be 'equivalent'.}
   \item{...}{further arguments (no effect)}
}



\author{
  Susanne Gammelgaard B�ttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}
\references{
  Further information about \bold{deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\examples{
data(rats)
rats.nwf <- networkfamily(rats)
rats.nwf2<- unique(getnetwork(rats.nwf),equi=TRUE)
}
\keyword{iplot}