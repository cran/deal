%                               -*- Mode: Rd -*- 
% findex.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:01:52 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Tue Jul 22 16:57:47 2003
% Update Count    : 11
% Status          : Unknown, Use with caution!
% 

\name{findex}
\alias{findex}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Translation between indices in a multiway array}

\description{Translate back and forth between indices in multiway arrays
}

\usage{
findex (i, dim, config=TRUE)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{i}{either a matrix (if \code{config} is \code{TRUE}) of
    configurations or a vector (if \code{config} is \code{FALSE}) of indices.}
   \item{dim}{a vector of integers giving the dimension of the multiway array.}
   \item{config}{a logical. If \code{TRUE}, \code{i} is interpreted as a
     matrix of configurations. If \code{FALSE}, \code{i} is interpreted
     as a matrix of indices in the unfolded multiway array with
     dimension \code{dim}.} 
}

\details{
  This function is intended for internal use.
  
  If \code{config} is \code{TRUE}, then \code{i} is interpreted as a matrix
  with one row per wanted entry. The columns are the configurations of
  each of the discrete variables (in the proper order).
    Returned is a vector of length the number of rows of \code{i}. The
    entries correspond to each row and is the corresponding number if
    the array were 'folded' out.

    If \code{config} is \code{FALSE}, then \code{i} is a vector of indices in
    the unfolded 
    array. We want the corresponding configurations of the discrete
    variables output is a matrix with one row per configuration.
    
   Thus, \code{findex(config=TRUE)} and \code{findex(config=FALSE)} are each others
   inverse functions.  
}


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

\examples{
  dims <- c(3,2,4)
  findex(findex(1:4,dims,config=FALSE),dims)
}

\keyword{iplot}
