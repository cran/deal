%                               -*- Mode: Rd -*- 
% elementin.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:01:48 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Thu Jul 24 16:08:35 2003
% Update Count    : 7
% Status          : Unknown, Use with caution!
% 

\name{elementin}
\alias{elementin}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Is a network element in a list of networks?}

\description{Sequentially check if the network is in the list.
}

\usage{
elementin(nw,nwl)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{an object of class \code{\link{network}}.}
  \item{nwl}{an object of class \code{\link{networkfamily}}.}
}

\details{Sequentially, the elements in \code{nwl} are compared to
  \code{nw} using \code{\link{nwequal}}. If this returns \code{TRUE}, then
  search is terminated and \code{TRUE} is returned. Otherwise, \code{FALSE} is
  returned. 
}

\seealso{\code{\link{nwequal}},\code{\link{unique.networkfamily}}
  }

\value{a logical. \code{TRUE} if \code{nw} is in \code{nwl}, else \code{FALSE}.}

\references{
  Further information about \bold{deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\examples{
data(rats)
rats.nw  <- network(rats)
rats.prior <- jointprior(rats.nw,12)
rats.nw <- learn(rats.nw,rats,rats.prior)$nw
allrats <- networkfamily(rats,rats.nw,rats.prior)

elementin(rats.nw,allrats$nw)
}

\keyword{models}
