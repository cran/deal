%                               -*- Mode: Rd -*- 
% autosearch.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:01:29 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon Sep 16 18:47:34 2002
% Update Count    : 8
% Status          : Unknown, Use with caution!
% 

\name{autosearch}
\alias{autosearch}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Autosearch}

\description{
  Greedy search for a network with high networkscore
}

\usage{
autosearch(initnw,data,prior=jointprior(network(data)),maxiter=50,
           trylist= rep(list(NULL),initnw$n),trace=FALSE,
           timetrace=FALSE,smalldf=NA,showban=FALSE,saveall=TRUE) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{initnw}{The initial network from which the search is started. }
  \item{data}{Dataframe used for learning the network, see \code{\link{network}}.}
  \item{prior}{A joint prior, see \code{\link{jointprior}}.}
  \item{maxiter}{Maximum search steps in the search algorithm.}
  \item{trylist}{Used internally for reusing learning of nodes, see \code{\link{maketrylist}}.}
  \item{trace}{If TRUE, plots the networks during search.}
  \item{timetrace}{If TRUE, prints some timing information on the screen.}
  \item{smalldf}{Used for analysing networks with time-varying nodes,
    see \code{\link{timeslice}}.}
  \item{showban}{Passed to \code{\link{plot.network}}.}
  \item{saveall}{If TRUE, all networks explored in the search is
    returned (memory consuming). Otherwise only the best is returned.}
}

\details{
  In each step, a list of networks is created with either one extra
  arrow, one less arrow or with one arrow turned (if a cycle is not
  generated). The network scores of all the proposal networks are
  calculated and the network with the highest score is chosen for the
  next step in the search. If no proposed networks has a higher network
  score than the previous network, the search is terminated. The
  returned network list contain all tried networks (depending on the
  value of saveall).  
}

\value{A list with two elements
  \item{nw}{A networkfamily with all (or the best) tried
    networks. Duplicate networks have not been removed (use
    \code{\link{nwfunique}}).} 
  \item{trylist}{Updated.}
}

\seealso{\code{\link{heuristic}}
  }


\references{
  Further information about \bold{Deal} can be found at:\cr
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
fit       <- network(rats,specifygraph=FALSE,inspect=FALSE)
fit.prior <- jointprior(fit,12)
fit       <- learn(fit,rats,fit.prior)$nw
fit       <- insert(fit,2,1,rats,fit.prior)$nw
fit       <- insert(fit,1,3,rats,fit.prior)$nw
\dontrun{hisc      <- autosearch(fit,rats,fit.prior)$nw}
\dontrun{hisc      <- nwfsort(hisc)}
}

\keyword{models}
