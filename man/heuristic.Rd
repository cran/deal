%                               -*- Mode: Rd -*- 
% heuristic.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:02:12 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon Sep 16 18:47:58 2002
% Update Count    : 3
% Status          : Unknown, Use with caution!
% 

\name{heuristic}
\alias{heuristic}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Heuristic greedy search with random restart}

\description{
  Tries to find the network with highest networkscore.
}

\usage{
heuristic(initnw,data,prior=jointprior(network(data)),
          maxiter=100,restart=10,degree=initnw$n,
          trylist= rep(list(NULL),initnw$n),trace=FALSE,
          timetrace=TRUE,smalldf=NA)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{initnw}{The initial network wherefrom search is started. It is
    also this network that is perturbed when restarting.}
  \item{data}{Dataframe used for learning the network, see \code{\link{network}}.}
  \item{prior}{A joint prior, see \code{\link{jointprior}}.}
  \item{maxiter}{Max search steps in the search algorithm (see \code{\link{autosearch}}).}
  \item{restart}{The number of times to perturb initnw and rerun the
    search.}
  \item{degree}{Degree of perturbation, see \code{\link{perturb}}.}
  \item{trylist}{Used internally for reusing learning of nodes, see \code{\link{maketrylist}}.}
  \item{trace}{If TRUE, plots the tried networks while searching.}
  \item{timetrace}{If TRUE, prints some timing information on the screen.}
  \item{smalldf}{Used for analysing networks with time-varying nodes,
    see \code{\link{timeslice}}.}
}

\details{
  Calls \code{\link{autosearch}} on the \code{initnw}. Then restarts by
  perturbing \code{initnw} \code{degree} times and calling \code{autosearch} again. The number
  of restarts is given by the option \code{restart}. If \code{restart=0}, the
  procedure is little more than a call to autosearch.
}

\value{A list with two elements
  \item{nw}{A networkfamily with all created networks.}
  \item{trylist}{Updated.}
}

\references{
  Further information about \bold{Deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

\seealso{\code{\link{autosearch}},\code{\link{perturb}}}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\examples{
  data(rats)
  fit       <- network(rats,specifygraph=FALSE,inspect=FALSE)
  fit.prior <- jointprior(fit,12) 
  fit       <- learn(fit,rats,fit.prior)$nw
  hisc      <- heuristic(fit,rats,fit.prior,restart=5,degree=3)$nw
}

\keyword{models}
