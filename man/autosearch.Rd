%                               -*- Mode: Rd -*- 
% autosearch.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:01:29 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Sun May 25 07:22:57 2003
% Update Count    : 34
% Status          : Unknown, Use with caution!
% 

\name{autosearch}
\alias{autosearch}
\alias{heuristic}
\alias{makenw}
\alias{as.network}
\alias{modelstreng}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Greedy search}

\description{
  From initial network, do local perturbations to increase network score.
%  Greedy search for a network with high networkscore
}

\usage{
autosearch(initnw,data,prior=jointprior(network(data)),maxiter=50,
           trylist= rep(list(NULL),initnw$n),trace=TRUE,
           timetrace=TRUE,showban=FALSE,saveall=FALSE) 

heuristic(initnw,data,prior=jointprior(network(data)),
          maxiter=100,restart=10,degree=initnw$n,
          trylist= rep(list(NULL),initnw$n),trace=TRUE,
          timetrace=TRUE,saveall=FALSE)

modelstreng(x)
makenw(tb,template) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{initnw}{The initial network from which the search is started. }
  \item{data}{Data frame used for learning the network, see \code{\link{network}}.}
  \item{prior}{A joint prior, see \code{\link{jointprior}}.}
  \item{maxiter}{Maximum search steps in the search algorithm.}
  \item{restart}{The number of times to perturb initnw and rerun the
    search.}
  \item{degree}{Degree of perturbation, see \code{\link{perturb}}.}
  \item{trylist}{Used internally for reusing learning of nodes, see \code{\link{maketrylist}}.}
  \item{trace}{If TRUE, plots the accepted networks during search.}
  \item{timetrace}{If TRUE, prints some timing information on the screen.}
%  \item{smalldf}{Used for analysing networks with time varying nodes,
%    see \code{\link{timeslice}}.}
  \item{showban}{Passed to \code{\link{plot.network}}.}
  \item{saveall}{If TRUE, all networks explored in the search is
    returned (memory consuming). Otherwise, only the best is returned.}
  \item{x}{A network object to be translated into a string.}
  \item{tb}{A table output from autosearch or heuristic. Can be
    translated into a networkfamily.}
  \item{template}{The initial network being analysed.}
}

\details{
  In \code{\link{autosearch}}, each step, a list of networks is
  created with either one
  arrow added, one arrow deleted or one arrow turned (if a cycle is not
  generated). The network scores of all the proposal networks are
  calculated and the network with the highest score is chosen for the
  next step in the search. If no proposed networks has a higher network
  score than the previous network, the search is terminated. The
  returned network list contains all tried networks (depending on the
  value of saveall).

  \code{heuristics} restarts by perturbing \code{initnw}
  \code{degree} times and calling 
  \code{autosearch} again. The number
  of restarts is given by the option \code{restart}.
}

\value{A list with three elements
  \item{nw}{The network with highest score}
  \item{tabel}{A table with all tried
    networks or only the some of the networks if saveall is
    \code{FALSE}. The table can be translated to a networkfamily using makenw.} 
  \item{trylist}{Updated, \code{\link{learn}}.}
}

\seealso{\code{\link{perturb}}
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
fit       <- network(rats)
fit.prior <- jointprior(fit,12)
fit       <- learn(fit,rats,fit.prior)$nw
fit       <- insert(fit,2,1,rats,fit.prior)$nw
fit       <- insert(fit,1,3,rats,fit.prior)$nw
hisc      <- autosearch(fit,rats,fit.prior)$nw
}

\keyword{models}
