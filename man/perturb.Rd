%                               -*- Mode: Rd -*- 
% perturb.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:54 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon Sep 16 18:48:26 2002
% Update Count    : 3
% Status          : Unknown, Use with caution!
% 

\name{perturb}
\alias{perturb}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Perturb a network}

\description{Randomly insert/delete/turn arrows to obtain another network
}

\usage{
perturb(nw,data,prior,degree=nw$n,trylist=rep(list(NULL),nw$n),nocalc=FALSE,timetrace=TRUE,smalldf=NA) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{The network from which arrows are added/removed/turned.}
  \item{data}{Dataframe used for learning the network.}
  \item{prior}{A joint prior. See jointprior().}
  \item{degree}{The number of attempts to randomly insert/remove/turn an
    arrow.}
  \item{trylist}{Used internally for reusing learning of nodes.}
  \item{nocalc}{If T, the network score is not updated}
  \item{timetrace}{Timing information available.}
  \item{smalldf}{Used for analysing networks with time-varying nodes.}
}

\details{
  Given the starting network, a new network is constructed by randomly
  choosing an action: remove, turn, add. After the action is chosen, we
  choose randomly among all possibilities of that action. If there are
  no possibilites, the unchanged network is returned.}

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

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\examples{
data(rats)
fit       <- network(rats,specifygraph=FALSE,inspect=FALSE)
fit.prior <- jointprior(fit,12)
fit       <- learn(fit,rats,fit.prior)$nw
fit.new   <- perturb(fit,rats,fit.prior,degree=10)$nw
}

\keyword{iplot}
