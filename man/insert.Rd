%                               -*- Mode: Rd -*- 
% insert.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:02:17 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Thu Jan 16 14:20:03 2003
% Update Count    : 8
% Status          : Unknown, Use with caution!
% 

\name{insert}
\alias{insert}
\alias{remover}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Insert/remove an arrow in network}

\description{Inserts/removes one arrow in a network (if legal)
}

\usage{
insert (nw,j,i,df,prior,nocalc=FALSE,trylist=rep(list(NULL),nw$n)) 
remover(nw,j,i,df,prior,nocalc=FALSE,trylist=rep(list(NULL),nw$n)) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{A network.}
  \item{j}{The 'from' node.}
  \item{i}{The 'to' node.}
  \item{df}{Data Frame used for learning the network, see \code{\link{network}}.}
  \item{prior}{A joint prior, see \code{\link{jointprior}}.}
  \item{nocalc}{If TRUE, learning is not called.}
  \item{trylist}{Used internally for reusing learning of nodes, see \code{\link{maketrylist}}.}
%  \item{smalldf}{Used for analysing networks with time varying nodes,
%    see \code{\link{timeslice}}.}
}

\details{
  Examines if the arrow from \code{j} to \code{i} is legal according to
  the following criteria
  \item{1.}{Arrows from/to the same node are not legal.}
  \item{2.}{Arrows from continous nodes to discrete nodes are not legal.}
%  \item{3.}{Arrows from time varying nodes to static nodes are not
%    legal, see \code{\link{timeslice}}.}
  \item{3.}{Arrows banned in ban list are not legal, see \code{\link{drawnetwork}}.}
  \item{4.}{Arrows already existing in the network are not legal.}

  If the arrow is not legal, a \code{NULL} network is returned. Otherwise, the
  arrow is inserted/removed, the network is re-learned (if
  nocalc=F). The trylist is updated.
}

\value{A list with two elements
  \item{nw}{A network family with all created networks.}
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
rats.nw    <- network(rats)
rats.nw    <- insert(rats.nw,2,1,nocalc=TRUE)$nw
rats.prior <- jointprior(rats.nw,12)

rats.nw2   <- network(rats)
rats.nw2   <- learn(rats.nw2,rats,rats.prior)$nw
rats.nw2   <- insert(rats.nw2,1,2,rats,rats.prior)$nw
}

\keyword{iplot}
