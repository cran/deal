%                               -*- Mode: Rd -*- 
% maketrylist.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:02:52 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Sat May 24 12:41:06 2003
% Update Count    : 6
% Status          : Unknown, Use with caution!
% 

\name{maketrylist}
\alias{maketrylist}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Creates the full trylist}

\description{For faster learning, a trylist is maintained.}

\usage{
maketrylist(initnw,data,prior=jointprior(network(data)),timetrace=FALSE) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{initnw}{The network}
  \item{data}{Data Frame}
  \item{prior}{Joint prior}
  \item{timetrace}{For timing purposes}
}

\details{For each node in the network, all possible parent
  configurations are created and learned. The result is called a
  trylist. This is very time-consuming,
  and a better choice is to maintain a trylist while searching and
  indeed this is automatically done. 
}

\value{A list with one element per node in the network. Element \emph{i}
is a list of nodes. These nodes are copies of node \emph{i} in the
network, except that all possible parent configurations are
present. For each copy, the node is learned so that the parameter
posteriors and 
log-likelihood contribution are calculated. These nodes can then be
copied into networks where the node appears.}


\references{
  Further information about \bold{Deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}

\note{The full trylist is also output from \code{\link{networkfamily}}.}


\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

\seealso{ \code{\link{reuselearn}},
  \code{\link{networkfamily}},
  \code{\link{heuristic}}
  }


  \examples{
data(rats)
rats.nw <- network(rats)
rats.pr <- jointprior(rats.nw,12)
rats.nw <- learn(rats.nw,rats,rats.pr)$nw
rats.tr <- maketrylist(rats.nw,rats,rats.pr)

#rats.hi <- heuristic(rats.nw,rats,rats.pr,trylist=rats.tr)$nw

}

\keyword{models}
