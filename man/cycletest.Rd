%                               -*- Mode: Rd -*- 
% cycletest.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:01:39 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Tue Jul 22 15:34:03 2003
% Update Count    : 4
% Status          : Unknown, Use with caution!
% 

\name{cycletest}
\alias{cycletest}
\alias{findleaf}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Test if network contains a cycle}

\description{The graph in a Bayesian network must be a directed acyclic
  graph. The procedure \code{cycletest} returns \code{TRUE} or
  \code{FALSE} if it is cyclic or not. This is used in \code{\link{autosearch}}}

\usage{
cycletest(nw)
findleaf (nw) 
}


%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{an object of class \code{\link{network}}}
}

\details{
  First, \code{cycletest} uses \code{findleaf} to find a node without
  parents (called a leaf). Then, this node is deleted from the graph. If
  this process can be repeated until all nodes have been deleted, no
  cycle is present in the graph.

  The procedure \code{findleaf} returns the index of the first found
  leaf. If no leaf is found, 0 is returned.
}

\value{
  \code{cycletest} returns \code{TRUE} if \code{nw} contains a cycle, \code{FALSE} if not.

  \code{findleaf} returns an index of a leaf, or 0 if no leaf is found.
}

\references{
  Further information about \bold{deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

\seealso{\code{\link{networkfamily}},
  \code{\link{drawnetwork}},
  \code{\link{autosearch}},
  \code{\link{perturb}}
  }



\examples{
data(rats)

# make Sex numeric so that a cycle is possible
rats$Sex <- as.numeric(rats$Sex)

# define network and prior and learn
nw <- network(rats)
nw.prior <- jointprior(nw,12)
nw <- learn(nw,rats,nw.prior)$nw

# insert a cycle
nw <- insert(nw,1,3,rats,nw.prior)$nw
nw <- insert(nw,3,4,rats,nw.prior)$nw
nw <- insert(nw,4,1,rats,nw.prior)$nw

cycletest(nw)
}

\keyword{iplot}
