%                               -*- Mode: Rd -*- 
% networkfamily.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:26 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Wed Sep 18 08:46:28 2002
% Update Count    : 8
% Status          : Unknown, Use with caution!
% 

\name{networkfamily}
\alias{networkfamily}
\alias{print.networkfamily}
\alias{plot.networkfamily}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Collection of networks}

\description{Methods for handling a range of networks. These may be
  plottet, printed or output in LaTeX format. Also functions for sorting
  and thinning on the basis of the network-score is defined.
}

\usage{
networkfamily(data,nw=network(data), prior=jointprior(nw),
              trylist=rep(list(NULL),nw$n), timetrace=TRUE) 

\method{print}{networkfamily}(x,...)
\method{plot}{networkfamily}(x,
        layout=rep(min(1+floor(sqrt(length(nwf))),5),2),
        cexscale=5,length=0.1,scale=10,sscale=.7*scale,...) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{The inital (often without arrows) network from which the
    full networkfamily is generated.}
  \item{data}{Dataframe used for learning the parameters in each network.}
  \item{prior}{A joint prior. See \code{\link{jointprior}}.}
  \item{trylist}{Used internally for reusing learning of nodes. See
    \code{\link{maketrylist}}. }
  \item{timetrace}{For timing purposes.}
  \item{x}{A networkfamily.}
  \item{layout}{2-dimensional vector of the number of plots in the rows
    and columns of each plotting page.}
  \item{cexscale}{See \code{\link{plot.network}}}
  \item{length}{See \code{\link{plot.network}}}
  \item{scale}{See \code{\link{plot.network}}}
  \item{sscale}{See \code{\link{plot.network}}}
  \item{...}{Other plot parameters passed to \code{\link{plot.network}}.}
}

\details{
  \code{networkfamily} generates and learns all possible networks with
  the nodes given as in the initial network \code{nw}. This is done by
  successively trying to generate the networks with all possible arrows
  to/from each node (see \code{\link{addarrows}}). If there is a banlist
  present in \code{nw} (see \code{\link{network}}), then this is
  respected.

  After generation of all possible networks, a test for cycles (see
  \code{\link{cycletest}}) is performed and only networks with directed
  acyclic graphs are returned.

}

\value{The function \code{networkfamily} returns list with two components
  \item{nw}{The networkfamily.}
  \item{trylist}{An updated trylist (see \code{\link{maketrylist}}).}
}

\note{No checks are made as to whether the networks have the same number
  of nodes, the same node-names, etc. It is indeed possible to define
  networkfamilies with networks with different structures.

  Generating all possible networks can be \emph{very} time consuming!
}

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

\references{
  Further information about \bold{Deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}


\seealso{\code{\link{network}},
  \code{\link{genlatex}},
  \code{\link{heuristic}},
  \code{\link{nwfsort}},
  \code{\link{nwfunique}},
  \code{\link{elementin}},
  \code{\link{addarrows}},
  \code{\link{cycletest}}
}

\examples{
data(rats)
allrats <- networkfamily(rats)$nw
\dontrun{plot(allrats)}
\dontrun{print(allrats)}
}

\keyword{iplot}
