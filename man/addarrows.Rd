%                               -*- Mode: Rd -*- 
% addarrows.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:01:19 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Tue Jan  7 09:25:26 2003
% Update Count    : 6
% Status          : Unknown, Use with caution!
% 

\name{addarrows}
\alias{addarrows}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Add arrows to/from node}

\description{Create networks with all possible arrows to/from nodes with
smaller indices added}

\usage{
addarrows(nw, node, data, prior,trylist=rep(list(NULL),nw$n))
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{Network in which arrows are inserted.}
  \item{node}{The index of the node in focus.}
  \item{data}{Data frame used for learning the network, see \code{\link{network}}.}
  \item{prior}{A joint prior, see \code{\link{jointprior}}.}
  \item{trylist}{Used internally for reusing learning of nodes, see \code{\link{maketrylist}}.}
}

\details{Used in \code{\link{networkfamily}} when constructing all possible networks
  for a given node configuration. Tries to insert all possible arrows to
  (from) the node in focus from (to) nodes with smaller indices. Each
  arrow combination gives rise to extra networks. 
}

\value{A list with two elements
  \item{nw}{A network family with all created networks. All networks have
  been learned.}
  \item{trylist}{Updated.}
}

\seealso{\code{\link{networkfamily}},\code{\link{addarrow}}}

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~
\references{
  Further information about \bold{Deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}

\examples{
data(rats)
rats.nw    <- network(rats)
rats.prior <- jointprior(rats.nw,12)
rats.nwl   <- addarrows(rats.nw,3,rats,rats.prior)$nw
\dontrun{plot(rats.nwl)}
}

\keyword{iplot}
