%                               -*- Mode: Rd -*- 
% postdist.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:02:26 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Tue Jan 07 12:43:56 2003
% Update Count    : 14
% Status          : Unknown, Use with caution!
% 

\name{postdist}
\alias{postdist}
\alias{postdist.node}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Calculate mean of posterior parameters and create probability distribution}

\description{After finding (learning) posterior parameters, the mean
of the distributions are found and used to update the local
probability distributions
}

\usage{
postdist(nw) 
postdist.node(nd,nw,vtype = "mode")
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{The network.}
  \item{nd}{The node to update.}
  \item{vtype}{Report "mode" or "mean" of the variance distribution.}
}

\details{
	In the learning procedure, the postdist.node is called to
	ensure that the prob-attribute is updated. The prob-attribute
	is first set in \code{\link{jointprior}} and is then updated
	using these functions to update them in the learning
	procedures. The prob attribute is used by
	\code{\link{savenet}} for exporting in .net format.}

\seealso{\code{\link{learn}},
\code{\link{savenet}},
\code{\link{jointprior}},
\code{\link{network}}
}

\value{A network with updated prob attributes for each node.
}

\references{
  Further information about \bold{Deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}


\keyword{iplot}
