%                               -*- Mode: Rd -*- 
% postdist.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:02:26 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Thu Jul 24 15:33:25 2003
% Update Count    : 19
% Status          : Unknown, Use with caution!
% 

\name{postdist}
\alias{postdist}
\alias{postdist.node}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Calculate point estimate of posterior parameters and create probability distribution}

\description{After finding (learning) posterior parameters, the mean (or mode)
of the distributions are found and are used to update the local
probability distributions
}

\usage{
postdist(nw) 
postdist.node(nd,nw,vtype = "mode")
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{an object of class \code{\link{network}}.}
  \item{nd}{an object of class \code{\link{node}}.}
  \item{vtype}{a string, taking values "mode" or "mean" of the variance distribution.}
}

\details{
	In the learning procedure, the \code{postdist.node} procedure is
	called to ensure that the \code{prob} property for each node is
	updated. The \code{prob} property
	is first set in \code{\link{jointprior}} and is then updated
	using \code{postdist} to update them in the learning
	procedures. The \code{prob} property is used by
	\code{\link{savenet}} for exporting a network to the \code{.net}
	format.} 

\seealso{\code{\link{learn}},
\code{\link{savenet}},
\code{\link{jointprior}},
\code{\link{network}}
}

\value{an object of class \code{\link{network}} with updated \code{prob}
  properties for each node. 
}

\references{
  Further information about \bold{deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}


\keyword{iplot}
