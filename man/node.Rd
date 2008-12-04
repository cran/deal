%                               -*- Mode: Rd -*- 
% node.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:31 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Thu Dec 04 13:16:49 2008
% Update Count    : 84
% Status          : Unknown, Use with caution!
% 
\encoding{latin1}
\name{node}
\alias{node}
\alias{plot.node}
\alias{print.node}
\alias{nodes}
\alias{nodes<-}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Representation of nodes}

\description{An important part of a \code{\link{network}} is the list of
  nodes. The nodes summarize the local properties of a node, given the
  parents of the node.
}

\usage{
node (idx,parents,type="discrete",name=paste(idx),
                 levels=2,levelnames=paste(1:levels),position=c(0,0)) 
\method{print}{node} (x,filename=NA,condposterior=TRUE,condprior=TRUE,...) 
\method{plot}{node} (x,cexscale=10,notext=FALSE,...)
nodes(nw)
value <- nodes(nw)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{an object of class \code{node}.}
  \item{parents}{a numeric vector with indices of the parents of the node.}
  \item{idx}{an integer, which gives the index of the node (the column number of the
    corresponding data frame).}
  \item{type}{a string, which gives the type of the node. Either
    \code{"discrete"} (for factors) or \code{"continuous"} (for numeric).}
  \item{name}{a string, which gives the name used when plotting and
    printing. Defaults to the column 
    name in the data frame.}
  \item{levels}{an integer. If \code{type} is \code{"discrete"}, this is
    the number of levels for the discrete variable.}
  \item{levelnames}{if \code{type} is \code{"discrete"}, this is a vector of
    strings (same length as \code{levels}) with the names of the
    levels. If \code{type} is \code{"continuous"}, the argument is ignored.}
  \item{position}{a numeric vector with coordinates where the node should
    appear in the 
    plot. Usually set by \code{\link{network}} and \code{\link{drawnetwork}}.}
  \item{nw}{an object of class \code{\link{network}}.}
  \item{value}{a list of elements of class \code{\link{node}}.}
  \item{filename}{a string or \code{NA}. If not \code{NA}, output is
    printed to a file.} 
  \item{condprior}{a logical. If \code{TRUE}, the conditional prior is
    printed, see \code{\link{conditional}}.}
  \item{condposterior}{a logical. If \code{TRUE}, the conditional posterior is
    printed, see \code{\link{learn}}.}
  \item{cexscale}{a numeric. Scale parameter to set the size of the nodes.}
  \item{notext}{a logical. If \code{TRUE}, no text is displayed in the nodes on the plot.}
  \item{\dots}{additional plot arguments.}
}

\details{
  The operations on a node are typically done when operating on a
  \code{\link{network}}, so these functions are not to be called
  directly. 

  When a network is created with \code{network}, the nodes in the
  nodelist are created using the \code{node} procedure.

  Local
  probability distributions are added as the property \code{prob} to
  each node using \code{prob.node}. If the node is continuous, this is a
  numeric vector with 
  the conditional variance and the conditional regression coefficients
  arising from a regression on the continuous parents, using data. If
  the node has discrete parents, \code{prob} is a matrix with a row
  for each configuration of the discrete parents. If the node is
  discrete, \code{prob} is a multiway array which gives the conditional
  probability distribution for each configuration of the discrete
  parents. The generated \code{prob} can be replaced to match the prior
  information available.

  \code{nodes} gives the list of nodes of a network. \code{localprob}
  gives the probability distribution for each node in the network.
  }

\value{
  The \code{node} creator function returns an object of class
  \code{node}, which is a list with the following 
  elements (properties),
  \item{idx}{an integer. A unique index for this node. It MUST
    correspond to the column index of the variable in the data frame.}
  \item{name}{a string. The printed name of the node.}
  \item{type}{a string. Either \code{"continuous"} or \code{"discrete"}.}
  \item{levels}{an integer. If the node is of type \code{"discrete"}, this
    integer is the 
    number of levels of the node.}
  \item{levelnames}{if \code{type} is \code{"discrete"}, this is a vector of
    strings (same length as \code{levels}) with the names of the
    levels. If \code{type} is \code{"continuous"}, the node does not have this property.}
  \item{parents}{a vector of indices of the parents to this node. It is
    best to manage this vector using the \code{\link{insert}} function.}
  \item{prob}{a numeric vector, matrix or multiway array, giving the
    initial probability distribution. If the node is discrete,
    \code{prob} is a multiway array. If the node is continuous,
    \code{prob} is a matrix with one row for each configuration of the
    discrete parents, reducing to a vector if the node has no discrete parents.}
  \item{condprior}{a list, generated by \code{\link{conditional}} giving
    the parameter priors deduced from
    \code{\link{jointprior}} using the master prior procedure (see
    \code{\link{localmaster}}). }
  \item{condposterior}{a list, which gives the parameter posteriors obtained from
    \code{\link{learnnode}}.}
  \item{loglik}{a numeric giving the log likelihood contribution for this node,
    calculated in \code{\link{learnnode}}.}
  \item{simprob}{a numeric vector, matrix or multiway array similar to \code{prob}, added by
    \code{\link{makesimprob}} and used by \code{\link{rnetwork}}.}
}

\references{
  Further information about \bold{deal} can be found at:\cr
  \url{http://www.math.aau.dk/~dethlef/novo/deal}.
}

\author{
  Susanne Gammelgaard Bottcher \email{alma@math.aau.dk}, \cr
  Claus Dethlefsen \email{cld@rn.dk}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\keyword{models}
