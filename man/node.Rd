%                               -*- Mode: Rd -*- 
% node.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:31 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon Sep 16 16:00:01 2002
% Update Count    : 15
% Status          : Unknown, Use with caution!
% 

\name{node}
\alias{node}
\alias{plot.node}
\alias{prob.node}
\alias{print.node}
\alias{prob}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Nodes}

\description{An important part of a \code{\link{network}} is the list of
  nodes. The nodes summarizes the local properties of the node given the
  parents of the node.
}

\usage{
node (idx,parents,type,name=paste(idx),
                 levels=2,levelnames=paste(1:levels)) 
\method{print}{node} (x,filename=NA,master=FALSE,condposterior=TRUE,condprior=TRUE,...) 
\method{plot}{node} (x,where=c(0,0),cexscale=10,notext=FALSE,...)
\method{prob}{node} (x,nw,df,equalcases=FALSE,vif=1.0,smalldf=NA)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{Node}
  \item{parents}{A vector with indices of the parents of the node.}
  \item{idx}{The index of the node (the column number of the
    dataframe).}
  \item{type}{\code{"discrete"} or \code{"continuous"}.}
  \item{name}{The name used in when plotting and printing. Defaults to the column
    name in the dataframe.}
  \item{levels}{If \code{type="discrete"}, this is the number of levels
    for the discrete variable.}
  \item{levelnames}{If \code{type="discrete"} this is a vector of
    strings (as long as \code{levels}) with the names of the
    levels.}
  \item{where}{The coordinates where the node should appear in the
    plot. Usually set by \code{\link{plot.network}}.}
  \item{df}{A dataframe, where the columns define the variables. A
    continuous variable should have type "numeric" and discrete varibles
    should have type "factor".}
  \item{equalcases}{When calculating the probability distribution for
    discrete variables, do not (if \code{TRUE}) set the probability
    equal to the 
    observed frequency. If \code{FALSE}, observed frequencies are used. }
  \item{vif}{Variance inflation factor. For continuous nodes, the
    probability distribution is assumed normal with a mean depending on
    the parents and a variance for each configuration of the discrete
    parents. The mean and variance are deduced from data as a linear
    regression, but the variance is multiplied by this vif.}
  \item{tvar}{Pinpoint time-varying nodes, see \code{\link{timeslice}}.}
  \item{smalldf}{For use with timevarying nodes, see \code{\link{timeslice}}.}
  \item{nw}{The network.}
  \item{filename}{Print to file.}
  \item{condprior}{Print condprior?}
  \item{condposterior}{Print condposterior?}
  \item{master}{Print master?}
  \item{cexscale}{The size of the nodes.}
  \item{notext}{If \code{FALSE}, do not display text on plots.}
  \item{\dots}{Passed to plot.}
}


\value{
  A node object may have the following attributes
  \item{idx}{A unique index for this node. It MUST correspond to the
    column index of the variable in the dataframe.}
  \item{name}{The printed name of the node.}
  \item{type}{Either \code{continuous} or \code{discrete}.}
  \item{levels}{If the node is of type discrete, this integer is the
    number of levels of the node.}
  \item{levelnames}{If the node is of type discrete, this vector of
    strings are the names of the levels. This must have length equal to
    levels.} 
  \item{parents}{A vector of indices of the parents to this node. It is
    best to manage this vector using the \code{\link{insert}} function.}
  \item{prob}{The initial probability distribution, set by
    \code{\link{network}}.}
  \item{condprior}{The parameter priors deduced from
    \code{\link{jointprior}} using the master prior procedure (see
    \code{\link{localmaster}}). }
  \item{condposterior}{The parameter posteriors obtained from
    \code{\link{learn.node}}.}
  \item{loglik}{The log-likelihood contribution for this node,
    calculated in \code{\link{learn.node}}.}
  \item{tvar}{If missing, the node is not timevarying.}
  \item{simprob}{Added by \code{\link{makesimprob}} and used by
    \code{\link{simulation}}.}
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
A <- factor(rep(c("A1","A2"),50))
B <- factor(rep(rep(c("B1","B2"),25),2))
thisnet <- network( data.frame(A,B) )
}

\keyword{models}
