%                               -*- Mode: Rd -*- 
% node.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:31 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon Feb 03 10:07:01 2003
% Update Count    : 36
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
  nodes. The nodes summarize the local properties of a node given the
  parents of the node.
}

\usage{
node (idx,parents,type,name=paste(idx),
                 levels=2,levelnames=paste(1:levels),position=c(0,0)) 
\method{print}{node} (x,filename=NA,master=FALSE,condposterior=TRUE,condprior=TRUE,...) 
\method{plot}{node} (x,cexscale=10,notext=FALSE,scale=10,...)
prob.node (x,nw,df)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{Node.}
  \item{parents}{A vector with indices of the parents of the node.}
  \item{idx}{The index of the node (the column number of the
    data frame).}
  \item{type}{\code{"discrete"} or \code{"continuous"}.}
  \item{name}{The name used when plotting and printing. Defaults to the column
    name in the data frame.}
  \item{levels}{If \code{type="discrete"}, this is the number of levels
    for the discrete variable.}
  \item{levelnames}{If \code{type="discrete"} this is a vector of
    strings (same size as \code{levels}) with the names of the
    levels.}
  \item{position}{The coordinates where the node should appear in the
    plot. Usually set by \code{\link{network}} and \code{\link{drawnetwork}}.}
  \item{df}{A data frame, where the columns define the variables. A
    continuous variable should have type "numeric" and discrete varibles
    should have type "factor".}
%  \item{equalcases}{When calculating the probability distribution for
%    discrete variables,(if \code{TRUE}) set the probability
%    equal for all configurations of a node given its parents. If
%\code{FALSE}, observed frequencies are used. }
%  \item{vif}{Variance inflation factor. Can be used to adjust the variance for the continuous variables.}
%  \item{tvar}{Pinpoint time varying nodes, see \code{\link{timeslice}}.}
%  \item{smalldf}{For use with time varying nodes, see \code{\link{timeslice}}.}
  \item{nw}{The network.}
  \item{filename}{If a string argument is given, output is printed to a file.}
  \item{condprior}{Print condprior? \code{TRUE}/\code{FALSE}}
  \item{condposterior}{Print condposterior? \code{TRUE}/\code{FALSE}}
  \item{master}{Print master? \code{TRUE}/\code{FALSE}.}
  \item{scale}{Used to position node.}
  \item{cexscale}{The size of the nodes.}
  \item{notext}{If \code{FALSE}, do not display text on plots.}
  \item{\dots}{Passed to plot.}
}


\value{
  A node object may have the following attributes
  \item{idx}{A unique index for this node. It MUST correspond to the
    column index of the variable in the data frame.}
  \item{name}{The printed name of the node.}
  \item{type}{Either \code{continuous} or \code{discrete}.}
  \item{levels}{If the node is of type discrete, this integer is the
    number of levels of the node.}
  \item{levelnames}{If the node is of type discrete, this vector of
    strings is the names of the levels. This must have length equal to
    levels.} 
  \item{parents}{A vector of indices of the parents to this node. It is
    best to manage this vector using the \code{\link{insert}} function.}
  \item{prob}{The initial probability distribution, set by
    \code{\link{network}}.}
  \item{condprior}{The parameter priors deduced from
    \code{\link{jointprior}} using the master prior procedure (see
    \code{\link{localmaster}}). }
  \item{condposterior}{The parameter posteriors obtained from
    \code{\link{learnnode}}.}
  \item{loglik}{The log likelihood contribution for this node,
    calculated in \code{\link{learnnode}}.}
%  \item{tvar}{If missing, the node is not time varying.}
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
