%                               -*- Mode: Rd -*- 
% network.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:21 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon Feb 03 10:05:52 2003
% Update Count    : 32
% Status          : Unknown, Use with caution!
% 

\name{network}
\alias{network}
\alias{plot.network}
\alias{prob.network}
\alias{print.network}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Bayesian network data structure}

\description{
  A Bayesian network is represented as an object of class
  \code{network}. Methods for printing and plotting are defined. 
}

\usage{
network(df,specifygraph=FALSE,inspectprob=FALSE,
        doprob=TRUE,yr=c(0,350),xr=yr) 
\method{print}{network}(x,filename=NA,master=FALSE,condposterior=FALSE,
                          condprior=FALSE,...) 
\method{plot}{network} (x,scale=10,arrowlength=.25,
                        notext=FALSE,
                        sscale=.7*scale,showban=TRUE,yr=c(0,350),xr=yr,
                        unitscale=20,cexscale=8,...)

prob.network (x,df) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{df}{A data frame, where the columns define the variables. A
    continuous variable should have type \code{numeric} and discrete varibles
    should have type \code{\link[base]{factor}}.}
  \item{specifygraph}{Interactively specify a directed acyclic graph and
    possibly a ban list (see below).}
  \item{inspectprob}{Inspect the calculated probability
    distribution using a graphical interface.}
%  \item{equalcases}{When calculating the probability distribution for
%    discrete variables,(if \code{TRUE}) set the probability
%    equal for all configurations of a node given its parents. If
%\code{FALSE}, observed frequencies are used. }
%  \item{vif}{Variance inflation factor. Can be used to adjust the variance for the continuous variables.}
  \item{doprob}{If \code{TRUE}, do not calculate a probability distribution. Used
    for example in \code{\link{simulation}}. }
%  \item{tvar}{Vector of indices of time varying nodes, see \code{\link{timeslice}}.}
%  \item{smalldf}{For use with time varying networks, see \code{\link{timeslice}}.}
  \item{x}{The network.}
  \item{filename}{If a string argument is given, output is printed to a file.}
  \item{condprior}{Print condprior? \code{TRUE}/\code{FALSE}}
  \item{condposterior}{Print condposterior? \code{TRUE}/\code{FALSE}}
  \item{master}{Print master? \code{TRUE}/\code{FALSE}.}
  \item{scale}{The axes in network plots are from \code{-scale} to
    \code{scale}. }
  \item{sscale}{The nodes are placed on a circle with radius \code{sscale}.}
  \item{unitscale}{Scale parameter for chopping off arrow heads.}
  \item{cexscale}{The size of the nodes.}
  \item{arrowlength}{The length of the arrow heads.}
  \item{xr}{Range of x-axis.}
  \item{yr}{Range of y-axis.}
  \item{notext}{No text is displayed in the nodes on the plot if notext
    is \code{TRUE}}
  \item{rotate}{The nodes are placed on a circle and they may be rotated
  by this number (in radians).}
  \item{showban}{If banned arrows should be shown (they are shown in red).}
  \item{\dots}{Passed to \code{\link{plot.node}}.}
}


\value{
  A network object with attributes
  \item{nodes}{A list of nodes. If \code{doprob==TRUE}, the nodes are given the
    attribute \code{prob} which is the initial probability distribution used
    by \code{\link{jointprior}}.}% Arguments \code{equalcases} and
%    \code{vif} are used to calculate \code{prob}.}
  \item{n}{The number of nodes.}
  \item{discrete}{A vector of indices of discrete nodes.}
  \item{continuous}{A vector of indices of continuous nodes.}
  \item{banlist}{A matrix with two columns. Each row contains the
    indices \code{i -> j} of arrows that may not be allowed in the
    directed acyclic graph.}
  \item{score}{Added by \code{\link{learn}} and is the log network
    score.}
  \item{relscore}{Added by \code{\link{nwfsort}} and is the relative
    log network score -- compared with the best network in a network family.}
}

\seealso{\code{\link{networkfamily}},
  \code{\link{node}},
  \code{\link{simulation}},
  \code{\link{learn}},
  \code{\link{drawnetwork}},
  \code{\link{jointprior}},
  \code{\link{heuristic}},
  \code{\link{nwequal}}
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
