%                               -*- Mode: Rd -*- 
% network.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:21 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Thu Dec 04 13:16:33 2008
% Update Count    : 89
% Status          : Unknown, Use with caution!
% 

\name{network}
\alias{network}
\alias{plot.network}
\alias{print.network}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Bayesian network data structure}

\description{
  A Bayesian network is represented as an object of class
  \code{network}. Methods for printing and plotting are defined. 
}
\encoding{latin1}
\usage{
network(df,specifygraph=FALSE,inspectprob=FALSE,
        doprob=TRUE,yr=c(0,350),xr=yr) 
\method{print}{network}(x,filename=NA,condposterior=FALSE,
                          condprior=FALSE,...) 
\method{plot}{network} (x,arrowlength=.25,
                        notext=FALSE,
                        sscale=7,showban=TRUE,yr=c(0,350),xr=yr,
                        unitscale=20,cexscale=8,...)


}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{df}{a data frame, where the columns define the variables. A
    continuous variable should have type \code{numeric} and discrete varibles
    should have type \code{\link[base]{factor}}.}
  \item{specifygraph}{a logical. If \code{TRUE}, provides a call to
    \code{\link{drawnetwork}} to interactively specify a directed
    acyclic graph and possibly a ban list (see below).}
  \item{inspectprob}{a logical. If \code{TRUE}, provides a plot of the
    graph and possibility to inspect the calculated probability
    distribution by clicking on the nodes.}
  \item{doprob}{a logical. If \code{TRUE}, do not calculate a
    probability distribution. Used 
    for example in \code{\link{rnetwork}}. }
  \item{x}{an object of class \code{\link{network}}.}
  \item{filename}{a string or \code{NA}. If not \code{NA}, output is
    printed to a file.} 
  \item{condprior}{a logical. If \code{TRUE}, the conditional prior is
    printed, see \code{\link{conditional}}.}
  \item{condposterior}{a logical. If \code{TRUE}, the conditional posterior is
    printed, see \code{\link{learn}}.}
  \item{sscale}{a numeric. The nodes are initially placed on a circle
    with radius \code{sscale}.} 
  \item{unitscale}{a numeric. Scale parameter for chopping off arrow heads.}
  \item{cexscale}{a numeric. Scale parameter to set the size of the nodes.}
  \item{arrowlength}{a numeric containing the length of the arrow heads.}
  \item{xr}{a numeric vector with two components containing the range on x-axis.}
  \item{yr}{a numeric vector with two components containing the range on y-axis.}
  \item{notext}{a logical. If \code{TRUE}, no text is displayed in the nodes on the plot.}
  \item{showban}{a logical. If \code{TRUE}, banned arrows are shown in red.}
  \item{\dots}{additional plot arguments, passed to \code{\link{plot.node}}.}
}


\value{
  The \code{netork} creator function returns an object of class
  \code{network}, which is a list with the following 
  elements (properties),
  \item{nodes}{a list of objects of class \code{node}. If
    \code{doprob} is \code{TRUE}, the nodes are given the 
    property \code{prob} which is the initial probability distribution used
    by \code{\link{jointprior}}.}
  \item{n}{an integer containing the number of nodes in the network.}
  \item{discrete}{a numeric vector of indices of discrete nodes.}
  \item{continuous}{a numeric vector of indices of continuous nodes.}
  \item{banlist}{a numeric matrix with two columns. Each row contains the
    indices \code{i -> j} of arrows that may not be allowed in the
    directed acyclic graph.}
  \item{score}{a numeric added by \code{\link{learn}} and is the log network
    score.}
  \item{relscore}{a numeric added by \code{\link{nwfsort}} and is the relative
     network score -- compared with the best network in a network family.}
}

\seealso{\code{\link{networkfamily}},
  \code{\link{node}},
  \code{\link{rnetwork}},
  \code{\link{learn}},
  \code{\link{drawnetwork}},
  \code{\link{jointprior}},
  \code{\link{heuristic}},
  \code{\link{nwequal}}
  }

% \references{
%   Further information about \bold{deal} can be found at:\cr
%   \url{http://www.math.aau.dk/~dethlef/novo/deal}.
% }

\author{
  Susanne Gammelgaard Bottcher, \cr
  Claus Dethlefsen \email{rpackage.deal@gmail.com}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\examples{
A <- factor(rep(c("A1","A2"),50))
B <- factor(rep(rep(c("B1","B2"),25),2))
thisnet <- network( data.frame(A,B) )

set.seed(109)
sex     <- gl(2,4,label=c("male","female"))
age     <- gl(2,2,8)
yield   <- rnorm(length(sex))
weight  <- rnorm(length(sex))
mydata  <- data.frame(sex,age,yield,weight)
mynw    <- network(mydata)

# adjust prior probability distribution
localprob(mynw,"sex")   <- c(0.4,0.6)
localprob(mynw,"age")   <- c(0.6,0.4)
localprob(mynw,"yield") <- c(2,0)
localprob(mynw,"weight")<- c(1,0)

print(mynw)
plot(mynw)

prior <- jointprior(mynw)
mynw  <- getnetwork(learn(mynw,mydata,prior))
thebest <- getnetwork(autosearch(mynw,mydata,prior))

print(mynw,condposterior=TRUE)

\dontrun{savenet(mynw,file("yield.net"))}
}

\keyword{models}
