%                               -*- Mode: Rd -*- 
% network.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:21 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon Jul 28 10:05:16 2003
% Update Count    : 71
% Status          : Unknown, Use with caution!
% 

\name{network}
\alias{network}
\alias{plot.network}
\alias{prob.network}
\alias{print.network}
\alias{banlist}
\alias{"banlist<-"}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Bayesian network data structure}

\description{
  A Bayesian network is represented as an object of class
  \code{network}. Methods for printing and plotting are defined. 
}

\usage{
network(df,specifygraph=FALSE,inspectprob=FALSE,
        doprob=TRUE,yr=c(0,350),xr=yr) 
\method{print}{network}(x,filename=NA,condposterior=FALSE,
                          condprior=FALSE,...) 
\method{plot}{network} (x,arrowlength=.25,
                        notext=FALSE,
                        sscale=7,showban=TRUE,yr=c(0,350),xr=yr,
                        unitscale=20,cexscale=8,...)

prob.network (x,df)

banlist(x)
"banlist<-"(x,value)

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
    for example in \code{\link{simulation}}. }
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
  \item{value}{a numeric matrix with two columns. Each row contains the
    indices \code{i -> j} of arrows that may not be allowed in the
    directed acyclic graph.}
}


\value{
  An object of class \code{network}, which is a list with the following
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
  \code{\link{simulation}},
  \code{\link{learn}},
  \code{\link{drawnetwork}},
  \code{\link{jointprior}},
  \code{\link{heuristic}},
  \code{\link{nwequal}}
  }

\references{
  Further information about \bold{deal} can be found at:\cr
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

set.seed(109)
sex     <- gl(2,4,label=c("male","female"))
age     <- gl(2,2,8)
yield   <- rnorm(length(sex))
weight  <- rnorm(length(sex))
mydata  <- data.frame(sex,age,yield,weight)
mynw    <- network(mydata)

# adjust prior probability distribution
mynw$nodes$sex$prob[1:2]   <- c(0.4,0.6)
mynw$nodes$age$prob[1:2]   <- c(0.6,0.4)
mynw$nodes$yield$prob[1:2] <- c(2,0)
mynw$nodes$weight$prob[1:2]<- c(1,0)

print(mynw)
plot(mynw)

prior <- jointprior(mynw)
mynw  <- learn(mynw,mydata,prior)$nw
thebest <- autosearch(mynw,mydata,prior)$nw

print(mynw,condposterior=TRUE)

\dontrun{savenet(mynw,"yield.net")}
}

\keyword{models}
