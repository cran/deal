%                               -*- Mode: Rd -*- 
% drawnetwork.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:01:44 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Sun Nov 03 16:57:52 2002
% Update Count    : 14
% Status          : Unknown, Use with caution!
% 

\name{drawnetwork}
\alias{drawnetwork}
\alias{inspectprob}


%- Also NEED an `\alias' for EACH other topic documented here.

\title{Graphical interface for manipulation of networks}

\description{
}

\usage{
drawnetwork(nw,df,prior,trylist=rep(list(NULL),nw$n),
            scale=10,unitscale=20,cexscale=8,
            arrowlength=.25,nocalc=FALSE,smalldf=NA,
            yr=c(0,350),xr=yr,...)

inspectprob(nw,scale=10,unitscale=20,cexscale=8,
            arrowlength=.25,xr=c(0,350),yr=xr,...) 

}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{The initial network, see \code{\link{network}}.}
  \item{df}{Dataframe used for learning the network, see \code{\link{network}}.}
  \item{prior}{A joint prior, see \code{\link{jointprior}}.}
  \item{trylist}{Used internally for reusing learning of nodes, see \code{\link{maketrylist}}.}
  \item{cexscale}{Passed to \code{\link{plot.network}}.}
  \item{arrowlength}{Passed to \code{\link{plot.network}}.}
  \item{nocalc}{Used when no learning is wanted, see eg. \code{\link{simulation}}.}
  \item{smalldf}{Used for analysing networks with time-varying nodes,
    see \code{\link{timeslice}}.}
  \item{unitscale}{Passed to \code{\link{plot.network}}.}
  \item{scale}{Passed to \code{\link{plot.network}}.}
  \item{xr}{Range on x-axis.}
  \item{yr}{Range on y-axis.}
  \item{...}{Passed to \code{\link{plot.network}}.}
}

\details{
To insert an arrow from node 'A' to node 'B' first click node 'A' and
then click node 'B'. When the graph is finished, click 'stop'.

To specify, that an arrow must not be present, press 'ban' (a toggle)
and draw the arrow. This is shown as a red dashed arrow. It is possible
to ban both directions between nodes. The banlist is stored with the
network in the attribute \code{banlist}. It is a matrix with two
columns. Each row is the 'from' node index and the 'to' node index,
where the indices are the column number in the dataframe.

Note that the network score changes as the network is re-learned
whenever a change is made. (Unless nocalc is TRUE).

\code{inspectprob} draws the network and makes it possible to inspect
the \code{prob} attributes of the nodes by clicking on them. The result
is shown in the output window.
}

\value{A list with two elements
  \item{nw}{A networkfamily with all created networks.}
  \item{trylist}{Updated.}
}

\seealso{\code{\link{network}}
  }

\references{
  Further information about \bold{Deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}


\examples{
data(rats)
rats.nw    <- network(rats)
rats.prior <- jointprior(rats.nw,12)
rats.nw    <- learn(rats.nw,rats,rats.prior)$nw

\dontrun{newrat  <- drawnetwork(rats.nw,rats,rats.prior)$nw}
}

\keyword{models}
