%                               -*- Mode: Rd -*- 
% learn.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:02:26 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Sun Nov 03 20:38:19 2002
% Update Count    : 20
% Status          : Unknown, Use with caution!
% 

\name{learn}
\alias{learn}
\alias{reuselearn}
\alias{learnnode}
\alias{udisclik}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Learning networks and nodes}

\description{Given a network and a dataframe and jointprior, the network
  parameters are updated to their posteriors.
}

\usage{
learn (nw, df, prior=jointprior(nw),
                       nodelist=1:nw$n,trylist=
                       rep(list(NULL),nw$n),
                       timetrace=FALSE,smalldf=NA,
                       usetrylist = nw$n<=6
                       ) 
learnnode(node,nw,df,prior=jointprior(nw),timetrace=FALSE)

udisclik(node,nw,df)

reuselearn(node,trylist) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{The network.}
  \item{df}{Dataframe used for learning the network, see \code{\link{network}}.}
  \item{prior}{A joint prior, see \code{\link{jointprior}}.}
  \item{nodelist}{A vector of indices of nodes to be learned.}
  \item{trylist}{Used internally for reusing learning of nodes, see \code{\link{maketrylist}}.}
  \item{smalldf}{Used for analysing networks with time-varying nodes,
    see \code{\link{timeslice}}.}
  \item{timetrace}{If TRUE, prints some timing information on the screen.}
  \item{node}{Node to be learned.}
  \item{usetrylist}{Use trylist or not. If the number of nodes is large,
    the lookup in the trylist is slower than re-learning the node.}
}

\details{
  Given data and a network, the parameters are updated. The joint prior
  defines the prior parameters by conditioning (see
  \code{\link{conditional}}) in the local master (see
  \code{\link{localmaster}}). The data are then learned by calling the 
  approriate \code{\link{post}} function.
  
  A socalled trylist is maintained. This consists of learned nodes with
  particular parent configurations. If a node with a certain parent
  configuration needs to be learned, it is checked, whether the node has
  already been learned. The previously learned nodes are given as input
  in the trylist parameter and is updated in the learning procedure. The
  learning procedure calls \code{reuselearn} which traverses through the
  trylist to see if it is possible to reuse previously learned nodes.

  If it is not possible to reuse information, the posterior
  distributions of the parameters are calculated. First, the master
  prior procedure is used (\code{\link{conditional}}) to deduce the
  prior parameters for the current node. Then the posteriors are
  determined from the data using the algorithm described in Bottcher
  (2002). 
  
  \code{udisclik} calculates the log-likelihood contribution of discrete
  nodes. For continuous nodes, this is done while learning.

  The learning procedure is called from various functions using the
  principle, that networks should always be updated with their
  score. Thus, eg. \code{\link{drawnetwork}} keeps the network updated
  when the graph is altered.
  
}

\seealso{\code{\link{networkfamily}},
\code{\link{jointprior}},
\code{\link{maketrylist}},
\code{\link{network}},
\code{\link{post}}
}

\value{A list with two elements
  \item{nw}{The network with the \code{condposterior} attributes updated for
    the nodes. Also, the attribute \code{score} is updated and contains
    the network score. The contribution to the network score for each
    node is contained in the attribute \code{loglik} for each node.}
  \item{trylist}{Updated.}
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
fit       <- network(rats)
fit.prior <- jointprior(fit,12)
fit       <- learn(fit,rats,fit.prior)$nw
}

\keyword{iplot}
