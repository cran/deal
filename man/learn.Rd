%                               -*- Mode: Rd -*- 
% learn.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:02:26 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon Feb 03 10:04:16 2003
% Update Count    : 52
% Status          : Unknown, Use with caution!
% 

\name{learn}
\alias{learn}
\alias{reuselearn}
\alias{learnnode}
\alias{udisclik}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Estimates the parameters in the local probability
distributions from data}

\description{Update the distributions of the parameters in the network,
  based on a prior network and data. Also, the network score is calculated.
%  Given a network, a data frame and a joint prior, the network
%  parameters are updated to their posteriors.
}

\usage{
learn (nw, df, prior=jointprior(nw),
                       nodelist=1:nw$n,trylist=
                       rep(list(NULL),nw$n),
                       timetrace=FALSE,
                       usetrylist = TRUE
                       ) 
learnnode(node,nw,df,prior=jointprior(nw),timetrace=FALSE)

udisclik(node,nw,df)

reuselearn(node,trylist) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{The network.}
  \item{df}{Data frame used for learning the network, see
    \code{\link{network}}.} 
  \item{prior}{A joint prior, see \code{\link{jointprior}}.}
  \item{nodelist}{A vector of indices of nodes to be learned.}
  \item{trylist}{Used internally for reusing learning of nodes, see
    \code{\link{maketrylist}}.} 
%  \item{smalldf}{Used for analysing networks with time varying nodes,
%    see \code{\link{timeslice}}.}
  \item{timetrace}{If \code{TRUE}, prints some timing information on the screen.}
  \item{node}{Node to be learned.}
  \item{usetrylist}{Use trylist or not. If the number of nodes is large,
    the look up in the trylist may be slower than re-learning the node.}
}

\details{
  The procedure \code{learn} determines the master prior, local parameter
  priors and local parameter posteriors. It may be called on all nodes
  or just a single node. 
  
  From the joint prior distribution, the marginal distribution of
  all parameters in the family consisting of the node and its parents
  can be determined. This is the master prior, see
  \code{\link{localmaster}}.

  The local parameter priors are now determined by conditioning in
  the master prior distribution, see
  \code{\link{conditional}}. The hyperparameters associated with the
  local parameter prior distribution is attached to each node in the
  attribute \code{condprior}.

  Finally, the local parameter posterior distributions are calculated (see
  \code{\link{post}}) and attached to each node in the attribute
  \code{condposterior}. 
  
  A so-called trylist is maintained. It consists of learned nodes with
  particular parent configurations. If a node with a certain parent
  configuration needs to be learned, it is checked, whether the node has
  already been learned. The previously learned nodes are given as input
  in the trylist parameter and is updated in the learning procedure. The
  learning procedure calls \code{reuselearn} which traverses through the
  trylist to see if it is possible to reuse previously learned nodes.

%  If it is not possible to reuse information, the posterior
%  distributions of the parameters are calculated. First, the master
%  prior procedure is used (\code{\link{conditional}}) to deduce the
%  prior parameters for the current node. Then the posteriors are
%  determined from the data using the algorithm described in Bottcher
%  (2002). 

  When one or more nodes in a network have been learned, the network
  score is updated and attached to the network in the attribute
  \code{score}.  
%  \code{udisclik} calculates the log-likelihood contribution of discrete
%  nodes. For continuous nodes, this is done while learning. The score is
%  used for strucural learning, see \code{\link{autosearch}}.

  The learning procedure is called from various functions using the
  principle, that networks should always be updated with their
  score. Thus, e.g.\ \code{\link{drawnetwork}} keeps the network updated
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
