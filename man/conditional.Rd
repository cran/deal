%                               -*- Mode: Rd -*- 
% conditional.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:01:34 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon May 27 14:55:16 2002
% Update Count    : 6
% Status          : Unknown, Use with caution!
% 

\name{conditional}
\alias{conditional.cont}
\alias{conditional.disc}
\alias{conditional}
\alias{cond.node}
\alias{cond}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Calculate conditional distribution}

\description{From master distribution the conditional distribution of a
  node given the parents is calculated.
}

\usage{
conditional.cont(A,mu,nu,rho,phi) 
conditional(A,master,nw) 
conditional.disc(A,master) 
cond.node(node,nw,nw.prior=jointprior(nw)) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{The network.}
  \item{node}{The current node.}
  \item{nw.prior}{A joint prior, see \code{\link{jointprior}}.}
  \item{A}{The index of the current node.}
  \item{master}{Local master.}
  \item{mu}{Mu parameter in local master.}
  \item{nu}{Nu parameter in local master.}
  \item{rho}{Rho parameter in local master.}
  \item{phi}{Phi parameter in local master.}
  
}

\seealso{\code{\link{learn}},
  \code{\link{localmaster}}
  }

\details{Used in \code{\link{learn}}, which calls \code{cond.node}. The
  local master is calculated and sent to \code{conditional} which calls either
  \code{conditional.cont} or \code{conditional.disc} depending on the
  type of the node. In each case the \code{\link{localmaster}} is used
  for calculating the conditional distribution of the current node given
  the remaining nodes in the local master.
}

\value{The node is updated with condprior attribute.
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


\keyword{iplot}
