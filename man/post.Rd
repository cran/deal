%                               -*- Mode: Rd -*- 
% post.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Mon May 06 10:35:00 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon Sep 16 18:48:38 2002
% Update Count    : 27
% Status          : Unknown, Use with caution!
% 

\name{post}
\alias{post}
\alias{post0}
\alias{postc}
\alias{postc0c}
\alias{postcc}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Calculation of parameter posteriors for continuous node}

\description{Learning parameters and calculating network score contribution
}

\usage{
post   (mu,tau,rho,phi,y,z,timetrace=FALSE) 
postc  (mu,tau,rho,phi,y,z,timetrace=FALSE) 
postcc (mu,tau,rho,phi,y,z,timetrace=FALSE) 
post0  (mu,tau,rho,phi,y,timetrace=FALSE) 
postc0c(mu,tau,rho,phi,y,timetrace=FALSE) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{mu}{The prior mean of the regression parameters. Dimension 1 +
    number of continuous parents.}
  \item{tau}{The prior parameter tau. Symmetric matrix with number of
    columns and rows equal to 1 plus number of continous parents.}
  \item{rho}{The prior parameter rho. An integer.}
  \item{phi}{The prior parameter phi. An integer.}
  \item{y}{Data vector of observations of the current node.}
  \item{z}{Matrix with a column of ones and columns with the
    observations of the continuous parents.}
  \item{timetrace}{For timing purposes.}
}

\details{These functions are called by the learning routines
  (learn.network, learn.node).

  post0: Posterior for continuous node with 0 parents as batch learning.

  postc0c: as post0, but using sequential learning in C.

 postc: Posterior for continuous node with continuous
 parents. Sequential learning.

 post: as postc, but as batch learning.

 postcc: as postc, but using C.

}

\value{Updated parameters and log-network score contribution of current node.
}

\seealso{\code{\link{learn.node}}}

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
  data(rats)
  fit       <- network(rats,specifygraph=FALSE,inspect=FALSE)
  fit.prior <- jointprior(fit,12) 
  W1        <- fit$nodes$W1
  W1        <- cond(W1,fit,fit.prior)
  W1.post   <-  postc0c(W1$condprior[[1]]$mu,
                       W1$condprior[[1]]$tau,
                       W1$condprior[[1]]$rho,
                       W1$condprior[[1]]$phi,
                       rats[,W1$idx])
}
\keyword{iplot}
