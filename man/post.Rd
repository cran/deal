%                               -*- Mode: Rd -*- 
% post.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Mon May 06 10:35:00 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Thu Jul 24 15:15:09 2003
% Update Count    : 42
% Status          : Unknown, Use with caution!
% 

\name{post}
\alias{post}
\alias{post0}
\alias{postc}
\alias{postc0c}
\alias{postcc}
\alias{postM}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Calculation of parameter posteriors for continuous node}

\description{Learns the parameters and calculates the network score
contribution for continuous nodes.  }

\usage{
post   (mu,tau,rho,phi,y,z,timetrace=FALSE) 
postM  (mu,tau,rho,phi,y,z,timetrace=FALSE) 
postc  (mu,tau,rho,phi,y,z,timetrace=FALSE) 
postcc (mu,tau,rho,phi,y,z,timetrace=FALSE) 
post0  (mu,tau,rho,phi,y,timetrace=FALSE) 
postc0c(mu,tau,rho,phi,y,timetrace=FALSE) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{mu}{a numeric vector of dimension 1 + the number of continuous
    parents. \code{mu} is a parameter in the local master, see
    \code{\link{conditional}}.} 
  \item{tau}{a numeric matrix, which gives the unscaled precision matrix of
    regression parameters. Symmetric matrix with number of
    columns and rows equal to 1 plus number of continous parents.}
  \item{rho}{a numeric, which gives a parameter in the distribution of the
    scale parameter.}
  \item{phi}{a numeric, which gives a parameter in the distribution of the
    scale parameter.}
  \item{y}{a numeric vector of observations of the current node.}
  \item{z}{a numeric matrix with a column of ones and columns with the
    observations of the continuous parents.}
  \item{timetrace}{a logical. If \code{TRUE}, prints some timing
    information on the screen.} 
}

\details{These functions are called by the learning routines
  (see \code{\link{learn}}) and is only intended for internal use. In
  fact, only postc0c and postcc are used for speed reasons. The
  remaining functions are included for experimental purposes.

  post0: posterior for continuous node with 0 parents as batch learning.

  postc0c: as post0, but using sequential learning in C.

 postc: posterior for continuous node with continuous
 parents. Sequential learning.

 post: as postc, but as batch learning.

 postM: as post, but using the Matrix library.

 postcc: as postc, but using C.

}

\value{A list with the following components,
  \item{mu}{numeric vector, giving the posterior mean of the regression parameters.}
  \item{tau}{a numeric matrix, which gives the posterior unscaled precision matrix of
    regression parameters.}
  \item{rho}{a numeric, which gives the posterior of a parameter in the
    distribution of the scale parameter.}
  \item{phi}{a numeric, which gives the posterior of a parameter in the
    distribution of the scale parameter.}
  \item{loglik}{a numeric, which gives the log-likelihood contribution to the
    network score for this node.}
}

\seealso{\code{\link{learnnode}}, \code{\link{conditional}}}

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
  data(rats)
  fit       <- network(rats)
  fit.prior <- jointprior(fit,12)
  W1        <- fit$nodes$W1
  W1        <- cond.node(W1,fit,fit.prior)
  W1.post   <-  postc0c(W1$condprior[[1]]$mu,
                       W1$condprior[[1]]$tau,
                       W1$condprior[[1]]$rho,
                       W1$condprior[[1]]$phi,
                       rats[,W1$idx])
}
\keyword{iplot}
