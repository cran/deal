%                               -*- Mode: Rd -*- 
% jointprior.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:02:21 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Wed Sep 25 17:39:49 2002
% Update Count    : 26
% Status          : Unknown, Use with caution!
% 

\name{jointprior}
\alias{jointprior}
\alias{jointdisc}
\alias{jointcont}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Setup a joint prior distribution for the parameters}

\description{Given a network with a \code{prob} attribute for each node, the
  joint prior distribution of the parameters is derived.
}

\usage{
jointprior(nw,N=NA,phiprior="bottcher",timetrace=FALSE,smalldf=NA) 
jointdisc(nw,timetrace=FALSE)
jointcont(nw,timetrace=FALSE)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{A network. Each node must have a \code{prob}
    attribute to describe the local probability distribution, see
    \code{\link{network}}.}
  \item{N}{The size of the imaginary database. If this is too small,
    errors might occur. If no \code{N} is given, the procedure tries to
    set a value as low as possible.}
  \item{phiprior}{The way of specifying the prior for phi. Either
    \code{phiprior="bottcher"} or \code{phiprior="heckerman"} can be used.}
  \item{timetrace}{If \code{TRUE}, prints some timing information on the
    screen.} 
  \item{smalldf}{Used for analysing networks with time-varying nodes,
    see \code{\link{timeslice}}.}
}

\details{For the discrete part of the network, the joint distribution is
  calculated by multiplying together the local probability
  distributions. Then, \code{jointalpha} is determined by multiplying
  each entry in the joint probability distribution by the size of the
  imaginary database \code{N}. 

  For the mixed part of the network, for each configuration of the discrete
  variables, the joint (Gaussian) distribution of the continuous
  variables are constructed and is represented by \code{jointmu} (one
  row for each configuration of the discrete parents) and
  \code{jointsigma} (a list of matrices -- one for each configuration of
  the discrete parentes). The configurations of the discrete parents are
  ordered according to \code{\link{findex}}. The algorithm for
  constructing the joint distribution of the continuous variables is
  described in eg. Shachter and Kenley (1989). 

  Then, the joint distribution of the
  parameters are deduced and expressed by \code{jointalpha} for the
  Dirichlet distribution; \code{jointnu}, \code{jointrho}, \code{mu} and
  \code{jointphi} for the Gaussian-inverse Wishart distribution.

  For the configuration \code{i} of the discrete variables,
  \deqn{\nu_i=\rho_i=\alpha_i}{nu_i=rho_i=jointalpha_i} and
  \deqn{\phi_i = (\nu_i -1)\Sigma_i}{phi_i = (nu_i -1)Sigma_i}
  if \code{phiprior="bottcher"} and
  \deqn{\phi_i = \nu_i(\rho_i -2)\Sigma_i/(\nu_i+1)}{phi_i = nu_i(rho_i -2)Sigma_i/(nu_i+1) }
  if \code{phiprior="heckerman"}.
}

\value{
  A list with the following elements
  \item{jointalpha}{The joint prior for the discrete variables.}
  \item{jointnu}{Joint prior for nu.}
  \item{jointrho}{Joint prior for rho.}
  \item{jointmu}{Joint prior for mu.}
  \item{jointsigma}{Sigma matrices (not used in further calculations).}
  \item{jointphi}{Joint prior for phi.}
}

\seealso{\code{\link{network}}
  }

  \references{
    Shachter and Kenley (1989), Gaussian influence diagrams. Management
    Science 35:527--550.
    
    
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
rats.nw    <- network(rats)
rats.prior <- jointprior(rats.nw,12)
}

\keyword{models}
