%                               -*- Mode: Rd -*- 
% jointprior.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:02:21 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Thu Jul 24 16:35:22 2003
% Update Count    : 53
% Status          : Unknown, Use with caution!
% 

\name{jointprior}
\alias{jointprior}
\alias{jointdisc}
\alias{jointcont}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Calculates the joint prior distribution}

\description{Given a network with a \code{prob} property for each node, derives the
  joint probability distribution. Then the quantities needed in
the local master procedure for finding the local parameter priors are
deduced.}

\usage{
jointprior(nw,N=NA,phiprior="bottcher",timetrace=FALSE) 
jointdisc(nw,timetrace=FALSE)
jointcont(nw,timetrace=FALSE)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{an object of class \code{\link{network}}. Each node must
    have a \code{prob} property to describe the local probability
    distribution. The \code{prob} property
    is created using \code{\link{prob.network}}, which is called by the
    \code{\link{network}} function.}
  \item{N}{an integer, which gives the size of the imaginary data base. If
    this is too small, 
    \code{NA}'s may be created in the output, resulting in errors in
    \code{\link{learn}}. If no \code{N} is given, the procedure tries to 
    set a value as low as possible.} 
  \item{phiprior}{a string, which specifies how the prior for phi is
    calculated. Either \code{phiprior="bottcher"} or
    \code{phiprior="heckerman"} can be used.} 
  \item{timetrace}{a logical. If \code{TRUE}, prints some timing
    information on the screen.} 
}

\details{For the discrete part of the network, the joint probability
  distribution is 
  calculated by multiplying together the local probability
  distributions. Then, \code{jointalpha} is determined by multiplying
  each entry in the joint probability distribution by the size of the
  imaginary data base \code{N}. 

  For the mixed part of the network, for each configuration of the discrete
  variables, the joint Gaussian distribution of the continuous
  variables is constructed and represented by \code{jointmu} (one
  row for each configuration of the discrete parents) and
  \code{jointsigma} (a list of matrices -- one for each configuration of
  the discrete parents). The configurations of the discrete parents are
  ordered according to \code{\link{findex}}. The algorithm for
  constructing the joint distribution of the continuous variables is
  described in Shachter and Kenley (1989). 

  Then, \code{jointalpha}, \code{jointnu}, \code{jointrho}, \code{mu} and
  \code{jointphi} are deduced. These quantities are later used for
deriving local parameter priors.

  For each configuration \code{i} of the discrete variables,
  \deqn{\nu_i=\rho_i=\alpha_i}{nu[i]=rho[i]=jointalpha[i]} and
  \deqn{\phi_i = (\nu_i -1)\Sigma_i}{phi[i] = (nu[i] -1)Sigma[i]}
  if \code{phiprior="bottcher"}, see Bøttcher(2001) and
  \deqn{\phi_i = \nu_i(\rho_i -2)\Sigma_i/(\nu_i+1)}{phi[i] = nu[i](rho[i] -2)Sigma[i]/(nu[i]+1) }
  if \code{phiprior="heckerman"}, see Heckerman, Geiger and Chickering (1995).

  The procedures \code{jointcont} and \code{jointdisc} are intended for
  internal use only.
}

\value{
  A list with the following elements,
  \item{jointalpha}{a table used in the local master procedure for discrete variables.}
  \item{jointnu}{a table used in the local master procedure for continuous variables.}
  \item{jointrho}{a table used in the local master procedure for continuous variables.}
  \item{jointmu}{a numeric matrix used in the local master procedure for continuous variables.}
  \item{jointsigma}{a list of numeric matrices (not used in further calculations).}
  \item{jointphi}{a list of numeric matrices used in the local master procedure for continuous variables.}
}

\seealso{\code{\link{network}}, \code{\link{prob.network}}
  }

  \references{
    Bøttcher, S.G. (2001). Learning Bayesian Networks with Mixed Variables, {\em Artificial Intelligence and Statistics 2001}, Morgan Kaufmann, San Francisco, CA, USA, 149-156.

    Heckerman, D., Geiger, D. and Chickering, D. (1995). Learning Bayesian
    networks: The combination of knowledge and statistical data. Machine
    Learning {\bf 20}: 197-243.  

Shachter, R.D. and Kenley, C.R. (1989), Gaussian influence diagrams. Management
    Science {\bf 35}:527-550.
    
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
rats.nw    <- network(rats)
rats.prior <- jointprior(rats.nw,12)

\dontrun{savenet(rats.nw,"rats.net")}
\dontrun{rats.nw <- readnet("rats.net")}
\dontrun{rats.nw <- prob.network(rats.nw,rats)}
\dontrun{rats.prior <- jointprior(rats.nw,12)}

}

\keyword{models}
