%                               -*- Mode: Rd -*- 
% localmaster.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:02:42 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon May 27 15:05:48 2002
% Update Count    : 33
% Status          : Unknown, Use with caution!
% 

\name{localmaster}
\alias{localmaster}
\alias{printmaster}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Local master}

\description{Calculates the joint distribution of a node and its parents
  from the joint prior.
}

\usage{
localmaster(family,nw,prior=jointprior(nw))
printmaster(nw,prior=jointprior(nw)) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{The network.}
   \item{family}{Indices of node and parents of the node.}
   \item{prior}{A joint prior, see \code{\link{jointprior}}.}
}

\details{Called by \code{\link{cond.node}}.

  For the discrete part of the
  network, the master is the marginal distribution of the discrete nodes
  in the family.

  For the mixed part of the network, for each configuration \eqn{i} of the
  discrete variables in \code{family}, the joint parameter priors are given
  by \code{\link{jointprior}} as
  \deqn{p(m_i|\Sigma_i) = N(\mu_i,\Sigma_i/\nu_i)}{p(m_i|Sigma_i) = N(mu_i,Sigma_i/nu_i)}
  \deqn{p(\Sigma_i) = IW(\rho_i,\Phi_i)}{p(Sigma_i) = IW(rho_i,Phi_i)}
  where \code{IW} denotes the inverse Wishart distribution.

  Then, the local master for configuration \eqn{i} is deduced for the
  family \eqn{A} as
  \deqn{\Sigma_{A\cap\Gamma|i_{A\cap\Delta}} \sim%
    IW(\rho_{i_{A\cap\Delta}},\tilde\Phi_{A\cap\Gamma|i_{A\cap\Delta}})}{%
    Sigma_{A intersect \Gamma|i_{A intersect Delta}} ~%
    IW(rho_{i_{A intersect Delta}},tildePhi_{A intersect Gamma|i_{A
	intersect Delta}})
  }
  \deqn{m_{A\cap\Gamma|i_{A\cap\Delta}}|\Sigma_{A\cap\Gamma|i_{A\cap\Delta}}%
    \sim%
    N(\bar\mu_{A\cap\Gamma|i_{A\cap\Delta}},
    \Sigma_{A\cap\Gamma|i_{A\cap\Delta}}/\nu_{A\cap\Delta})}{%
      m_{A intersect Gamma|i_{A intersect Delta}}|Sigma_{A intersect Gamma|i_{A intersect Delta}}%
    \sim%
    N(bar.mu_{A intersect Gamma|i_{A intersect Delta}},
    Sigma_{A intersect Gamma|i_{A intersect Delta}}/nu_{A intersect Delta})
    }
where \eqn{\Gamma}{Gamma} is the set of continuous nodes and
\eqn{\Delta}{Delta} is the set of discrete nodes. Furthermore,
\deqn{\rho_{i_{A\cap\Delta}}%
  = \sum_{j:j_{A\cap\Delta}=i_{A\cap\Delta}} \rho_j}{%
rho_{i_{A intersect Delta}}%
  = sum_{j:j_{A intersect Delta}=i_{A intersect Delta}} rho_j}  
and likewise for \eqn{\nu_{i_{A\cap\Delta}}}{nu_{i_{A intersect Delta}}}
and \eqn{\Phi_{i_{A\cap\Delta}}}{Phi_{i_{A intersect Delta}}}. Finally,
\deqn{\bar\mu_{A\cap\Delta=%
    (
    \sum_{j:j_{A\cap\Delta}=i_{A\cap\Delta}}\mu_j\nu_j
    )/\nu_{i_{A\cap\Delta}}
}}{barmu_{A intersect Delta%
    (
    sum_{j:j_{A intersect Delta}=i_{A intersect Delta}} mu_j nu_j
    )/nu_{i_{A intersect Delta}}
}}
\deqn{\tilde\Phi_{A\cap\Gamma|i_{A\cap\Delta}}=%
  \Phi_{i_{A\cap\Delta}} +
  \sum_{j:j_{A\cap\Delta}=i_{A\cap\Delta}}
  \nu_j(\mu_j - \bar\mu_{i_{A\cap\Delta}})
  (\mu_j - \bar\mu_{i_{A\cap\Delta}})^\top}{%
tildePhi_{A intersect Gamma|i_{A intersect Delta}}=%
  Phi_{i_{A intersect Delta}} +
  sum_{j:j_{A intersect Delta}=i_{A intersect Delta}}
  nu_j(mu_j - barmu_{i_{A intersect Delta}})
  t(mu_j - barmu_{i_{A intersect Delta}})
}

}
\seealso{\code{\link{cond.node}}}

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~
\references{
  Further information about \bold{Deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}


\keyword{iplot}
