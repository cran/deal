%                               -*- Mode: Rd -*- 
% addrandomarrow.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:01:24 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Thu Jan 16 13:53:29 2003
% Update Count    : 10
% Status          : Unknown, Use with caution!
% 

\name{addrandomarrow}
\alias{addrandomarrow}
\alias{turnrandomarrow}
\alias{deleterandomarrow}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Adding/Turning/Removing random arrows}

\description{Create network with exactly one arrow added/turned/removed
  randomly chosen.
}

\usage{
addrandomarrow(nw,data,prior,trylist=rep(list(NULL),nw$n),nocalc=FALSE,
               timetrace=FALSE)
turnrandomarrow(nw,data,prior,trylist=rep(list(NULL),nw$n),nocalc=FALSE,
               timetrace=FALSE)
deleterandomarrow(nw,data,prior,trylist=rep(list(NULL),nw$n),nocalc=FALSE,
               timetrace=timetrace)  }

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{The network from which arrows are added/removed/turned.}
  \item{data}{Data frame used for learning the network, see \code{\link{network}}.}
  \item{prior}{A joint prior, see \code{\link{jointprior}}.}
  \item{trylist}{Used internally for reusing learning of nodes, see \code{\link{maketrylist}}.}
  \item{nocalc}{Do not call the learning procedure if \code{TRUE}. Used if only a
    random graph is wanted.}
  \item{timetrace}{If \code{TRUE}, prints some timing information on the screen.}
%  \item{smalldf}{Used for analysing networks with time varying nodes,
%    see \code{\link{timeslice}}.}
}

\details{
  Given the starting network, a network is created with exactly
  one arrow added/turned/removed. The arrow is chosen randomly amongst all
  possibilities. If there are no possibilities, the original network is
  returned. Used in \code{\link{perturb}}.
}

\seealso{\code{\link{perturb}}
  }

\value{A list with two elements
  \item{nw}{A network family with all created networks.}
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

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\examples{
data(rats)
fit       <- network(rats,specifygraph=FALSE,inspect=FALSE)
fit.prior <- jointprior(fit,12)
fit       <- learn(fit,rats,fit.prior)$nw
rats.extra <- addrandomarrow(fit,rats,fit.prior)$nw
\dontrun{plot(rats.extra)}
}

\keyword{iplot}
