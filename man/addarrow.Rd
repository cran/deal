%                               -*- Mode: Rd -*- 
% addarrow.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:00:53 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Thu Jan 16 13:52:58 2003
% Update Count    : 9
% Status          : Unknown, Use with caution!
% 

\name{addarrow}
\alias{addarrow}
%- Also NEED an `\alias' for EACH other topic documented here.
\alias{turnarrow}
\alias{removearrow}

\title{Adding/Turning/Removing arrows}

\description{Create all networks with exactly one arrow added/turned/removed.
}

\usage{
addarrow   (nw,df,prior,trylist=rep(list(NULL),nw$n)) 
turnarrow  (nw,df,prior,trylist=rep(list(NULL),nw$n)) 
removearrow(nw,df,prior,trylist=rep(list(NULL),nw$n)) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{The network from which arrows are added/removed/turned.}
  \item{df}{Data frame used for learning the network, see \code{\link{network}}.}
  \item{prior}{A joint prior, see \code{\link{jointprior}}.}
  \item{trylist}{Used internally for reusing learning of nodes, see \code{\link{maketrylist}}.}
%  \item{smalldf}{Used for analysing networks with time varying nodes,
%    see \code{\link{timeslice}}.}
}

\details{
  Given the starting network, a list of networks is created with exactly
  one arrow added/turned/removed. All possible networks (with the
  restrictions in \code{\link{insert}}) is returned in
  the list of networks. All networks are learned with the data in
  \code{df}. Used by \code{\link{autosearch}}. 
}

\seealso{\code{\link{heuristic}},
  \code{\link{autosearch}},
  \code{\link{insert}},
  \code{\link{addarrows}},
  \code{\link{network}},
  \code{\link{networkfamily}}
  }

\value{A list with two elements
  \item{nw}{A network family with all created networks. All networks have
  been learned.}
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
fit        <- network(rats,specifygraph=FALSE,inspect=FALSE)
fit.prior  <- jointprior(fit,12)
fit        <- learn(fit,rats,fit.prior)$nw
rats.extra <- addarrow(fit,rats,fit.prior)$nw
\dontrun{plot(rats.extra)}
rats.extra2<- addarrow(rats.extra[[2]],rats,fit.prior)$nw
\dontrun{plot(rats.extra2)}
}

\keyword{iplot}
