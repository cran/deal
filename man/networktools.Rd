%                               -*- Mode: Rd -*- 
% networktools.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Wed Jan 07 12:35:45 2004
% Last Modified By: Claus Dethlefsen
% Last Modified On: Fri Jan 09 12:36:08 2004
% Update Count    : 13
% Status          : Unknown, Use with caution!
% 

\name{Network tools}
\alias{modelstring}
\alias{makenw}
\alias{size}
\alias{as.network}
\alias{banlist}
\alias{getnetwork}
\alias{gettrylist}
\alias{banlist<-}

\title{Tools for manipulating networks}

\description{Various extraction/replacement functions for networks}

\usage{
modelstring(x)
makenw(tb,template)
as.network(nwstring,template)
size(x)
banlist(x)
banlist(x) <- value
getnetwork(x)
gettrylist(x)
}

\arguments{
    \item{x}{an object of class \code{\link{network}}.} 
  \item{tb}{a table output from \code{\link{autosearch}} or
    \code{\link{heuristic}} in the list property \code{table}. Can be
    translated into a \code{\link{networkfamily}}.}
  \item{template}{an object of class \code{\link{network}} with the same
    nodes as the networks described in the table \code{tb}.}
  \item{nwstring}{a string representing the network.}
  \item{value}{a numeric matrix with two columns. Each row contains the
    indices \code{i -> j} of arrows that may not be allowed in the
    directed acyclic graph.}
}

\details{
    The string representation of a network is a minimal size
  representation to speed up calculations. The functions
  \code{modelstring}, \code{as.network} and \code{makenw} converts
  between the string represention and network objects.

  \code{size} extracts the number of nodes in a network object.

  \code{banlist} extracts the banlist from a network object.

  \code{getnetwork} and \code{gettrylist} are accessor function that
  extracts a network object or trylist from the result from
  \code{\link{autosearch}}, \code{\link{heuristic}},
  \code{\link{learn}}, \code{\link{perturb}}, 
  \code{\link{networkfamily}}, \code{\link{drawnetwork}}.
}

\keyword{models}
