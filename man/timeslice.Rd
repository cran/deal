%                               -*- Mode: Rd -*- 
% timeslice.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:04:36 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Mon Sep 16 16:00:50 2002
% Update Count    : 19
% Status          : Unknown, Use with caution!
% 

\name{timeslice}
\alias{timeslice}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{Prepare time series data for analysis with Deal}

\description{Prepare a dataframe with dynamic and non-dynamic variables
  for analysis as timeslice networks.
}

\usage{
timeslice(df, tvar, id = 0, time = 0, dropid = TRUE)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{df}{Dataframe}
  \item{tvar}{The indices (column numbers) of the timevarying variables
    in \code{df}.} 
  \item{id}{If on column in \code{df} defines individuals, this is the
    column index. Otherwise this should be 0.}
  \item{time}{The column in \code{df} that corresponds to time. If no
    such variable is present, this should be 0.}
  \item{dropid}{If \code{TRUE} the column number \code{id} is dropped
    in the returned dataframes.}
}

\details{
  Repeated measurements of a variable \eqn{x}{x} on the same individual
  is assumed to follow a first order Markov process. A new column
  \eqn{x_{t-1}}{x_{t-1}} in the dataframe is created where the
    time-lagged variable occurs. The original variable is renamed to
    \eqn{x_t}{x_t} and the two variables are then nodes in the network.

    In the function \code{\link{network}} the argument \code{tvar} is
    the column indices of both \eqn{x_t}{x_t} and \eqn{x_{t-1}}{x_{t-1}}
    (and similarly for other time-varying variables). This will tag the
    nodes in the network. The dataframe supplied to all functions used
    in the analysis should be \code{bigdf} output from
    \code{timeslice}. This is used as data for the time-varying
    nodes. The additional argument \code{smalldf} should be
    \code{smalldf} supplied also by \code{timeslice}. This is used as
    data for non-timevarying nodes.
  }

\value{
  A list with two components
  \item{bigdf}{A dataframe like \code{df} but with one extra column per
    timevarying variable.}
  \item{smalldf}{A dataframe like \code{df} but with only one replicate
    per \code{id} of the non-timevarying variables.}
}

\note{na.omit should be applied after this function -- not before.}

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


\seealso{\code{\link{network}}}


\keyword{models}
