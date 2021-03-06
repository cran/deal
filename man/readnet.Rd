\encoding{latin1}
\name{readnet}
\alias{readnet}
\alias{savenet}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Reads/saves .net file}
\description{
Reads/saves a Bayesian network specification in the \code{.net} language used by Hugin.
}
\usage{
readnet(con=file("default.net"))
savenet(nw, con=file("default.net"))
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{con}{a connection.}
  \item{nw}{an object of class \code{\link{network}}.}
}
\details{
  \code{readnet} reads only the structure of a network, i.e.\ the
  directed acyclic graph.

  \code{savenet} exports the \code{prob} property for each node in the
  network object along with the network structure defined by the parents
  of each node. 
} 
\value{
  \code{readnet} creates an object of class \code{\link{network}} with
  the nodes specified as 
  in the \code{.net} connection. The network 
  has not been learned and the nodes do not have \code{prob} properties
  (see \code{\link{prob.network}}).

  \code{savenet} writes the object to the connection.
}
% \references{
%   Further information about \bold{deal} can be found at:\cr
%   \url{http://www.math.aau.dk/~dethlef/novo/deal}.
% }

\author{
  Susanne Gammelgaard Bottcher, \cr
  Claus Dethlefsen \email{rpackage.deal@gmail.com}.
}

\note{The call to \code{readnet(savenet(network))} is \emph{not} the identity
  function as information is thrown away in both \code{savenet} and
  \code{readnet}.} 

\seealso{\code{\link{network}}}

\examples{
data(rats)
nw <- network(rats)
\dontrun{savenet(nw,file("default.net"))}
\dontrun{nw2 <- readnet(file("default.net"))}
\dontrun{nw2 <- prob(nw2,rats)}

}
\keyword{ iplot }% at least one, from doc/KEYWORDS
