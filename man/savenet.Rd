\name{savenet}
\alias{savenet}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Saves Bayesian network as .net file}
\description{
Exports a network object in the standard format .net (used by eg. Hugin).
}
\usage{
savenet(nw, filename = "default.net")
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nw}{Network object, see \code{\link{network}}.}
  \item{filename}{Filename (should end with .net).}
}
\details{
  The \code{prob} attribute for each node in the network object is
  exported along with the network structure defined by the parents of
  each node. 
}
\value{A file is created.}
\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

\references{
  Further information about \bold{Deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}

\seealso{\code{\link{network}}}

\examples{
data(rats)
nw <- network(rats) # the empty network
savenet(nw)
}
\keyword{file}% at least one, from doc/KEYWORDS

