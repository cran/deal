\name{readnet}
\alias{readnet}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Read .net file}
\description{
Read a Bayesian network specification in the .net language (see eg. Hugin)
}
\usage{
readnet(fn)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{fn}{Filename to be read}
}
\details{
  Reads only the 'node' and 'potential' specifycation. Does not store
  the 'data' entries as well as other information.
}
\value{
  A network with the nodes specified as in the .net file. The network
  has not been learned and does not contain 'prob' attributes.
}
\references{
  Further information about \bold{Deal} can be found at:\cr
  \url{http://www.math.auc.dk/novo/deal}.
}
\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

\note{The call to readnet(savenet(network)) is *not* the identity
  function as information is thrown away in both savenet and readnet.}

\seealso{\code{\link{network}}, \code{\link{savenet}} }

\examples{
data(rats)
nw <- network(rats)
savenet(nw)
nw2 <- readnet("default.net")
}
\keyword{ iplot }% at least one, from doc/KEYWORDS
