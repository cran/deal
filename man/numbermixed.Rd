%                               -*- Mode: Rd -*- 
% numbermixed.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:03:38 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Sun May 26 11:04:17 2002
% Update Count    : 2
% Status          : Unknown, Use with caution!
% 

\name{numbermixed}
\alias{numbermixed}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{The number of distinct networks}

\description{Calculate the number of DAGS in a mixed network.
}

\usage{
numbermixed(nd,nc)
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nd}{The number of discrete nodes.}
  \item{nc}{The number of continuous nodes.}
}

\details{
  No arrows are allowed from continuous nodes to discrete nodes. Cycles
  are not allowed. The
  number of networks is given by Bøttcher (2002), using the result in
  Robinson (1977).

  When nd+nc>15, the procedure is quite slow.
}

\value{
  The number of networks with the given node configuration.
}

\references{
  Bøttcher, S.G. (2002). Learning Bayesian Networks with Mixed
  Variables.

  Robinson, R.W. (1977). Counting unlabeled acyclic digraphs, Lecture
  Notes in Mathematics, 622: Combinatorial Mathematics.

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
numbermixed(2,2)
\dontrun{numbermixed(5,10)}
}

\keyword{models}
