%                               -*- Mode: Rd -*- 
% genlatex.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:02:07 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Sun Nov 14 18:01:13 2004
% Update Count    : 13
% Status          : Unknown, Use with caution!
% 

\name{genlatex}
\alias{genlatex}
\alias{genpicfile}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{From a network family, generate LaTeX output}

\description{The networks in a network family is arranged as
  pictex-graphs in a LaTeX-table. 
}

\usage{
genlatex(nwl,outdir="pic/",prefix="scoretable",picdir="",picpre="pic",
         ncol=5,nrow=7,width=12/ncol,vadjust=-1.8)
genpicfile (nwl,outdir="pic/",prefix="pic",w=1.6,h=1.6,bigscale=3) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nwl}{object of class \code{networkfamily} containing a list of
    objects of class \code{network}.}
  \item{outdir}{character string, the directory for storing output.}
  \item{prefix}{character string, the filename (without extension) of the LaTeX file. The
    filenames of the picfiles begin with the given prefix.} 
  \item{picdir}{character string, the directory where pic-files are stored.}
  \item{picpre}{character string, prefix for pic-files.}
  \item{ncol}{integer, the number of columns in LaTeX table.}
  \item{nrow}{integer, the number of rows in LaTeX table.}
  \item{width}{numeric, the width of each cell in the LaTeX table.}
  \item{vadjust}{numeric, the vertical adjustment in LaTeX table.}
  \item{w}{numeric, the width of pictex objects}
  \item{h}{numeric, the height of pictex objects}
  \item{bigscale}{numeric, the scaling of the best network, which is output in 'nice.tex'}
  
}

\value{Files:

  \item{\{outdir\}\{picpre\}xx.tex}{one pictex file for each network in the
  network family, indexed by xx.}

  \item{\{outdir\}\{prefix\}.tex}{LaTeX file with table including all pictex files.}

  \item{\{outdir\}\{picpre\}nice.tex}{pictex file with the best network.}
}

\seealso{\code{\link{networkfamily}}}

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.aau.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.aau.dk}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\examples{
data(rats)
allrats <- getnetwork(networkfamily(rats,network(rats)))
allrats <- nwfsort(allrats)

\dontrun{dir.create("c:/temp")}
\dontrun{genpicfile(allrats,outdir="c:/temp/pic/")}
\dontrun{genlatex(allrats,outdir="c:/temp/pic/",picdir="c:/temp/pic/")}

## LATEX FILE:
#\documentclass{article}
#\usepackage{array,pictex}
#\begin{document}
#\input{scoretable}
#\input{picnice}
#\end{document}
}

\keyword{iplot}
