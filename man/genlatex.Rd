%                               -*- Mode: Rd -*- 
% genlatex.Rd --- 
% Author          : Claus Dethlefsen
% Created On      : Sat May 25 23:02:07 2002
% Last Modified By: Claus Dethlefsen
% Last Modified On: Tue May 28 19:31:30 2002
% Update Count    : 2
% Status          : Unknown, Use with caution!
% 

\name{genlatex}
\alias{genlatex}
\alias{genpicfile}

%- Also NEED an `\alias' for EACH other topic documented here.

\title{From a networkfamily, generate LaTeX output}

\description{The networks in a networkfamily is arranged as
  pictex-graphs in a LaTeX-table. 
}

\usage{
genlatex(nwl,outdir="pic/",prefix="scoretable",picdir="",picpre="pic",
         ncol=5,nrow=7,width=12/ncol,vadjust=-1.8)
genpicfile (nwl,outdir="pic/",prefix="pic",w=1.6,h=1.6,bigscale=3) 
}

%- maybe also `usage' for other objects documented here.
\arguments{
  \item{nwl}{Object of class \code{networkfamily} containing a list of
    objects of class \code{network}.}
  \item{outdir}{Directory for storing output.}
  \item{prefix}{The filename (without extension) of the LaTeX file. The
    filenames of the picfiles begin with the given prefix.} 
  \item{picdir}{Directory where pic-files are stored.}
  \item{picpre}{Prefix for pic-files.}
  \item{ncol}{Number of columns in LaTeX table.}
  \item{nrow}{Number of rows in LaTeX table.}
  \item{width}{Width of each cell in the LaTeX table.}
  \item{vadjust}{Vertical adjustment in LaTeX table.}
  \item{w}{Width of pictex objects}
  \item{h}{Height of pictex objects}
  \item{bigscale}{Scaling of the best network, which is output in 'nice.tex'}
  
}

\value{Files:

  \{outdir\}\{picpre\}xx.tex : one pictex file for each network in the
  networkfamily, indexed by xx.

  \{outdir\}\{prefix\}.tex   : LaTeX file with table including all pictex files.

  \{outdir\}nice.tex       : pictex file with the best network.
}

\seealso{\code{\link{networkfamily}}}

\author{
  Susanne Gammelgaard Bøttcher \email{alma@math.auc.dk}, \cr
  Claus Dethlefsen \email{dethlef@math.auc.dk}.
}

%\note{ ~~further notes~~ }

% ~Make other sections like WARNING with \section{WARNING }{....} ~

\examples{
data(rats)
allrats <- networkfamily(rats,network(rats))$nw
allrats <- nwfsort(allrats)

## delete # to run the following three commands
#dir.create("c:/temp")
#genpicfile(allrats,outdir="c:/temp/pic/")
#genlatex(allrats,outdir="c:/temp/pic/",picdir="c:/temp/pic/")

## LATEX FILE:
#\documentclass{article}
#\usepackage{array,pictex}
#\begin{document}
#\input{scoretable}
#\end{document}
}

\keyword{iplot}
