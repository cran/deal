## network.R
## Author          : Claus Dethlefsen
## Created On      : Fri Nov 02 21:20:16 2001
## Last Modified By: Claus Dethlefsen
## Last Modified On: Sun Sep 15 08:15:27 2002
## Update Count    : 251
## Status          : Unknown, Use with caution!
###############################################################################
##
##    Copyright (C) 2002  Susanne Gammelgaard Bøttcher, Claus Dethlefsen
##
##    This program is free software; you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation; either version 2 of the License, or
##    (at your option) any later version.
##
##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.
##
##    You should have received a copy of the GNU General Public License
##    along with this program; if not, write to the Free Software
##    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
######################################################################

## Et netværk skal have en vektor af diskrete knude-idx og en vektor
## af kont. knude-idx fremfor 1:nd, (nd+1):nw$n.
## Og så skal vi selv lige finde ud af det hele fra en dataframe.
## dataframen skal vel ikke sendes med netværket rundt?

network <- function(df,specifygraph=FALSE,inspectprob=FALSE,equalcases=FALSE,vif=1.0,doprob=TRUE,tvar=NA,smalldf=NA) {
  ## creator for class 'network'
  ## df is a dataframe with one column per variable and one row per
  ## observation. Discrete variables are factors. We assume complete
  ## data, that is no NA's and at least one observation for each
  ## configuration of the factors.
    ## vif: variance inflation factor. The reported variances are
    ## multiplied by this factor.
  ##
  ## We create a 'trivial' network, which is a network without any arrows.

  if (length(dim(df))<1) stop("Can't handle networks with one node, sorry\n")
  
  nw   <- list()
  nw$n <- ncol(df)  ## df must have at least 2 columns...
  
  nw$discrete <- c()
  nw$continuous <- c()
  
  nw$nodes <- list()
  for (i in 1:nw$n) {
    ## create one node per column
    if (is.factor(df[,i])) {
      ## the node is discrete
      nw$nodes[[i]] <- node(i,c(),"discrete",
                            names(df)[i],
                            length(levels(df[,i])),
                            levels(df[,i]))
      nw$discrete <- c(nw$discrete,i)
    }
    else {
      ## the node is continuous
      nw$nodes[[i]] <- node(i,c(),"continuous",
                            names(df)[i])
      nw$continuous <- c(nw$continuous,i)

    }
  }

  if (!is.na(tvar)) {
      for (j in tvar)
          nw$nodes[[j]]$tvar <- TRUE
  }
  
  nw$nd <- length(nw$discrete)
  nw$nc <- length(nw$continuous)
  stopifnot(nw$nd+nw$nc==nw$n) # invariant

  names(nw$nodes) <- names(df)
  
  class(nw) <- "network"

  if (specifygraph) {
##    cat("Specifygraph not yet implemented, sorry... using trivial graph\n")
#    cat("Specify prior network\n")
    nw <- drawnetwork(nw,nocalc=TRUE,smalldf=smalldf)$nw
    
  }
  

  if (doprob) 
    nw <- prob(x=nw,df=df,equalcases=equalcases,vif=vif,smalldf=smalldf)

  if (inspectprob) nw <- inspectprob(nw)
  
  nw
}




print.network <- function(x,filename=NA,master=FALSE,condposterior=FALSE,
                          condprior=FALSE,...) {
    nw <- x
    str <- paste("## ",nw$n,"(",nw$nd,"discrete+",nw$nc,") nodes;score=",
               nw$score,";relscore=",nw$relscore,"\n")
  if (is.na(filename)) cat(str)
  else cat(str,file=filename)

  for (i in 1:nw$n)
    print(nw$nodes[[i]],filename=filename,master,condposterior,condprior)
  invisible(nw)
}

plot.network <- function(x,scale=10,unitscale=1.3,cexscale=10,rotate=pi/4,length=.25,notext=FALSE,sscale=.7*scale,showban=TRUE,...) {

    nw <- x
    
    plot(0,0,xlim=c(-scale,scale),
       ylim=c(-scale,scale),type="n",
       axes=FALSE,xlab="",ylab="",...)

  unit <- 2*pi/nw$n

  ## show nodes
  for (i in 1:nw$n) 
    plot(nw$nodes[[i]],
         where=0.7*scale*c(cos(unit*i+rotate),sin(unit*i+rotate)),
         cexscale=cexscale,notext=notext,...)

  ## show score and relscore
  if (length(nw$score)>0 && !notext) {
#  if (length(nw$score)>0 ) {
    
    string <- paste("Score:",format(nw$score,2))
    if (length(nw$relscore)>0)
      string <- paste(string,"\n","Relscore:",format(nw$relscore,2))
    
    text(0,0.97*scale,string)
  }

  ## show banlist
  if (showban) {
##      print(nw$banlist)
      if (!is.null(nw$banlist))
          if (nrow(nw$banlist)>0) {
##                    cat("showing ban-arrows\n")
                    bl <- nw$banlist
                    for (i in 1:nrow(bl)) {
                        from <- bl[i,2]
                        to   <- bl[i,1]
                        x  <- sscale*c(cos(unit*from+rotate),
                                       sin(unit*from+rotate)) 
                        y  <- sscale*c(cos(unit*to+rotate),
                                       sin(unit*to+rotate)) 
                        u <- (x - y) / sqrt(sum( (x-y)^2 )) 
        
                        x <- x - u*unitscale 
                        y <- y + u*unitscale 
                        arrows( y[1],y[2],x[1],x[2],length=length,col="red",lty=2)
                    } ## for
                } ## if (nrow...)
  } ## if (showban)


  ##< show arrows

  ## Skal lige skaleres ved at lave en enhedsvektor i pilens retning
  ## og så trække radius fra i begge ender

#  sscale <- 0.7*scale

  for (i in 1:nw$n) {
    ni <- nw$nodes[[i]]    # node i
    if (length(ni$parents)>0) {
      for (j in 1:length(ni$parents)) {
        x  <- sscale*c(cos(unit*i+rotate),sin(unit*i+rotate)) # coords of ni
        pj <- ni$parents[j]  # parent j (index)
        y  <- sscale*c(cos(unit*pj+rotate),sin(unit*pj+rotate)) # coords of pj
        
        u <- (x - y) / sqrt(sum( (x-y)^2 )) # unit vector from y to x
        
        x <- x - u*unitscale 
        y <- y + u*unitscale 

        arrows( y[1],y[2],x[1],x[2],length=length,...)
      }
    }
  }
    
}

prob.network <- function(x,df,equalcases=FALSE,vif=1.0,smalldf=NA) {
  ## calculate initial probability
  x$nodes <- lapply(x$nodes,prob,x,df,equalcases,vif,smalldf=smalldf)
  x
}


