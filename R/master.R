## master.R
## Author          : Claus Dethlefsen
## Created On      : Thu Nov 29 21:28:29 2001
## Last Modified By: Claus Dethlefsen
## Last Modified On: Sun Sep 15 08:14:50 2002
## Update Count    : 295
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




## interessante funktioner: match(), intersect()

localmaster <- function(family,nw,prior=jointprior(nw)) {
  
  ## family: indices of a subset of nodes in the network 'nw'
  ## jointprior: jointprior(nw,N)
  ##
  ## Returns: the joint local master prior for the family


    listsum <- function(liste,idx=1:nrow(liste[[1]])) {
        ## sum elements of list containing a matrix as each element
        ## narrow down to liste[[i]][idx,idx] (always made to be a matrix)
        
        res <- matrix(0,
                      nrow(as.matrix(liste[[1]][idx,idx])),
                      ncol(as.matrix(liste[[1]][idx,idx])))
        
        for (i in 1:length(liste)) 
            res <- res + as.matrix(liste[[i]][idx,idx])
        
        res
    }


    
  ## determine indices of discrete and cont. nodes
  didx <- match(family,nw$discrete)
  didx <- didx[!is.na(didx)]
  cidx <- match(family,nw$continuous)
  cidx <- cidx[!is.na(cidx)]

  if (FALSE) {
  line();line()
  cat("didx=",didx,"cidx=",cidx,"\n")
}
  
  ## initialize
  alpha <- NA
  nu    <- NA
  rho   <- NA
  mu    <- NA
  phi   <- NA
  
  if (!length(cidx)>=1) { ## no cont. nodes
    alpha <- apply(prior$jointalpha,didx,sum)
    if (FALSE) {
      cat("No cont. nodes\n")
      print(didx)
    }
    ## Possible problem: didx
  }
  else if(!length(didx)>=1) { ## no disc. nodes
    if (FALSE) {
      cat("No disc. nodes\n")
      print(didx)
    }
    
    nu <- sum(prior$jointnu)
    rho<- sum(prior$jointrho)
    
    ##    if (length(cidx)==1) ## apply doesn't work on a vector
    ##      mu <- sum(prior$jointmu[,cidx]*c(prior$jointnu))/nu
    ##    else
    ##    mu <-
    ##    apply(prior$jointmu[,cidx]*c(prior$jointnu),2,sum)
    ##          /nu
    ##
    ## Better (?):

    if (FALSE) {
      cat("prior$jointmu[,cidx]:\n");print(prior$jointmu[,cidx])
      cat("prior$jointnu:\n");print(prior$jointnu)
      cat("as.matrix(prior$jointmu[,cidx]*c(prior$jointnu))\n")
      print(as.matrix(
                            prior$jointmu[,cidx]*c(prior$jointnu)
                            ))
    }
    M <- as.matrix(prior$jointmu[,cidx]*c(prior$jointnu))
    if (nrow(prior$jointmu)==1)
      dim(M) <- c(1,length(prior$jointmu[,cidx]))
      
    mu <- apply( M ,2,sum )/nu
    ## possible problem: cidx
    ## matrix for use in phi

    if (FALSE) {
      cat("mu=\n"); print(mu)
      cat("prior$jointmu:\n");print(prior$jointmu)
    }
    
    ss <- matrix(0,length(cidx),length(cidx))
    for (i in 1:nrow(prior$jointmu)) {
      if (FALSE) {
        cat("i=",i,"\n")
      }
      thismu <- as.matrix(prior$jointmu[i,cidx])
      mumean <- as.matrix(mu)
      if (FALSE) {
        cat("thismu=","\n"); print(thismu)
        cat("mumean=","\n"); print(mumean)
      }
        
      ss <- ss+prior$jointnu[i]*(thismu-mumean)%*%t(thismu-mumean)
    }
    
    phi<- listsum(prior$jointphi,cidx)+ss
  }
  
  else { ## mixed
    if (FALSE) {
      cat("Mixed nodes\n")
    }
    nu    <- apply(prior$jointnu   ,didx, sum)
    rho   <- apply(prior$jointrho  ,didx, sum)
    nconfig <- length(nu) # number of configs.
    mu    <- matrix(0,nconfig,length(cidx))
    phi    <- list()
    for (i in 1:nconfig) phi[[i]] <- matrix(0,length(cidx),length(cidx))

    if (FALSE) {
      line()
      cat("nu=\n");print(nu)
      cat("rho=\n");print(rho)
      cat("nconfig=\n");print(nconfig)
      cat("mu=\n");print(mu)
      cat("phi=\n");print(phi)
      line()
    }    
    ## find dimension from  levels of discrete nodes
    D <- c()
    for (i in 1:length(didx)) {
      ##      D <- c(D,nw$nodes[[didx[i]]]$levels)
      ## changed 13/12
      D <- c(D,nw$nodes[[nw$discrete[didx[i]]]]$levels)
    }
    ##    midx <- array(1:nconfig,dim=D) # index array
    jmu <- prior$jointmu

    if (FALSE) {
      cat("D=\n");print(D)
      cat("jmu=\n");print(jmu)
    }
    
    ## virker kun med een diskret forælder
    ## loop through all configurations 1:N
    for (i in 1:nrow(jmu)) {
      if (FALSE) {
        line()
        cat("Analysing tilstand",i,"ud af",nrow(jmu),"\n")
        cat("Jointalpha=\n")
        print(prior$jointalpha)
      }
      ## the corresp. configuration of the disc. variables in the
      ## joint distribution
      idx <- findex(i,dim(prior$jointalpha),config=FALSE)
      if (FALSE) {
        cat("idx=\n");print(idx)
        cat("didx=\n");print(didx)
      }
      y   <- findex(matrix(idx[didx],1),D,config=TRUE)
      if (FALSE) {
        cat("y=\n");print(y)
      }
      ##      y <- midx[idx[didx]] # y points to the index in the local master
#      cat("nu",i,"\n")
      mu[y,] <- mu[y,] + jmu[i,cidx]*prior$jointnu[i]
      phi[[y]][,] <- phi[[y]][,] +
        prior$jointphi[[i]][cidx,cidx]
#      cat("fandt vi problem\n")
    }
    for (i in 1:nrow(mu)) 
      mu[i,] <- mu[i,]/nu[i]
      
    ## adjust phi with sum(nu_j(mu_j-mean(mu))(mu_j-mean(mu))^t)
    ## are these always zero?
    for (i in 1:nrow(jmu)) {
#cat("men nu",i,"\n")    
      idx <- findex(i,dim(prior$jointalpha),config=FALSE)
      y   <- findex(matrix(idx[didx],1),D,config=TRUE)
      phi[[y]] <- phi[[y]] +
        prior$jointnu[i]*(jmu[i,cidx]-mu[y,])%*%t(jmu[i,cidx]-mu[y,])
      rownames(phi[[y]]) <- colnames(phi[[y]])
    }
    colnames(mu) <- colnames(phi[[1]])
#cat("men nu\n")    
    ## noget i denne stil:
    ## lav mu <- list() # den bliver passende lang (N)
    ## den skal initialiseres med 0-vektorer
    ## lav muidx <- array(1:N,dim=c())
    ## loeb igennem listen af jointmu og
    ## lav x <- findex() på hver entry.
    ## noget i stil med y<-muidx[x[didx]] må være index'et for
    ## mu og der skal altsaa summes hertil
    ## mu[[y]] <- mu[[y]] + jointmu[i]
    
    ##initialize
    
    
  }
  
  list(alpha=alpha,
       nu=nu,
       rho=rho,
       mu=mu,
       phi=phi)
}



printmaster <- function(nw,prior=jointprior(nw)) {
  ## find all families and run 'localmaster' on them
  ## print them.

  for (i in 1:nw$n) {
    cat("Family:",nw$nodes[[i]]$name," ")
    if (length(nw$nodes[[i]]$parents)>0) {
      for (j in 1:length(nw$nodes[[i]]$parents))
        cat(nw$nodes[[nw$nodes[[i]]$parents[j]]]$name," ")
    }
    cat("\n")
    print(
          localmaster(
                      sort(c(nw$nodes[[i]]$idx,
                        nw$nodes[[i]]$parents)),
                      nw,prior)
          )
    line()
  }
  
    invisible()
}
