## conditional.R
## Author          : Claus Dethlefsen
## Created On      : Sun Dec 02 14:18:04 2001
## Last Modified By: Claus Dethlefsen
## Last Modified On: Sun Sep 15 08:05:04 2002
## Update Count    : 281
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

conditional.cont <- function(A,mu,nu,rho,phi) {
    ## Conditional distribution for continuous node with index A
    ## The master parameters mu, nu, rho and phi
    ## See Bottcher (2002) for details.
    
    B <- A ## renaming due to compatibility
    
  if (FALSE) { ## debug info
    
    mu <- matrix(mu,ncol=1)
    
    cat("conditional.cont(\n")
    cat("B=\n");print(B)
    cat("mu=\n");print(mu)
    cat("nu=\n");print(nu)
    cat("rho=\n");print(rho)
    cat("phi=\n");print(phi)
    cat(")\n")
  }
  
  ## calculate conditional probabilities
  ## p. 14 in Bottcher
  ##
  A <- setdiff(1:ncol(phi),B)
  if (length(A)<1) A <- TRUE
  
  ##cat("A=\n");print(A)
  
  rho.BlA <- rho + length(A)
  phi.AA.inv <- solve(phi[A,A])
  phi.tmp <- phi[B,A]%*%phi.AA.inv
  phi.BlA <- phi[B,B] - phi.tmp%*%phi[A,B]
  mu.BlA  <- c(mu[B] - phi.tmp%*%mu[A], phi.tmp)
  tau.BlA.inv.11 <- 1/nu + t(mu[A])%*%phi.AA.inv%*%mu[A]
  tau.BlA.inv.22 <- phi.AA.inv
  tau.BlA.inv.12 <- -t(mu[A]%*%phi.AA.inv)
  
  ##tau.inv <- rbind(cbind(tau.BlA.inv.11,tau.BlA.inv.12),
  ##                   cbind(t(tau.BlA.inv.12),tau.BlA.inv.22)
  ##                   )
  ## changed 13/12
  tau.inv <- rbind(cbind(tau.BlA.inv.11,t(tau.BlA.inv.12)),
                   cbind(tau.BlA.inv.12,tau.BlA.inv.22)
                   )
  tau <- solve(tau.inv)

  ##  cat("=\n")
  ##  cat("tau=\n");print(tau)
  ##  cat("phi=\n");print(phi.BlA)
  ##  cat("mu=\n");print(mu.BlA)
  ##  cat("rho=\n");print(rho.BlA)
  
    list(tau=tau,phi=phi.BlA,mu=mu.BlA,rho=rho.BlA)
}

conditional.disc <- function(A,master) {
    list(list(alpha=apply(master,A,sum)))
}

conditional <- function(A,master,nw) {
  ## From node index A and given the master prior, calculate the
  ## conditional of A given the parents. (In nw, we use parents,
  ## discrete and continuous)

  ## A is always 1-dimensional

  if (FALSE) {
      cat("Conditional(",A,")\n")
      cat("Master:\n")
      print(master)
  }
  
  ## dette skal ikke matches i nw, men i familien
  ## family: (sorted) indices of A and its parents
  family <- sort(c(nw$nodes[[A]]$idx,nw$nodes[[A]]$parents))

  ## Example:
  ##     nw$discrete   <- c(1,4,5,6)
  ##     nw$continuous <- c(2,3,7)
  ##     A             <- 4
  ##     family        <- c(1,3,4,6)
  ##     intersect(family,nw$discrete) = c(1,4,6)
  ##     didx <- 2
  ##     intersect(family,nw$continuous) = c(3)
  ##     cidx <- c()
  ##
  ## didx and cidx are used as indices for A in the master
  didx    <- match(A,intersect(family,nw$discrete))
  didx    <- didx[!is.na(didx)]
  cidx    <- match(A,intersect(family,nw$continuous))
  cidx    <- cidx[!is.na(cidx)]

  if (FALSE) {
    cat("Family=",family,"\n")
    cat("didx=",didx,"\n")
    cat("cidx=",cidx,"\n")
  }

  if (nw$nodes[[A]]$type=="continuous") {
    cond <- list()

    if (!is.list(master$phi)) {
      cond[1] <- list(conditional.cont(cidx,
                                       master$mu,
                                       master$nu,
                                       master$rho,
                                       master$phi
                                       ))
    }
    else {
      for (i in 1:length(master$phi)) {

        if (FALSE) {
          line()
          print(i)
          print(family)
          print(master)
        }
        cond[i] <- list(conditional.cont(cidx,
                                         master$mu[i,],
                                         master$nu[i],
                                         master$rho[i],
                                         master$phi[[i]]
                                         ))
      }
    }
  }
  else if (nw$nodes[[A]]$type=="discrete") {
    ##    cond <- conditional.disc(didx,master$alpha)
    ##    cond <- conditional.disc(family,master$alpha)

    if (FALSE) {
      cat("nw$discrete=",nw$discrete,"\n")
      cat("match=",match(family,nw$discrete),"\n")
      cat("master$alpha=",master$alpha,"\n")
      
    }

    ## er cond ikke bare det samme som masteren?
    cond <- list(list(alpha=master$alpha)) ## 27/2 2002
  }
  else stop("Something happened\n")

  cond
}

## test
##
## a <- localmaster(1:4,newrat,jointprior(newrat,12))
## conditional.cont(1,a$mu[1,],a$nu[1,1],a$rho[1,1],a$phi[[1]])
  

