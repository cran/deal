## postc0.R --- 
## Author          : Claus Dethlefsen
## Created On      : Tue Mar 12 06:52:02 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Wed Jun 04 11:33:16 2003
## Update Count    : 97
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


post0 <- function(mu,tau,rho,phi,y,timetrace=FALSE) {
    ## Posterior for continuous node with 0 parents
    if (timetrace) {t1 <- proc.time();cat("[post0 ")}
    
    if (FALSE) {
        cat("mu=\n"); print(mu)
        cat("tau=\n");print(tau)
        cat("rho=\n");print(rho)
        cat("phi=\n");print(phi)
        cat("y=\n");print(y)
    }

    mu.n  <- (tau*mu+sum(y))/(tau+length(y))
    tau.n <- tau + length(y)
    rho.n <- rho + length(y)    
    phi.n <- phi + (y - mu.n)%*%y + (mu - mu.n)*tau*mu

    if (FALSE) {
        print(mu.n)
        print(tau.n)
        print(rho.n)
        print(phi.n)
    }
    ##    print(phi)
    ##    print(rho)
    ##    print(diag(length(y)))
    ##    print(matrix(1/tau,length(y),length(y)))
        
    s <- as.numeric(phi)/rho*(diag(length(y)) + matrix(1/tau,length(y),length(y)))
    k <- lgamma( (rho + length(y))/2 ) - lgamma(rho/2)-0.5*log(det(rho*s*pi))
    ind <- log( 1 + (mahalanobis(y,center=mu,cov=s,inverted=FALSE))/rho)
    loglik <- k - (rho+length(y))/2 * ind
    
    if (timetrace) {
        t2 <- proc.time()
        cat((t2-t1)[1],"]")
    }
    
    list(mu=mu.n,tau=tau.n,rho=rho.n,phi=phi.n,loglik=loglik)
}


postc0c <- function(mu,tau,rho,phi,y,timetrace=FALSE) {
    ## Posterior for continuous node with 0 parents
    if (timetrace) {t1 <- proc.time();cat("[postc0 ")}
    
    
    ## call to C
    if (FALSE) {
    cat("Ready to call C:\n")
    cat("mu=\n");print(as.double(mu))
    cat("tau=\n");print(as.double(tau))
    cat("rho=\n");print(as.double(rho))
    cat("phi=\n");print(as.double(phi))
    cat("loglik=\n");print(as.double(0))
    cat("y=\n");print(as.double(y))
    cat("n=\n");print(as.integer(length(y)))

    if (FALSE) {
    cat("mu=\n");print(mu)
    cat("tau=\n");print(tau)
    cat("rho=\n");print(rho)
    cat("phi=\n");print(phi)
    cat("loglik=\n");print(0)
    cat("y=\n");print(y)
    cat("n=\n");print(length(y))
}
}
    
    res <- .C("postc0",
              mu =as.double(mu),
              tau=as.double(tau),
              rho=as.double(rho),
              phi=as.double(phi),
              loglik=as.double(0),
              as.double(y),
              as.integer(length(y)),
              package="deal"
              )
    
    
    if (timetrace) {
        t2 <- proc.time()
        cat((t2-t1)[1],"]")
    }
    
    list(mu=res$mu,tau=res$tau,rho=res$rho,phi=res$phi,loglik=res$loglik)
}
    
