## postc.R --- 
## Author          : Claus Dethlefsen
## Created On      : Tue Mar 12 06:52:02 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Mon Nov 04 00:45:20 2002
## Update Count    : 140
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

postc <- function(mu,tau,rho,phi,y,z,timetrace=FALSE) {
    ## Posterior for continuous node with continuous parents
    ## written as a for-loop in R (slow)
    if (timetrace) {t1 <- proc.time();cat("[postc ")}
    
    
    if (FALSE) {
        cat("mu=\n"); print(mu)
        cat("tau=\n");print(tau)
        cat("rho=\n");print(rho)
        cat("phi=\n");print(phi)
        cat("y=\n");print(y)
        cat("z=\n");print(z)
    }
    
    loglik <- 0
    for (i in 1:length(y)) {
        if (FALSE) {
            cat("rho=\n"); print(rho)
            cat("y[i]=\n");print(y[i])
            cat("1=\n");print(1)
            cat("mu=\n");print(mu)
            cat("phi=\n");print(phi)
            cat("tau=\n");print(tau)
        }            
        if (FALSE) {
            posterior <- posterior.cont(rho,y[i],z[i,],mu,phi,tau)
            cat("posterior\n")
            print(posterior)
        }
##        loglik <- loglik + posterior$lik
##        mu    <- posterior$mu
##        tau   <- posterior$tau
##        rho   <- posterior$rho
##        phi   <- posterior$phi

## likelihood
        logscale <- log(phi) + log( 1 + t(z[i,])%*%solve(tau)%*%z[i,])
        logk     <- lgamma( (rho+1)/2 ) - lgamma(rho/2) - 0.5*(logscale  +  log(pi)) 
        mscore   <- logk - 0.5*(rho+1)*log(1 + ((y[i] - z[i,]%*%mu)^2)/exp(logscale))

        loglik <- loglik + mscore

## update
        oldtau <- tau
        oldmu <- mu
        tau <- tau + z[i,]%*%t(z[i,])
        mu <- solve(tau)%*%(oldtau%*%mu+z[i,]*y[i])
        rho<- rho + 1
        phi<- phi + (y[i]-t(z[i,])%*%mu)*y[i] + t(oldmu-mu)%*%oldtau%*%oldmu
        if (FALSE) {
            cat("new values\n")
            cat("loglik=",loglik,"\n")
            cat("tau=",tau,"\n")
            cat("mu=",mu,"\n")
            cat("rho=",rho,"\n")
            cat("phi=",phi,"\n")
            cat("slut\n")
        }
    }

  if (timetrace) {
    t2 <- proc.time()
    cat((t2-t1)[1],"]")
  }

    list(mu=mu,tau=tau,rho=rho,phi=phi,loglik=loglik)
}


post <- function(mu,tau,rho,phi,y,z,timetrace=FALSE) {
    ## Posterior for continuous node with continuous parents
    ## written as matrix notation in R
    if (timetrace) {t1 <- proc.time();cat("[post ")}
    
    if (FALSE) {
        cat("mu=\n"); print(mu)
        cat("tau=\n");print(tau)
        cat("rho=\n");print(rho)
        cat("phi=\n");print(phi)
        cat("y=\n");print(y)
        cat("z=\n");print(z)
    }
    

    mu.n  <- solve(tau+t(z)%*%z)%*%(tau%*%mu+t(z)%*%y)
    tau.n <- tau + t(z)%*%z
    rho.n <- rho + length(y)
    phi.n <- phi + t(y - z%*%mu.n)%*%y + t(mu - mu.n)%*%tau%*%mu


    loglik <- 0
    s <- as.numeric(phi)/rho*(diag(nrow(z))+ z%*%solve(tau)%*%t(z))
    k <- lgamma( (rho + length(y))/2 ) - lgamma(rho/2)-0.5*log(det(rho*s*pi))
    ind <- log( 1 + (mahalanobis(y,center=z%*%mu,cov=s,inverted=FALSE))/rho)
    loglik <- as.numeric(k) - (rho+length(y))/2 * ind

        
    if (FALSE) {
        line()
        print(sum(loglik))
    }

    if (timetrace) {
        t2 <- proc.time()
        cat((t2-t1)[1],"]")
    }

    list(mu=mu.n,tau=tau.n,rho=rho.n,phi=phi.n,loglik=loglik)
}

postM <- function(mu,tau,rho,phi,y,z,timetrace=FALSE) {
    ## Posterior for continuous node with continuous parents
    ## written as Matrix notation in R (needs Matrix)
    if (timetrace) {t1 <- proc.time();cat("[postM ")}
    
    if (FALSE) {
        cat("mu=\n"); print(mu)
        cat("tau=\n");print(tau)
        cat("rho=\n");print(rho)
        cat("phi=\n");print(phi)
        cat("y=\n");print(y)
        cat("z=\n");print(z)
    }
    z <- as.Matrix(z)
    mu.n  <- solve(as.Matrix(tau+t(z)%*%z))%*%(tau%*%mu+t(z)%*%y)
    tau.n <- tau + t(z)%*%z
    rho.n <- rho + length(y)
    phi.n <- phi + t(y - z%*%mu.n)%*%y + t(mu - mu.n)%*%tau%*%mu


    loglik <- 0
    s <- as.numeric(phi)/rho*(diag(nrow(z))+ z%*%solve(tau)%*%t(z))
    k <- lgamma( (rho + length(y))/2 ) - lgamma(rho/2)-0.5*log(det(rho*s*pi))
    ind <- log( 1 + (mahalanobis(y,center=z%*%mu,cov=s,inverted=FALSE))/rho)
    loglik <- as.numeric(k) - (rho+length(y))/2 * ind

        
    if (FALSE) {
        line()
        print(sum(loglik))
    }

    if (timetrace) {
        t2 <- proc.time()
        cat((t2-t1)[1],"]")
    }

    list(mu=mu.n,tau=tau.n,rho=rho.n,phi=phi.n,loglik=loglik)
}


postcc <- function(mu,tau,rho,phi,y,z,timetrace=FALSE) {
    ## Posterior for continuous node with x parents
    ## written as for-loop in C (fast)
    if (timetrace) {t1 <- proc.time();cat("[postcc ")}
    
    
    ## call to C
    if (FALSE) {
        print(z)
        cat("Ready to call C:\n")
        cat("mu=\n");print(as.double(mu))
        cat("tau=\n");print(as.double(t(tau)))
        cat("rho=\n");print(as.double(rho))
        cat("phi=\n");print(as.double(phi))
        cat("loglik=\n");print(as.double(0))
        cat("y=\n");print(as.double(y))
        cat("z=\n");print(as.double(t(z)))
        cat("n=\n");print(as.integer(length(y)))
        cat("d=\n");print(as.integer(ncol(z)))
        
    }

    res <- .C("postc",
              mu =as.double(c(mu)),
              tau=as.double(t(tau)),
              rho=as.double(rho),
              phi=as.double(phi),
              loglik=as.double(0),
              as.double(y),
              as.double(t(z)),
              as.integer(length(y)),
              as.integer(ncol(z))
              )
    if (FALSE) {
    res <- list()
    res$mu <- mu
    res$tau<- tau
    res$rho<- rho
    res$phi <- phi
    res$loglik <- 0
}
    if (timetrace) {
        t2 <- proc.time()
        cat((t2-t1)[1],"]")
    }
    if (FALSE) 
        print(res)    
    list(mu=res$mu,tau=matrix(res$tau,ncol(z),ncol(z)),rho=res$rho,phi=res$phi,loglik=res$loglik)
}
    
