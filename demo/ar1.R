## ar1.R --- 
## Author          : Claus Dethlefsen
## Created On      : Fri Mar 15 07:43:52 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Tue May 28 09:31:06 2002
## Update Count    : 45
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

arsim <- function(n=100, rho=0.99, sigma2=.5, X0=0) {
  res <- rep(NA,n)
  res[1] <- rnorm(1,X0,sqrt( sigma2/(1-rho^2) ) )
  for (i in 2:n)
    res[i] <- rho*res[i-1] + rnorm(1,0,sqrt(sigma2))
  res
}

N      <- 2000
rho    <- .9
sigma2 <- 1

X      <- arsim(n=N,rho=rho,sigma2=sigma2)
X      <- data.frame(X=X)
ar1    <- timeslice(X,1)$bigdf
    
ar1.nw    <- network(ar1)
ar1.prior <- jointprior(ar1.nw,12)
ar1.nw    <- learn(ar1.nw,ar1,ar1.prior)$nw

arfamily  <- networkfamily(ar1,ar1.nw,ar1.prior)$nw

thenet    <- arfamily[[2]]

cat("Posterior rho   \t",
    thenet$nodes[[1]]$condposterior[[1]]$mu[2],
    " (prior=",thenet$nodes[[1]]$condprior[[1]]$mu[2],")",
    "\t\t- true value:\t",rho,"\n",
    "Posterior sigma2\t",
    thenet$nodes[[1]]$condposterior[[1]]$phi/
    (thenet$nodes[[1]]$condposterior[[1]]$rho-2),
    "  (prior=", thenet$nodes[[1]]$condprior[[1]]$phi/
    (thenet$nodes[[1]]$condprior[[1]]$rho-2)
    ,")",
    "\t- true value:\t",sigma2,"\n",sep="")
