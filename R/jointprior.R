## jointprior.R
## Author          : Claus Dethlefsen
## Created On      : Tue Nov 27 09:03:14 2001
## Last Modified By: Claus Dethlefsen
## Last Modified On: Thu Jan 16 14:22:41 2003
## Update Count    : 190
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

jointprior <- function(nw,N=NA,phiprior="bottcher",
                       timetrace=FALSE) {
    
    ## Setup a joint prior distribution for the parameters
    ## phiprior="bottcher" or "heckerman"
    ##
    ## The parameters for the discrete part of the network are stored in
    ## multi-way arrays.
    ## The parameters for the mixed part of the network are stored in
    ## a matrix (mu) and lists with one row or entry per configuration
    ## of the discrete variables.
    ## For translation back and forth between configurations of the
    ## discrete variables and the entry number in the matrix/list, see
    ## the findex function.
    
    if (timetrace) {t1 <- proc.time();cat("[Jointprior ")}
    
    ##  cat("Using",phiprior,"prior\n")
    
    ## ##############################################
    ## Parameters for discrete variables
    ## ##############################################
    
    ## jointalpha
    if (nw$nd>0) { ## at least one discrete node
        
        jointprob <- jointdisc(nw,timetrace=timetrace)
        ## determine smallest possible imaginary sample size
        ## and reset it if too small.
        
        ## Instead of calculating jointalpha, I could find the minN by
        ## taking the min(prob) of each discrete node and multiply together.
        if (nw$nc>0) {
            minN <- min(2/jointprob)
            if (is.na(N)) N <- minN
            if (N<minN) {
                cat("Warning: Your choice of imaginary sample size is very low\n")
                cat("We advice you to set the imaginary sample size to",minN,"\n")
            }
        }
        cat("Imaginary sample size:",N,"\n")
        jointalpha <- jointprob * N
        
        jointnu   <- jointalpha
        jointrho  <- jointalpha
    }
    else {  ## no discrete nodes
        jointnu   <- N
        jointrho  <- N
        jointalpha <- N
    }

    if (FALSE) cat("Ready for cont. variables\n")
  
    ## ##############################################
    ## Parameters for continuous variables
    ## ##############################################

    if (nw$nc>0) { ## at least one cont. node
        NN <- prod(dim(jointalpha))
        if (FALSE) cat("NN=",NN,"\n")
        
        ## dan alle teksterne i den rigtige rækkefølge
        if (nw$nd>0) {
            Dim <- dim(jointalpha)
            dparents <- nw$discrete
            lvek <- c()
            for (i in 1:NN) {
                cf <- findex( i, Dim, FALSE)
                label <- ""
                for (j in 1:ncol(cf)) {
                    label <- paste(label, nw$nodes[[dparents[j]]]$levelnames[cf[1,j]]
                                   ,sep=":")
                }
                lvek <- c(lvek,label)
            }
        }
        
        if (FALSE) cat("setting up matrices\n")
        jointmu    <- matrix(NA,NN,nw$nc)
        jointsigma <- list()
        jointphi   <- list()

        ## generate mu-vector and sigma2-vector
        jcont  <- jointcont(nw,timetrace=timetrace)
        jointmu <- jcont$mu
        jointsigma <- jcont$sigma2
        dnames <- colnames(jointmu)

        if (FALSE) {
            line()
            cat("(jointprior:)\n")
            cat("jointmu:\n");print(jointmu)
            cat("jointsigma=\n");print(jointsigma)
            cat("dnames=\n");print(dnames)
        }
    
        for (i in 1:NN) {
            
            if (phiprior=="bottcher") {
                jointphi[[i]]   <- jointsigma[[i]] * (jointnu[i]-1)
            }
            else {
                if (phiprior=="heckerman") {
                    jointphi[[i]] <- (jointrho[i]-2)/(jointnu[i]+1)*
                        jointnu[i]*jointsigma[[i]]
                }
                else
                    stop("No such phiprior implemented")
            }
            
            
            ## set names
            colnames(jointmu)         <- dnames
            colnames(jointsigma[[i]]) <- dnames
            rownames(jointsigma[[i]]) <- dnames
            colnames(jointphi[[i]])   <- dnames
            rownames(jointphi[[i]])   <- dnames
        }
        
        ## ALSO SET NAMES ON THE LIST
        if (nw$nd>0) {
            names(jointsigma) <- lvek
            names(jointphi)   <- lvek
            rownames(jointmu) <- lvek
        }
    }
    
    else { ## no cont. nodes
        jointphi   <- NA
        jointmu    <- NA
        jointsigma <- NA
    }
    
    if (timetrace) {
        t2 <- proc.time()
        cat((t2-t1)[1],"]\n")
    }
    list(jointalpha=jointalpha, jointnu=jointnu, jointrho=jointrho,
         jointmu=jointmu,jointsigma=jointsigma,jointphi=jointphi)
}

