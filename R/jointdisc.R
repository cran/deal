## jointdisc.R --- 
## Author          : Claus Dethlefsen
## Created On      : Wed Mar 06 12:52:57 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Tue Dec 10 19:19:18 2002
## Update Count    : 27
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

jointdisc <- function(nw,timetrace=FALSE) {
    ## From the discrete part of nw, the joint distribution is
    ## determined from the local distributions in nodes$prob.
    ##
    ## If eg. A|B,C, B|C, C are given, the joint distribution of A,B,C
    ## is returned
    ##
    
    if (timetrace) {t1 <- proc.time();cat("[jointdisc ")}
    
    ## First, determine the discrete nodes and their dimensions
    
    Dim <- c()
    lablist <- list()
    for (i in nw$discrete) {
        Dim <- c(Dim, nw$nodes[[i]]$levels)
        lablist <- c(lablist,list(nw$nodes[[i]]$levelnames))
    }
    
    ## Dim is the dimension of the returned joint distribution
    jointprob <- array(1,Dim)
    dimnames(jointprob) <- lablist
    
    ## for each node, multiply jointprob by the local distribution
    ## (blown up appropriately).
    
    for (nid in nw$discrete) {
        node    <- nw$nodes[[nid]] 
        Pn      <- node$prob        ## the local distribution
        parents <- node$parents     ## the parents, 
        if (nw$nd>0)    dparents<- sort(intersect(parents,nw$discrete))
        else dparents <- c()

        idx <- c(node$idx, dparents) ## sequence in Pn
        pidx<- 1:length(idx)         ## corresponding idx
        jidx<- 1:nw$nd               ## idx in jointprior
    
        ## dimension of c(node,parents)
        nDim <- c(node$levels)
        for (i in dparents) 
            nDim <- c(nDim,nw$nodes[[i]]$levels)
        
        ## blow up
        ## first, permute Dim appropriately
        ivek <- c(pidx,setdiff(jidx,pidx))
        jDim <- Dim[ivek]
        bigPn <- array(Pn,jDim)
        ## permute indices appropriately
        permvek <- match(1:nw$nd,ivek)
        bigPn <- aperm(bigPn, permvek)
        
        jointprob <- jointprob * bigPn
        if (FALSE) {
            line()
            cat("(discjoint:)\n")
            cat("Node ", node$name,"\n")
            cat("Discrete parents:", dparents,"\n")
            cat("Pn:\n");print(Pn)
            cat("idx in Pn:",idx,"\n")
            cat("pidx in Pn:",pidx,"\n")
            cat("jidx in joint:",jidx,"\n")
            cat("nDim=",nDim,"\n")
            ##      cat("TD=",TD,"\n")
            cat("jDim=\n");print(jDim)
            cat("permvek:",permvek,"\n")
            cat("bigPn:\n");print(bigPn)
        }
    } ## for
    
    if (timetrace) {
        t2 <- proc.time()
        cat((t2-t1)[1],"]")
    }
    jointprob
} ## function discjoint
  
