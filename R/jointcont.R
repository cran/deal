## jointcont.R
## Author          : Claus Dethlefsen
## Created On      : Wed Mar 06 12:52:57 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Sun Sep 15 08:10:56 2002
## Update Count    : 252
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

jointcont <- function(nw,timetrace=FALSE) {
    ## From the continuous part of nw, the joint distribution is
    ## determined from the local distributions in nodes$prob.
    ##
    ## If eg. x|y,z, y|z, z are given, the joint distribution of x,y,z
    ## is returned
    ##
    if (FALSE) cat("Joint cont. entered\n")
    
    if (timetrace) {t1 <- proc.time();cat("[jointcont ")}
    
    ## First, determine the discrete nodes and their dimensions
    Dim <- c()
    TD <- 1
    if (nw$nd>0) {
        for (i in nw$discrete) {
            Dim <- c(Dim, nw$nodes[[i]]$levels)
        }
        TD <- prod(Dim)
    }

    if (FALSE) cat("Running\n")
    ## dan alle teksterne i den rigtige rækkefølge
    lablist <- c()
    if (nw$nd>0) {
        for (i in 1:TD) {
            cf <- findex( i, Dim, FALSE)
            label <- ""
            for (j in 1:ncol(cf)) {
                label <- paste(label, nw$nodes[[nw$discrete[j]]]$levelnames[cf[1,j]]
                               ,sep=":")
            }
            lablist <- c(lablist,label)
        }
    }
    
    
    if (FALSE) {
        line()
        cat("(jointcont:)\n")
        cat("TD=",TD,"\n")
        cat("Dim=",Dim,"\n")
        cat("length(unlist(lablist))=",length(lablist),"\n")
        cat("lablist:\n");print(lablist)
    }
    
    ## determine the continuous nodes
    lab <- c()
    for (i in nw$continuous)
        lab <- c(lab,nw$nodes[[i]]$name)
    
    mu     <- matrix(0,TD,nw$nc) 
    sigma2 <- matrix(0,nw$nc,nw$nc)
    sigma2list <- list()
    colnames(mu) <- lab
    rownames(mu) <- lablist
    rownames(sigma2) <- colnames(sigma2) <- lab
    for (i in 1:TD) sigma2list[[i]] <- sigma2
    names(sigma2list) <- lablist
    
    if (FALSE) {
        cat("Ready to roll\n")
        cat("sigma2list=\n");print(sigma2list)
        cat("mu=\n");print(mu)
    }
    
    calclist <- c()
    allnodes <- c(nw$continuous)
    
    ##  cat("allnodes=",allnodes,"\n")
    
    nidx <- 0
    while ( length( setdiff(allnodes,calclist) )>0 ) {
        
        nidx <- nidx%%(nw$nc)+1
        nid  <- nw$continuous[nidx]
        
        if (FALSE) {
            cat("calclist=",calclist,"\n")
            cat("mangler: ", setdiff(allnodes,calclist),"\n")
            cat("trying nidx=",nidx,"nid=",nid,"\n")
        }
        
        if ( length(intersect(nid,calclist))>0) {
            ##  cat("Skipping,length(intersect(nid,calclist))>0\n")
            next
        }
        
        node    <- nw$nodes[[nid]] 
        Pn      <- node$prob        ## the local distribution
        parents <- node$parents     ## the parents, 
        if (nw$nc>0)    cparents<- sort(intersect(parents,nw$continuous))
        else cparents <- c()
        if (nw$nd>0)    dparents<- sort(intersect(parents,nw$discrete))
        else dparents <- c()
        
        if ( length( setdiff(cparents,calclist) ) > 0  ) {
            ##  cat("Skipping, length( setdiff(cparents,calclist) ) > 0\n")
            next
        }
        if (FALSE) {
            line()
            cat("(jointcont)\n")
            cat("node:",node$name,"\n")
            cat("Pn=\n");print(Pn)
            cat("parents:",parents,"\n")
            cat("cparents:",cparents,"\n")
            cat("dparents:",dparents,"\n")
        }
        
        
        ## calculate unconditional mu, sigma2 from node|parents
        if (!length(cparents)>0) {
            if (FALSE) 
                cat("No continuous parents\n")
            ## her skal vi bestemme konfigurationerne af de diskrete
            ## forældre
            ## dernæst skal disse 'blæses op' og indexer beregnes så det er
            ## de rigtige steder i mu og sigma2list vi retter.
            ##        line()
            ##        cat("(jointcont:)\n")
            M <- array(1:TD,dim=Dim)
            ##        print(M)
            if (length(dparents)>0) {
                if (FALSE) cat("diskrete parents\n")
                
                mdim <- c()
                for (i in dparents) 
                    mdim <- c(mdim,nw$nodes[[i]]$levels)
                m <- array(1:TD,dim=mdim) # skal der stå TD her?
                
                if (FALSE) {
                    cat("mdim=",mdim,"\n")
                    cat("m=\n"); print(m)
                }
                
                ## inflate
                ## first, permute Dim appropriately
                ivek <- c(match(dparents,nw$discrete),
                          match(setdiff(nw$discrete,dparents),nw$discrete))
                jDim <- Dim[ivek]
                bigM <- array(m,jDim)
                if (FALSE) {
                    cat("ivek=",ivek,"\n")
                    cat("jDim=",jDim,"\n")
                    cat("bigM=\n"); print(bigM)
                }
                
                ## permute back
                ##          permvek <- match(nw$discrete,ivek)
                permvek <- match(1:nw$nd,ivek)
                bigM <- aperm(bigM, permvek)
                if (FALSE) {
                    cat("permvek:",permvek,"\n")
                    cat("bigM\n");         print(bigM)
                }
                for (i in 1:length(unique(bigM))) { ## not nice
                    theidx <- M[bigM==i]
                    cf <- findex(theidx,Dim,config=FALSE)
                    cfm<- cf[,match(dparents,nw$discrete)]
                    if (FALSE) {
                        line()
                        cat("i=",i,"\n")
                        cat("theidx=",theidx,"\n")
                        cat("cf=\n");print(cf)
                        cat("cfm=",cfm,"\n");print(cfm)
                    }
                    cfm <- matrix(cfm,nrow=length(theidx))
                    theidxm <- findex(cfm,mdim,config=TRUE)
                    paridx  <- match(1:nw$nc,c(nid,cparents))
                    if (FALSE) {
                        cat("theidxm=",theidxm,"\n")
                        cat("Pn:\n");print(Pn)
                        cat("mu=\n");print(mu)
                        cat("theidx=",theidx,"nidx=",nidx,"\n")
                        cat("paridx=",paridx,"\n")
                    }
                    for (k in 1:length(theidx)) {
                        if (FALSE) {
                            cat("mu[",theidx,",",nidx,"]=\n")
                            print(mu[theidx,nidx])
                            cat("Pn[",theidxm[k],",",paridx+1,"]=\n")
                            print(Pn[theidxm[k],2])
                        }
                        ##            mu[theidx,nidx] <- Pn[theidxm[k],nidx+1]
                        ##            mu[theidx,nidx] <- Pn[theidxm[k],paridx+1]            
                        mu[theidx,nidx] <- Pn[theidxm[k],2]
                        sigma2list[[theidx[k]]][nidx,nidx] <- Pn[theidxm[k],1]
                    }
                    if (FALSE) {
                        print(mu)
                        print(sigma2list)
                        cat("slutprut\n")
                    }
                }
            }
            else { ## no discrete parents
                ##        cat("no discrete parents\n")
                for (i in 1:TD) {
                    mu[i,nidx] <- Pn[2]
                    sigma2list[[i]][nidx,nidx] <- Pn[1]
                }
            } ## end else (no discrete parents)
            
            ##      for (i in 1:TD) {
            ##        cat("i=",i,"\n")
            ##        ## Pn skal altid være en matrix...
            ##        mu[i,nidx] <- Pn[i,2]
            ##        sigma2list[[i]][nidx,nidx] <- Pn[i,1]
            ##      }
            ##    }
            
            ##      mu[1,nidx] <- Pn[2]
            ##      sigma2[nidx,nidx] <- Pn[1] # så det stadig virker
            if (FALSE) {
                print(mu)
                print(sigma2list)
            }
        }
        else { # we have continuous (and possibly discrete) parents
            ##      cat("Continuous parents\n")
            ##      if (!length(dparents)>0) {
            ##        cat("Continuous and no discrete parents\n")
            
            
            for (k in 1:TD) {
                ##        cat("k=",k,"\n")
                ##      parentidx <- match(1:nw$nc,cparents)
                if (length(dparents)>0) {
                    ## should be moved to the top...
                    mdim <- c()
                    for (i in dparents) 
                        mdim <- c(mdim,nw$nodes[[i]]$levels)
                    
                    ##          line("*",10)
                    ##          cat("k=",k,"\n")
                    Mcf <- findex(k,Dim,config=FALSE)
                    ##          cat("Mcf=\n");print(Mcf)
                    didx <- match(dparents,nw$discrete)
                    ##          cat("didx=",didx,"\n")
                    dcf <- Mcf[,didx]
                    ##          cat("dcf=\n");print(dcf)

                    if (length(dcf)==2) ## dirty solution to bug 5/9-02
                        dcf <- matrix(dcf,ncol=2)
                    
                    kidx <- findex(dcf,mdim,config=TRUE)
                    ##          cat("kidx=",kidx,"\n")
                    ##          line("*",10)
                    ##kidx <- 1
                }
                else
                    kidx <- 1
                ##      parentidx <- cparents
                parentidx <- match(cparents,nw$continuous)
                ##        cat("parentidx=",parentidx,"\n")
                if (!length(dparents)>0) {        
                    m.ylx <- Pn[2]
                    s2.ylx<- Pn[1]
                    b.ylx <- Pn[3:length(Pn)]
                }
                else {
                    ##          cat("Pn=\n");print(Pn)
                    m.ylx <- Pn[kidx,2]
                    s2.ylx<- Pn[kidx,1]
                    b.ylx <- Pn[kidx,3:ncol(Pn)]
                }
                ##        cat("mu=\n")
                ##        print(mu)
                m.x   <- mu[k,parentidx]
                s2.x  <- sigma2list[[k]][parentidx,parentidx]
                ##        cat("** her skal ikke stå 1, men løkke\n")
                ##        s2.x   <- sigma2[parentidx,parentidx]
                
                s.xy  <- s2.x %*% b.ylx
                s2.y  <- s2.ylx + c(s.xy)%*%b.ylx
                
                m.y   <- m.ylx + b.ylx%*%m.x
                
                if (FALSE) {
                    cat("(jointcont)\n")
                    cat("m.ylx=",m.ylx,"\n")
                    cat("s2.ylx=",s2.ylx,"\n")
                    cat("b.ylx=",b.ylx,"\n")
                    cat("m.x=\n");print(m.x)
                    cat("s2.x=\n");print(s2.x)
                    cat("s.xy=\n");print(s.xy)
                    cat("s2.y=\n");print(s2.y)
                    cat("m.y=\n");print(m.y)
                }
                
                mu[,nidx] <- m.y
                ##        sigma2[nidx,nidx] <- s2.y
                ##        sigma2[parentidx,nidx] <- s.xy
                ##        sigma2[nidx,parentidx] <- t(s.xy)
                
                sigma2list[[k]][nidx,nidx] <- s2.y
                sigma2list[[k]][parentidx,nidx] <- s.xy
                sigma2list[[k]][nidx,parentidx] <- t(s.xy)
            }
            ##      } # continuous parents, but no discrete
            ##      else {
            ##        cat("Mixed parents\n")
            ##        stop("*** not implemented yet\n")
            ##      }
            
        }
        
        
        calclist <- c(calclist,nid)
        
        if (FALSE) {
            ##      line()
            ##      cat("(disccont:)\n")
            ##      cat("Node ", node$name,"\n")
            ##      cat("Continuous parents:", cparents,"\n")
            ##      cat("Discrete parents:", dparents,"\n")
            ##      cat("Pn:\n");print(Pn)
            cat("calclist:",calclist,"\n")
            cat("nid=",nid,"nidx=",nidx,"\n")
            ##      cat("TD=",TD,"\n")
            cat("mu=\n");print(mu)
            cat("sigma2list\n");print(sigma2list)
        }
        
    } ## while
    
    if (timetrace) {
        t2 <- proc.time()
        cat((t2-t1)[1],"]")
    }
    
    if (FALSE) {
        cat("(jointcont RESULT\n")
        cat("mu\n")
        print(mu)
        cat("sigma2list\n")
        print(sigma2list)
    }
    list(mu=mu,sigma2=sigma2list)
} ## function discjoint

