## simulation.R
## Author          : Claus Dethlefsen
## Created On      : Tue Feb 26 11:22:30 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Sun Sep 15 08:19:35 2002
## Update Count    : 415
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

simulation <- function(nw, n=24, file="") {
    ## Simulate a dataset and output to screen or 'file'.
    ## nw is a network consisting of:
    ## (slightly different from ordinary networks)
    ##   nw$n: the number of nodes
    ##   nw$ndiscrete:   the number of discrete nodes
    ##   nw$ncontinuous: the number of cont nodes
    ##   nw$nodes: A list of nodes with parents defining the DAG
    
    ##  cat("\nSimulating data\n")

    mymultinomial <- function(n,p) {
        ## n: the number of cases to simulate
        ## p: a vector of probabilies for the categories
        
        ##  stopifnot(sum(p)==1) ## too strict. Should be something with eps.
        
        mycoin <- runif(n)
        
        mycoin[mycoin==1] <- 0.99999
        
        res <- rep(NA,n)
        
        prob <- c(0,cumsum(p))
        
        for (i in 2:(length(p)+1) ) 
            res[ prob[i-1] <= mycoin & mycoin < prob[i] ] <- i-1
        
        res
    }

    
    res <- matrix(1, n, nw$n)
    res <- data.frame(res)
    colnames(res) <- names(nw$nodes)
    
    ## create factors for discrete variables
    if (length(nw$discrete)>0) {
        for (j in 1:length(nw$discrete)){
            res[,nw$discrete[j]] <- factor(res[,nw$discrete[j]],
                                           levels=nw$nodes[[nw$discrete[j]]]$levelnames)
        }
        if (FALSE) {
            cat("(simulation): res\n")
            print(res)
        }
    }
    
    ## ####################################################################
    ## simulate discrete nodes
    initsimlist <- c()
    ##  for (i in nw$discrete) {
    nid <- 0
    while ( length( setdiff(nw$discrete,initsimlist) )>0 ) {
        
        nid <- nid%%(nw$n)+1
        if ( length(intersect(nid,initsimlist))>0) next
        
        node <- nw$nodes[[nid]]
        
        if (!node$type=="discrete" ) next
        
        if ( length( setdiff(node$parents,initsimlist) ) > 0  ) next
        
        if (FALSE) cat("Simulation of node",node$name,"\n")
        
        if (!length(node$parents)>0) { ## discrete node without parents
            if (FALSE) {
                cat("simprob=\n");print(node$simprob)
            }
            res[,node$idx] <-
                node$levelnames[mymultinomial(n,node$simprob)] 
            initsimlist <- c(initsimlist,nid)
        }
        else { ## discrete node with parents
            if (FALSE) {
                cat("Parents:",node$parents,"\n")
                cat("simprob=\n");print(node$simprob)
            }
            
            ptab <- table(res[,node$parents])
            
            if (FALSE) {
                cat("Parent-table:\n");
                print(ptab)
            }
            ## dimension of c(node,parents)
            Dim <- dim(node$simprob)
            pDim <- Dim[-1] # parent dimension
            if (FALSE) {
                cat("Dim=",Dim,"\n")
                cat("pDim=",pDim,"\n")
            }
            for (j in 1:prod(pDim)) {
                if (FALSE)
                    cat("j=",j,"\n")
                cf <- findex(j,pDim,config=FALSE)
                if (FALSE) {
                    cat("cf=\n");print(cf)
                }
                idx <- 1:n
                for (k in 1:length(node$parents)) {
                    if (FALSE) {
                        cat("k=",k,"\n")
                        cat("parent=",node$parents[k],"\n")
                        cat("levelname=",
                            nw$nodes[[node$parents[k]]]$levelnames[cf[1,k]],"\n")
                    }
                    pcf <- nw$nodes[[node$parents[k]]]$levelnames[cf[1,k]]
                    
                    idx <- idx[res[idx,node$parents[k]]==pcf]
                    if (FALSE) {
                        cat(pcf,",")                      
                        cat("idx=\n");print(idx)
                    }
                }
                
                nl <- node$levels
                np <- length(node$parents)
                up <- matrix( rep( cf, rep(nl,np) ), nl, np)
                icf <- cbind(1:node$levels,up)
                
                if (FALSE) {
                    cat("\n")
                    cat("idx=\n"); print(idx)
                    cat("simprob=\n");print(node$simprob)
                    cat("cf=\n");print(cf)
                    cat("node$levels=",node$levels,"\n")
                                        #                  icf <- cbind(c(1:node$levels,cf))
                    cat("icf=");print(icf)
                    print(node$simprob[icf])
                }
                ##                  thissim <- mymultinomial(n,node$simprob[,cf])
                thissim <- mymultinomial(ptab[cf],node$simprob[icf])
                if (FALSE){
                    cat("thissim=\n");print(thissim)
                    print(node$levelnames[thissim])
                }
                ## der hvor res[,parents]==cf skal
                ##    res[,node]<-levelnames[thissim]
                res[idx,node$idx] <- node$levelnames[thissim]
            } ## for
            
            
            ## for hver tilstand af forældre
            ## sample table(res[,node$parents))[tilstand] fra multi
            ##   simprob[,config]
            ## put ind i res[tilstand,node$idx]
            initsimlist <- c(initsimlist,nid)
            if (FALSE) {
                cat("node:",node$name,"idx=",node$idx,"nid=",nid,"\n")
                cat("initsimlist\n")
                print(initsimlist)
            }
        }
        
    } ## while
    
    ## ####################################################################
    ## simulate continuous nodes
    
    allnodes <- nw$continuous
    simlist <- initsimlist      
    nid <- 0
    while ( length( setdiff(allnodes,simlist) )>0 ) {
        
        nid <- nid%%(nw$n)+1
        if ( length(intersect(nid,simlist))>0) next
        
        node <- nw$nodes[[nid]]
        parents <- node$parents
        if (nw$nd>0)      dparents<- sort(intersect(parents,nw$discrete))
        else dparents <- c()
        if (nw$nc>0)      cparents<- sort(intersect(parents,nw$continuous))
        
        if ( length( setdiff(parents,simlist) ) > 0  ) next
        
        if (FALSE) cat("Simulating",node$name,"\n")
        
        if (!length(parents)>0) {
            ## no parents
            mu <- node$simprob[2]
            s2 <- node$simprob[1]
            res[,nid] <- rnorm(n,mu,sqrt(s2))
            simlist <- c(simlist,nid)
            next
        }
        
        if (!length(dparents)>0) {
            ## no discrete parents            
            if (FALSE) {
                cat("simprob:\n");print(node$simprob)
                cat("cparents",cparents,"\n")
            }
            s2 <- node$simprob[1]
            beta <- cbind(node$simprob[2:(length(cparents)+2)])
            if (FALSE) {
                cat("Jo, det er her\n")
                cat("s2",s2,"\n")
                cat("beta",beta,"\n")
                print(beta)
##                cat(cbind(beta))
##                cat(matrix(beta,ncol=1))
            }
            if (FALSE) {
##                print(res[,cparents])
                print(cbind(1,res[,cparents]))
            }
            pres <- as.matrix(res[,cparents])
            mu <- cbind(1,pres)%*%beta
            res[,nid] <- rnorm(n,mu,sqrt(s2))
            simlist <- c(simlist,nid)
            next
        } ## if

        ## discrete and possibly cont. parents are present
        if (FALSE) {
            cat("dparents=",dparents,"\n")
            cat("cparents=",cparents,"\n")
            cat("parents=",parents,"\n")
        }
        Dim <- c()
        for (i in dparents)
            Dim <- c(Dim,nw$nodes[[i]]$levels)
        if (FALSE) {
            cat("Dim=",Dim,"\n")
        }
        
        for (j in 1:prod(Dim)) {
            if (FALSE) cat("j=",j,"\n")
            cf <- findex(j,Dim,config=FALSE)
            if (FALSE) {
                cat("cf=\n");print(cf)
            }
            
            idx <- 1:n
            for (k in 1:length(dparents)) {
                if (FALSE) {
                    cat("k=",k,"\n")
                    cat("parent=",dparents[k],"\n")
                    cat("levelname=",
                        nw$nodes[[dparents[k]]]$levelnames[cf[1,k]],"\n")
                }
                    pcf <- nw$nodes[[dparents[k]]]$levelnames[cf[1,k]]
                
                idx <- idx[res[idx,dparents[k]]==pcf]
                if (FALSE) {
                    cat(pcf,",")                      
                    cat("idx=\n");print(idx)
                }
            } ## for k
            if (length(idx)>0) {
                if (FALSE) {
                    print(res[idx,])
                    print(node$simprob)
                }
                if (!length(cparents)>0) {
                    ## no cont. parents
                    s2 <- node$simprob[j,1]
                    mu <- node$simprob[j,2]
                }
                else { ## cont. parents
                    beta <- cbind(node$simprob[j,2:(length(cparents)+2)])
                    if (FALSE) {
                        cat("beta=\n");print(beta)
                        print(res[idx,cparents])
                        cat("length(idx)=",length(idx),"\n")
                    }
##                    if (length(idx)<2)
##                        ridx <-
##                            as.matrix(res[idx,cparents])
##                    else
                        ridx <- as.matrix(res[idx,cparents])
                    if (FALSE) {cat("res[idx,cparents]=\n");print(ridx)}
                    if (FALSE) print(cbind(1,ridx))
                    mu <- cbind(1,ridx)%*%beta
                    if (FALSE) {
                        cat("beta=\n");print(beta)
                        cat("mu=\n");print(mu)
                        cat("res[idx,cparents]=\n");print(ridx)
                    }
                }
                res[idx,nid] <- rnorm(length(idx),mu,sqrt(s2))
                
                if (FALSE) {
                    cat("s2=",s2,"\n")
                    cat("mu=",mu,"\n")
                    print(res)
                }
            } ## if
        } ## for j
        simlist <- c(simlist,nid)
        next

        ## mixed parents
        break
    } ## while

    initsimlist <- simlist
    
    ## ####################################################################
    ## Last resort
    
    ##    allnodes<- c(nw$discrete,nw$continuous)
    allnodes <- nw$continuous
    if ( length( setdiff(allnodes,initsimlist) )>0 ) {

        cat("Damn, this is slow\n")
        for (obs in 1:n) {
            
            ##    simlist <- c()
            simlist <- initsimlist      
            
            nid <- 0
            while ( length( setdiff(allnodes,simlist) )>0 ) {
                
                nid <- nid%%(nw$n)+1
                if ( length(intersect(nid,simlist))>0) next
                
                node <- nw$nodes[[nid]]
                parents <- node$parents
                if (nw$nd>0)      dparents<- sort(intersect(parents,nw$discrete))
                else dparents <- c()
                if (nw$nc>0)      cparents<- sort(intersect(parents,nw$continuous))
                
                if ( length( setdiff(parents,simlist) ) > 0  ) next
                
                if (FALSE) cat("Simulating",node$name,"\n")
                ## first find out the values of the parents
                
                ## then, for cont. nodes, find mean and variance
                ##
                ## for disc. nodes. Look up in the table
                
                if (!length(parents)>0) {
                    if (node$type=="continuous") {
                        res[obs,node$idx] <-
                            rnorm(1,node$simprob[1,2],sqrt(node$simprob[1,1]))
                        ##          print(res[obs,node$idx])
                    }
                    else if (node$type=="discrete"){
                        ##          print(mymultinomial(1,node$simprob)            )
                        res[obs,node$idx] <-
                            node$levelnames[mymultinomial(1,node$simprob)] 
                        ##mymultinomial(1,node$simprob)
                    }
                }
                else {
######################################################################
                    ## at least one parent!        
                    if (node$type=="discrete") {
                        
                        Dim <- c()
                        dnames <- list(node$levelnames)
                        for (i in dparents) {
                            Dim <- c(Dim,nw$nodes[[i]]$levels)
                            dnames <- c(dnames,list(nw$nodes[[i]]$levelnames))
                        }
                        Dim <- c(node$levels,Dim)
                        
                        pval <- c()
                        for (j in parents) 
                            pval <- c(pval,res[obs,j])
                        
                        idx <- cbind(1:node$levels)
                        for (j in 1:length(pval))
                            idx <- cbind(idx,pval[j])
                        
                        if (FALSE) {
                            line()
                            cat("(simulation):\n")
                            print(pval)
                            print(node$simprob)
                            print(1:node$levels)
                            print(Dim)
                            print(idx)
                        }
                        fidx <- findex(idx,Dim,config=TRUE)
                        pvek <- node$simprob[fidx]
                        pvek <- pvek/sum(pvek)
                        names(pvek) <- node$levelnames
                        if (FALSE) {
                            print(pvek)
                            line()
                      }
                        res[obs,node$idx] <-
                            node$levelnames[mymultinomial(1,pvek)] 
                    }
                    
                    else if (node$type=="continuous") {
                        
                        if (length(dparents)>0) {
                            Dim <- c()
                            dnames <- list(node$levelnames)
                            for (i in dparents) {
                                Dim <- c(Dim,nw$nodes[[i]]$levels)
                                dnames <- c(dnames,list(nw$nodes[[i]]$levelnames))
                            }
                            
                            ## find out the configuration of disc parents
                            pval <- c()
                            for (j in dparents) 
                                pval <- c(pval,res[obs,j])
                            
                            ## translate it to a row-number in simprob
                            idx <- findex(rbind(pval),Dim,config=TRUE)
                            
                            if (FALSE) {
                                print(rbind(pval))
                                print(idx)
                            }
                        }
                        else {
                            Dim <- c()
                            idx <- 1
                        }
                        
                        ## get the values of the cont. variables
                        cval <- c()
                        for (j in cparents)
                            cval <- c(cval,res[obs,j])
                                        #          print(cval)
                        ## get the coefficients
                        s2 <- node$simprob[idx,1]
                        coef <- node$simprob[idx,2:ncol(node$simprob)]
                                        #          print(s2)
                                        #          print(coef)
                        ## find the mean and variance.
                        mn <- c(1,cval)%*%coef
                        
                                        #          print(mn)
                        
                        res[obs,node$idx] <-
                            rnorm(1,mn,sqrt(s2))
                                        #          print(res[obs,node$idx])
                                        #          line()
                    }
                    
                    else stop("Node type illegal\n")
                }
                
                simlist <- c(simlist,nid)
            } ## while
        } ## for
    } ## if 
    
    if (file!="") write.table(res,file=file,row.names=FALSE,col.names=TRUE)
    res
}


