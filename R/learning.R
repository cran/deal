## learning.R
## Author          : Claus Dethlefsen
## Created On      : Mon Jan 14 12:24:13 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Mon Nov 04 10:57:50 2002
## Update Count    : 495
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

learn <- function(nw, df, prior=jointprior(nw),
                          nodelist=1:nw$n,trylist=
                          rep(list(NULL),nw$n),
                          timetrace=FALSE,smalldf=NA,
                          usetrylist = nw$n<=6
                          ) {
    ## nw: network to be learned (condprior must be present in the nodes)
    ## df: dataframe with observations
    ## nodelist: vector of node-indices of nodes to be learned (default
    ##                          is to learn all nodes) 
    ## trylist: a list of networks wherefrom some learning may be reused
    ##
    ## Returns a network with the following attributes
    ##       score: calculated (or updated) network-score
    ##       for each node in nodelist:
    ##           loglik: the log-likelihood contribution of the node
    ##           cond:   updated posterior parameters
    ##
    ## Uses: reuselearn, cond, learnnode
    ## and network attributes: nodes, score is updated
    ## and node attributes: tvar,condprior,condposterior is updated
    ##
    ## Used by: insert,remover,removearrow,turnarrow,
    ##          manualsearch,networkfamily,
    ##          turnrandomarrow,deleterandomarrow (perturb)
    
    
    if (timetrace) {t1 <- proc.time();cat("[Learn.network ")}
    
    old <- df 
    if (FALSE) {
        cat("Learn.network\n")
        cat("nrow(data)=",nrow(df),"\n")
    }
    
    for (i in nodelist) {
        ##   cat("node,",nw$nodes[[i]]$name,"\n")
        node <- nw$nodes[[i]]
        ##    cat("parents:",node$parents,"\n")
        
        if (FALSE) {
            cat("nu\n")
            print(is.null(node$tvar))
            print(smalldf)
            print(is.na(smalldf))
            print(is.null(node$tvar)&!is.na(smalldf))
        }
        
        if (is.null(node$tvar)&!is.na(smalldf))
            df <- smalldf
        else
            df <- old
        ##   cat("node:",node$name,"nrow(df)=",nrow(df),"\n")
        if (usetrylist) {
            reuse <- reuselearn(node,trylist)
            if (reuse != 0) {
                ##      cat("reusing,",reuse,"\n")
                nw$nodes[[i]] <- trylist[[i]][[reuse]]
                break
            }
        }
        
        node <- cond.node(node,nw,prior) ## master prior procedure
        
        node$condposterior <- node$condprior ## reset posterior
        node$loglik        <- 0
        node <- learnnode(node,nw,df,timetrace=FALSE)## learn!
        
        ## update trylist
        if (usetrylist)
            trylist[[i]][length(trylist[[i]])+1] <- list(node)
        
        ## update network
        nw$nodes[[i]] <- node
    }
    
    ## calculate network score
    nw$score <- 0
    for (i in 1:nw$n) nw$score <- nw$score + nw$nodes[[i]]$loglik
    
    if (timetrace) {
        t2 <- proc.time()
        cat((t2-t1)[1],"]")
    }
    list(nw=nw,trylist=trylist)
}

learnnode <- function(node,nw,df,prior=jointprior(nw),timetrace=FALSE) {
    ## node: node to be learned. condprior must be present
    ## nw:   network
    ## df:   dataframe to learn from
    ##
    ## Returns: node with extra (or updated) attributes:
    ##          loglik: the loglikelihood contribution from this node
    ##          cond:   the posterior parameters
    ##
    ## Uses: udisclik,postc0c,postcc
    ## And network attributes: nc,nd,continuous,discrete,nodes
    ## And node attributes: type,condprior,condposterior
    ## (updated),loglik (updated),parents,levels,idx
    ##
    ## Used by: learn.network
    
    if (timetrace) {t1 <- proc.time();cat("[Learn.node ")}
    
    if (FALSE) {
        cat("Learn.node\n")
    }
    
    if (node$type=="discrete") {
        if (FALSE) {
            print(node)
            print(nrow(df))
            cat("Using smart update\n")
            cat("node,parents:",c(node$idx,node$parents),"\n")
            cat("table:\n");print(table(df[,sort(c(node$idx,node$parents))]))
            cat("alpha=\n");print(node$condposterior[[1]]$alpha)
            cat("alpha+table=\n");
            print(node$condprior[[1]]$alpha+as.array(table(df[,sort(c(node$idx,node$parents))])))
        }
        
        node$condposterior[[1]]$alpha <- node$condprior[[1]]$alpha+
            as.array(table(df[,sort(c(node$idx,node$parents))]))
        node$loglik <- udisclik(node,nw,df) ## batch update likelihood term
        node <- postdist.node(node,nw)
        if (timetrace) {
            t2 <- proc.time()
            cat((t2-t1)[1],"]")
        }
        return(node)
    }
    
    
    ## continuous nodes:
    
    ## 0 parents
    if (!length(node$parents)>0) {
        ## cat("No parents -- fast learning\n")
        res <- postc0c(node$condposterior[[1]]$mu,
                       node$condposterior[[1]]$tau,
                       node$condposterior[[1]]$rho,
                       node$condposterior[[1]]$phi,
                       df[,node$idx])
        ##        line()
        ##        print(res)
        ##        res <- post0(node$condposterior[[1]]$mu,
        ##                       node$condposterior[[1]]$tau,
        ##                       node$condposterior[[1]]$rho,
        ##                       node$condposterior[[1]]$phi,
        ##                       df[,node$idx])
        ##        print(res)
        ##        cat("diff=",res$loglik- res2$loglik,"\n")
        node$condposterior[[1]]$mu <- res$mu
        node$condposterior[[1]]$tau <- res$tau
        node$condposterior[[1]]$rho <- res$rho
        node$condposterior[[1]]$phi <- res$phi
        node$loglik <- res$loglik
        node <- postdist.node(node,nw)
        return(node)
    }
    parents <- node$parents     ## the parents, 
    if (nw$nc>0)    cparents<- sort(intersect(parents,nw$continuous))
    else cparents <- c()
    if (nw$nd>0)    dparents<- sort(intersect(parents,nw$discrete))
    else dparents <- c()
    
    if (length(dparents)>0& (!length(cparents)>0)) {
        ##        cat("Discrete parents, no Cont. parents\n")
        ##        cat("dparents=",dparents,"\n")
        
        mscore <- 0
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
            
            
            idx <- 1:nrow(df)
            for (k in 1:length(dparents)) {
                if (FALSE) {
                    cat("k=",k,"\n")
                    cat("parent=",dparents[k],"\n")
                    cat("levelname=",
                        nw$nodes[[dparents[k]]]$levelnames[cf[1,k]],"\n")
                }
                pcf <- nw$nodes[[dparents[k]]]$levelnames[cf[1,k]]
                
                idx <- idx[df[idx,dparents[k]]==pcf]
                if (FALSE) {
                    cat(pcf,",")                      
                    cat("idx=\n");print(idx)
                }
            } ## for k
            if (length(idx)>0) {
                if (FALSE) {
                    cat("j=",j,"\n")
                    print(df[idx,node$idx])
                    print(node)
                }
                mu  <- node$condposterior[[j]]$mu
                tau <- node$condposterior[[j]]$tau
                rho <- node$condposterior[[j]]$rho
                phi <- node$condposterior[[j]]$phi
                y   <- df[idx,node$idx]
                if (FALSE) {
                    cat("mu=\n");print(mu)
                    cat("tau=\n");print(tau)
                    cat("rho=\n");print(rho)
                    cat("phi=\n");print(phi)
                    cat("y=\n");print(y)
                }
                
                res <- postc0c(mu, tau, rho, phi, y)
#                res <- post0(mu, tau, rho, phi, y)
#                cat("diff0=",res$loglik-res2$loglik,"\n")
                node$condposterior[[j]]$mu <- res$mu
                node$condposterior[[j]]$tau <- res$tau
                node$condposterior[[j]]$rho <- res$rho
                node$condposterior[[j]]$phi <- res$phi
                mscore  <- mscore + res$loglik
            }
            
            
        } ## for j
        ##        print(node)
        node$loglik <- mscore
        node <- postdist.node(node,nw)
        return(node)
    }
    
    if (!length(dparents)>0&length(cparents)>0) {
        ##        cat("Continuous parents\n")
#        cat("node:",node$name,"\n")
#        print(cbind(1,df[,cparents]))
        res <- postcc(node$condposterior[[1]]$mu,
                      node$condposterior[[1]]$tau,
                      node$condposterior[[1]]$rho,
                      node$condposterior[[1]]$phi,
                      df[,node$idx],
                      cbind(1,df[,cparents]))
#        res <- post(node$condposterior[[1]]$mu,
#                      node$condposterior[[1]]$tau,
#                      node$condposterior[[1]]$rho,
#                      node$condposterior[[1]]$phi,
#                      df[,node$idx],
#                      cbind(1,df[,cparents]))
#        line()
#        print(res)
#        print(res2)
#        cat("diff:",res$loglik-res2$loglik,"\n")
        
        node$condposterior[[1]]$mu <- res$mu
        node$condposterior[[1]]$tau <- res$tau
        node$condposterior[[1]]$rho <- res$rho
        node$condposterior[[1]]$phi <- res$phi
        node$loglik <- res$loglik
        node <- postdist.node(node,nw)
        return(node)
    }
    
    if (length(dparents)>0&length(cparents)>0) {
                                        #    cat("Mixed parents\n")
                                        #    cat("dparents=",dparents,"\n")
                                        #    cat("cparents=",cparents,"\n")
                                        #    print(node$name)
        mscore <- 0
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
            
            
            idx <- 1:nrow(df)
            for (k in 1:length(dparents)) {
                if (FALSE) {
                    cat("k=",k,"\n")
                    cat("parent=",dparents[k],"\n")
                    cat("levelname=",
                        nw$nodes[[dparents[k]]]$levelnames[cf[1,k]],"\n")
                }
                pcf <- nw$nodes[[dparents[k]]]$levelnames[cf[1,k]]
                
                idx <- idx[df[idx,dparents[k]]==pcf]
                if (FALSE) {
                    cat(pcf,",")                      
                    cat("idx=\n");print(idx)
                }
            } ## for k
            if (length(idx)>0) {
                if (FALSE) {
                    cat("j=",j,"\n")
                    print(df[idx,node$idx])
                    print(node)
                }
                mu <- node$condposterior[[j]]$mu
                tau <- node$condposterior[[j]]$tau
                rho <- node$condposterior[[j]]$rho
                phi <- node$condposterior[[j]]$phi
                y   <- df[idx,node$idx]
                z   <- cbind(1,df[idx,cparents])
                if (FALSE) {
                    cat("mu=\n");print(mu)
                    cat("tau=\n");print(tau)
                    cat("rho=\n");print(rho)
                    cat("phi=\n");print(phi)
                    cat("y=\n");print(y)
                }
                
#                res <- post(mu, tau, rho, phi, y, z)
#                cat("her\n")
               res <- postcc(mu, tau, rho, phi, y, z)
#                cat("diff:",res$loglik-res2$loglik,"\n")
                node$condposterior[[j]]$mu <- res$mu
                node$condposterior[[j]]$tau <- res$tau
                node$condposterior[[j]]$rho <- res$rho
                node$condposterior[[j]]$phi <- res$phi
                mscore  <- mscore + res$loglik
            }
            
            
        } ## for j
        ##        print(node)
        node$loglik <- mscore
        node <- postdist.node(node,nw)
        return(node)
        
    }
    
    
}


udisclik <- function(node,nw,df) {
    ## update likelihood term for the discrete nodes

    alpha  <- node$condposterior[[1]]$alpha
    cprior <- node$condprior[[1]]$alpha
    n <- sum(cprior) # img.db size
    N <- sum(alpha)  # n+#obs
    nobs <- N-n
    
    if (FALSE) {
        line()
        cat("Node:",node$name,"\n")
        cat("udisclik()\n")
        cat("alpha:\n");print(alpha)
        cat("lgamma(alpha):\n");print(lgamma(alpha))
        cat("cprior:\n");print(cprior)
        cat("lgamma(cprior):\n");print(lgamma(cprior))
        cat("n=",n,";N=",N,"nobs=",nobs,"\n")
    }
        
    if (length(node$parents)>0) {
        ## we have parents!
        ##      cat("udisclik: Does not work\n")
        ##      for (j in node$parents) {
        ##          if (FALSE)
        ##              cat("parent:",j,"\n")
        ## alphaj <-  nw$nodes[[j]]$condposterior[[1]]$alpha
        ## condj  <-  nw$nodes[[j]]$condprior[[1]]$alpha
        
        ## (dårlige navne)
        if (FALSE) {
            cat("Parents:",node$parents,"\n")
            cat("alpha=\n");print(alpha)
            cat("cprior=\n");print(cprior)
        }
        idx <- sort(c(node$idx,node$parents))
        cidx <- 1:length(idx)
        pidx <- cidx[-match(node$idx,idx)]
        if (FALSE) {
            cat("pidx=",pidx,"\n")
            cat("idx=",idx,"\n")
            cat("cidx=",cidx,"\n")
        }
        ## alpha_{+d|i_pa(d)}
        ##      alphaj <- table(cprior,pidx)
        alphaj <- apply(cprior,pidx,sum)
        ## alpha_{+d|i_pa(d)}+n_{+d|i_pa(d)}
        condj <- alphaj + as.array(table(df[,node$parents]))
        
        if (FALSE) {
            cat("alphaj=\n");print(alphaj)
            cat("lgamma(alphaj)\n");print(lgamma(alphaj))
            cat("condj=\n");print(condj)
            cat("lgamma(condj)\n");print(lgamma(condj))
        }
        
        ##        tres <- prod(gamma(condj)/gamma(alphaj))
        logtres <- -sum( lgamma(condj) - lgamma(alphaj) )
        if (FALSE) cat("logtres=",logtres,"\n")
        ##    } 
        
        ##      res[[i]] <- tres * prod(gamma(alpha)/gamma(cprior))
        if (FALSE) {
            line()
            cat("alpha=\n");print(alpha)
            cat("cprior=\n");print(cprior)
            cat("sum( lgamma(alpha) - lgamma(cprior)) \n")
            print(sum( lgamma(alpha) - lgamma(cprior) ))
        }
        res   <- logtres + sum( lgamma(alpha) - lgamma(cprior) )
        ## cat("res=",res,"\n")
        
    }## if parents
    else { ## no parents
        ##      res[[i]] <- prod(gamma(alpha)/gamma(cprior))*gamma(n)/gamma(N)
        res <- sum( lgamma(alpha) - lgamma(cprior)) + lgamma(n)-lgamma(N)
        if (FALSE) {
            cat("lgamma(alpha)=",lgamma(alpha),"\n")
            cat("lgamma(cprior)=",lgamma(cprior),"\n")
            cat("lgamma(n)=",lgamma(n),"\n")
            cat("lgamma(N)=",lgamma(N),"\n")
        }
        ##     cat("log(res[[i]])=",log(res[[i]]),"logres=",logres,"\n")
    }
    ##    cat("res[[i]]=",res[[i]],"log(res[[i]])=",log(res[[i]]),"\n")
    ##    res[[i]] <- log(res[[i]])
    if (FALSE) cat("res=",res,"\n")
    res
}


reuselearn <- function(node, trylist) {
  ## find out if we can reuse the learning in trylist
  i    <- node$idx
  if (is.null(trylist[[i]])) return(0)
  
  ipar <- node$parents
  match <- 0
  trylist <- trylist[[i]]
  
#  cat("length(trylist)=",length(trylist),"\n")
  if (length(trylist)>0) { ## what's a proper test?
#      cat("trying to reuse node:",i,"\n")
#      cat("parents:",ipar,"\n")
      for (j in 1:length(trylist)){
	jpar <- trylist[[j]]$parents
#        cat("parentsj:",jpar,"\n")


#        cat("jpar=",length(jpar),"ipar=",length(ipar),"\n")

        if ( length(ipar) !=
            length(jpar) ) {
#          cat("not same number of parents:")
#          cat(length(ipar), "!=", length(jpar),"\n")
          next
        }

        
        if (length(ipar)==0) {
#          cat("no parents!\n")
          match <- j
          break
        }

	if (!all(sort(ipar)==sort(jpar))) {
#          cat("no match\n")
        }
        else {
#          cat("match!\n")
          match <- j
          break
        }
        
      }
    }
    ## ingen match: 0
    ## match trylistnr
#  cat("match=",match,"\n")
  match
  }
