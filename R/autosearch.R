## autosearch.R
## Author          : Claus Dethlefsen
## Created On      : Fri Jan 11 10:54:00 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Thu Oct 24 08:39:32 2002
## Update Count    : 220
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

autosearch <- function(initnw,data,prior=jointprior(network(data)),maxiter=50,trylist= rep(list(NULL),initnw$n),trace=FALSE,timetrace=FALSE,smalldf=NA,showban=FALSE,saveall=TRUE) {
    ## Greedy search
    
    ## initnw: initial network with conditionals calculated
    ##
    ## output: networklist: a sorted list of all tried networks.

    ## used by: heuristic.
    ## uses: addarrow,removearrow,turnarrow,nwfsort,cycletest
    ##       initnw$score
    
    ## Algoritme:
    ## Dan alle netværk med een extra pil         (addarrow)
    ## Dan alle netværk med en pil, der vender om (turnarrow)
    ## Dan alle net med een pil mindre            (removearrow)
    ## Beregn likelihood for nye netværk
    ## Vælg den, der øger likelihooden mest muligt eller stop.
    
    if (timetrace) {t1 <- proc.time();cat("[Autosearch ")
                    tadd <- 0
                    trem <- 0
                    ttur <- 0
                    tsor <- 0
                    tcho <- 0
                }
    
    
    nwl <- list(initnw)
    nw <- initnw
    slut <- FALSE
    it   <- 0
    hiscore <- initnw$score

    while (!slut & it < maxiter) {
        it <- it + 1
        
        if (timetrace) {s1 <- proc.time()[1]}
        ## cat("adding arrows\n")
        thisnwl.add <- addarrow(nw,data,prior,trylist=trylist,smalldf=smalldf)
        trylist     <- thisnwl.add$trylist
        thisnwl.add <- thisnwl.add$nw
        if (timetrace) {s2 <- proc.time()[1];
                        tadd <- tadd+s2-s1
                    }
        ## cat("removing arrows\n")
        thisnwl.rem <- removearrow(nw,data,prior,trylist=trylist,smalldf=smalldf)
        trylist <- thisnwl.rem$trylist
        thisnwl.rem <- thisnwl.rem$nw
        if (timetrace) {s3 <- proc.time()[1];
                        trem <- trem+s3-s2
                    }
        ## cat("turning arrows\n")
        thisnwl.tur <- turnarrow(nw,data,prior,trylist=trylist,smalldf=smalldf)
        trylist <- thisnwl.tur$trylist
        thisnwl.tur <- thisnwl.tur$nw
        if (timetrace) {s4 <- proc.time()[1];
                        ttur <- ttur+s4-s3
                    }
        thisnwl <- c(thisnwl.add,thisnwl.rem,thisnwl.tur)
        class(thisnwl) <- "networkfamily"

        thisnwl <- nwfsort(thisnwl)
        if (timetrace) {s5 <- proc.time()[1];
                        tsor <- tsor+s5-s4
                    }
        
        
        ## remove cycles and then choose the best
        if (saveall)
        {
            thisnwl <- thisnwl[!unlist(lapply(thisnwl,cycletest))]
            nw <- thisnwl[[1]]
        ## what if all of them contains cycles? They do not.
        }
        else
        {
            ## new strategy: choose the 'best' and then check for cycle.
            kk <- 1
            while (TRUE) {
                nw <- thisnwl[[kk]]
                kk <- kk + 1
                if (!cycletest(nw)) break
                if (timetrace) cat(".")
            }
        }
        if (timetrace) {s6 <- proc.time()[1];
                        tcho <- tcho+s6-s5
                    }
        
        if (saveall) nwl <- c(nwl,thisnwl)
        else nwl <- list(thisnwl[[1]])
        
        if (nw$score > hiscore) {
            hiscore <- nw$score
            if (trace) {plot(nw,showban=showban)
                        print(nw)
                    }
        }
        else
        {
            slut <- TRUE
        }
        
        if (FALSE) {
            cat("Conditions:\n")
            c1 <- !slut
            c2 <- it<maxiter
            cat("!slut=",c1,"\n")
            cat("it<maxiter=",c2,"\n")
            cat("c1 & c2 = ", c1 & c2,"\n")
            cat("c1 | c2 = ", c1 | c2,"\n")
        }
    } ## end while
    
    ##  nwl <- nwfsort(nwl) # nødvendig?
    class(nwl) <- "networkfamily"
    
    if (timetrace) {
        t2 <- proc.time()
        total <- (t2-t1)[1]
        cat("Total",total,"add",tadd,"rem",trem,"turn",ttur,"sort",tsor,"choose",tcho,"rest",total-tadd-trem-ttur-tsor-tcho,"]\n")
        
    }
    
    list(nwl=nwl,trylist=trylist)
}


 addarrow <- function(nw,df,prior,trylist=rep(list(NULL),nw$n),smalldf=NA) {
  ## Create all networks with one extra arrow
  ## return list of networks (nwl) (Possibly NULL)
  ## trylist: a list of networks wherefrom some learning may be reused

     ## used by: autosearch
     ## uses: insert
     ## and network attributes: n
     
  nwl <- list()
  n <- nw$n
  try <- cbind(1:n,rep(1:n,rep(n,n)))
  
  for (i in 1:nrow(try)) {
#    for (j in 1:nw$n) {
    newnet <- insert(nw,try[i,1],try[i,2],df,prior,trylist=trylist,smalldf=smalldf)

    if ( !is.null(newnet$nw) ) { # prevent NULL networks
        # cat("Add",nw$nodes[[i]]$name,"->",nw$nodes[[j]]$name,"\n")
      nwl[length(nwl)+1] <- list(newnet$nw)
      trylist <- newnet$trylist
    }

  }
  class(nwl) <- "networkfamily"
  list(nw=nwl,trylist=trylist)
}



removearrow <- function(nw,df,prior,trylist=rep(list(NULL),nw$n),smalldf=NA) {
  ## create all networks with one arrow less
  ## return list of networks (possibly NULL)
  ## trylist: a list of networks wherefrom some learning may be reused
  

    ## used by: autosearch
    ## uses: insert, learn
    ## and network attributes: n, nodes$parents
  nwl <- list()
  for (i in 1:nw$n) {
    if (length(nw$nodes[[i]]$parents) > 0) {
      for (j in 1:length(nw$nodes[[i]]$parents)) {
#        cat("Remove:",nw$nodes[[nw$nodes[[i]]$parents[j]]]$name,"/->",nw$nodes[[i]]$name,"\n")
        newnet <- nw
        newnet$nodes[[i]]$parents <- newnet$nodes[[i]]$parents[-j]
        newnet <- learn(newnet,df,prior,i,trylist=trylist,smalldf=smalldf)
        trylist <- newnet$trylist
        newnet <- newnet$nw
        nwl[length(nwl)+1] <- list(newnet)
      }
    }
  }
  class(nwl) <- "networkfamily"
  list(nw=nwl,trylist=trylist)
}

turnarrow <- function(nw,df,prior,trylist=rep(list(NULL),nw$n),smalldf=NA) {
  ## create all networks with one arrow turned
  ## return list of networks (possibly NULL)
  ## trylist: a list of networks wherefrom some learning may be reused

    ## used by: autosearch
    ## uses: insert, learn
    ## and network attributes: n, nodes$parents

  nwl <- list()
  for (i in 1:nw$n) {
    if (length(nw$nodes[[i]]$parents) > 0) {
      for (j in 1:length(nw$nodes[[i]]$parents)) {
        newnet <- nw
        parent <- nw$nodes[[i]]$parents[j]
        newnet$nodes[[i]]$parents <- newnet$nodes[[i]]$parents[-j]
        newnet <- learn(newnet,df,prior,i,trylist=trylist,smalldf=smalldf)
        trylist<- newnet$trylist
        newnet <- newnet$nw
        newnet <- insert(newnet,i,parent,df,prior,trylist=trylist,smalldf=smalldf) #parent is learned here
        trylist <- newnet$trylist
        newnet  <- newnet$nw
        if (length(newnet) > 0) { # prevent NULL networks
#          cat("Turn:",nw$nodes[[i]]$name,"<-",nw$nodes[[nw$nodes[[i]]$parents[j]]]$name,"\n")
          nwl[length(nwl)+1] <- list(newnet) 
        }
      }
    }
  }
  class(nwl) <- "networkfamily"
  list(nw=nwl,trylist=trylist)
}
