## perturb.ssc --- 
## Author          : Claus Dethlefsen
## Created On      : Sun Jan 13 10:16:01 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Sun Sep 15 08:17:49 2002
## Update Count    : 91
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

perturb <- function(nw,data,prior,degree=nw$n,trylist=rep(list(NULL),nw$n),nocalc=FALSE,timetrace=TRUE,smalldf=NA) {
  ## change nw by randomly adding, deleting or turning arrows.
  ## In 'degree' steps, one of the three actions is taken. Note that
  ## adding, deleting or turning may not be possible due to an empty
  ## graph or a complete graph, so that the returned network is
  ## identical to the input network. (is this wanted?) If nw is
  ## {empty,complete},
  ## the returned network is slightly likely to be {empty,complete}
  ## nocalc=T: do not learn network (data+prior is not used)
  if (timetrace) {t1 <- proc.time();cat("[Perturb ")}

  for (i in 1:degree) {
    choice <- runif(1)
    if (choice <= 1/3)       nw <- addrandomarrow(nw,data,prior,trylist,nocalc,timetrace=FALSE,smalldf=smalldf)
    else if (choice <= 2/3)  nw <- turnrandomarrow(nw,data,prior,trylist,nocalc,timetrace=FALSE,smalldf=smalldf)
    else if (choice <= 1)    nw <- deleterandomarrow(nw,data,prior,trylist,nocalc,timetrace=FALSE,smalldf=smalldf)
    trylist <- nw$trylist
    nw <- nw$nw
  }
  ## sort the parents of each node
  for (i in 1:nw$n) {
    if (length(nw$nodes[[i]]$parents)>0)
      nw$nodes[[i]]$parents <- sort(nw$nodes[[i]]$parents)
  }
  
  if (timetrace) {
    t2 <- proc.time()
    cat((t2-t1)[1],"]\n")
  }
  list(nw=nw,trylist=trylist)
}

addrandomarrow <- function(nw,data,prior,trylist=rep(list(NULL),nw$n),nocalc=FALSE,timetrace=FALSE,smalldf=NA) {
  ## add an arrow at random. Continue until one arrow is added or the
  ## graph is complete.
  ## cat("adding arrow\n")
  if (timetrace) {t1 <- proc.time();cat("[addrandomarrow ")}

  n <- nw$n

  ## all possible combinations
  possible <- findex(1:(n^2), c(n,n),config=FALSE) 

  ## delete arrows from a node to itself
  possible <- possible[diff(t(possible))!=0,]
  m <- nrow(possible)

  ## perturb
  order <- sample(1:m,m)

  for (r in order) {
    from <- possible[r,1]
    to <- possible[r,2]
#    cat("from=",from,"to=",to,"\n")
    newnet <- insert(nw, from,to,data,prior,trylist=trylist,nocalc=nocalc,smalldf=smalldf)
    trylist <- newnet$trylist
    newnet <- newnet$nw
    if (length(newnet)>0) {
      if (!cycletest(newnet)) {
        if (timetrace) {
          t2 <- proc.time()
          cat((t2-t1)[1],"] ")
        }
        return(list(nw=newnet,trylist=trylist))
      }
      else
        {;#cat("Oh, no - you created a cycle. Try again\n")
       }
    }
  }
##  cat("not possible to add arrow\n")
  if (timetrace) {
    t2 <- proc.time()
    cat((t2-t1)[1],"] ")
  }
  list(nw=nw,trylist=trylist)
}

turnrandomarrow <- function(nw,data,prior,trylist=rep(list(NULL),nw$n),nocalc=FALSE,timetrace=FALSE,smalldf=NA) {
  ## continue until an arrow is turned or it is not possible
  ## cat("turning arrow\n")

  if (timetrace) {t1 <- proc.time();cat("[turnrandomarrow ")}

  ## make a list of arrows
  parentlist <- c()
  for (i in 1:nw$n) {
    theseparents <- nw$nodes[[i]]$parents
    if (length(theseparents)>0)
      parentlist <- rbind(parentlist, cbind(i,theseparents))
  }

  if (length(parentlist)==0) {
    if (timetrace) {
      t2 <- proc.time()
      cat((t2-t1)[1],"]\n")
    }
    return(list(nw=nw,trylist=trylist))
  }

  ## try to turn them one by one until it succeeds.
  m <- nrow(parentlist)
  order <- sample(1:m,m)
  for (r in order) {
    to <- parentlist[r,1]
    from   <- parentlist[r,2]
#cat("from",from,"to",to,"\n")
    newnet <- nw
    newnet$nodes[[to]]$parents <-  setdiff(newnet$nodes[[to]]$parents,from)
#    print(newnet)
    if (!nocalc) {
      newnet <- learn(newnet,data,prior,to,trylist=trylist,smalldf=smalldf)
      trylist <- newnet$trylist
      newnet <- newnet$nw
    }
    newnet <- insert(newnet, to, from,data,prior,trylist=trylist,nocalc=nocalc,smalldf=smalldf)
    trylist <- newnet$trylist
    newnet <- newnet$nw
    ##    print(newnet)

    if (length(newnet)>0)
      if (!cycletest(newnet)) {
        if (timetrace) {
          t2 <- proc.time()
          cat((t2-t1)[1],"] ")
        }
        return(list(nw=newnet,trylist=trylist))
      }
      else
       {;# cat("Oh, no - you created a cycle. Try again\n")
      }
  }
  
  ##   cat("not possible to turn any arrows\n") 
  if (timetrace) {
    t2 <- proc.time()
    cat((t2-t1)[1],"] ")
  }
  list(nw=nw,trylist=trylist)
}

deleterandomarrow <- function(nw,data,prior,trylist=rep(list(NULL),nw$n),nocalc=FALSE,timetrace=timetrace,smalldf=NA) {
  ## delete an arrow at random. Return nw, if the graph is empty.
  ##  cat("deleting arrow\n")

  if (timetrace) {t1 <- proc.time();cat("[deleterandomarrow ")}

  parentlist <- c()
  for (i in 1:nw$n) {
    theseparents <- nw$nodes[[i]]$parents
    if (length(theseparents)>0)
      parentlist <- rbind(parentlist, cbind(i,theseparents))
  }

  if (length(parentlist)==0) {
    if (timetrace) {
      t2 <- proc.time()
      cat((t2-t1)[1],"] ")
    }
    return(list(nw=nw,trylist=trylist))
  }
  
  ## choose a parent at random
  todie <- sample(1:nrow(parentlist),1)

  ## and delete it
  i <- parentlist[todie,1]
  p <- parentlist[todie,2]
  nw$nodes[[i]]$parents <- setdiff(nw$nodes[[i]]$parents,p)
  if (!nocalc) {
    nw <- learn(nw,data,prior,i,trylist=trylist,smalldf=smalldf)
    trylist <- nw$trylist
    nw <- nw$nw
  }
  
  if (timetrace) {
    t2 <- proc.time()
    cat((t2-t1)[1],"] ")
  }
  list(nw=nw,trylist=trylist)
}

## test:

##par(mfrow=c(3,3))
##palle <- newrat
##plot(palle)
##for (i in 1:8) {
##  palle <- perturb(palle,1)
##  plot(palle)
##}
