## heuristic.R
## Author          : Claus Dethlefsen
## Created On      : Sun Jan 13 11:23:16 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Mon Sep 16 18:44:16 2002
## Update Count    : 104
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

heuristic <-
  function(initnw,data,prior=jointprior(network(data)),maxiter=100,restart=10,degree=initnw$n,trylist= rep(list(NULL),initnw$n),trace=FALSE,timetrace=TRUE,smalldf=NA)
{
  ## Heuristic search with random restart
  ## initnw: an initial network (already learned)
  ## data:   dataframe
  ## prior:  your favorite prior (has a default)
  ## maxiter:Max search steps in the search algorithm
  ## restart:The number of times to perturb initnw and rerun the search
  ## degree: Degree of perturbation
  ## trace=F: Do not plot
  
  ## outputs: The network with highest likelihood,
  ##          A list of start and end networks in the restart
  ##          A list of all networks tried 

  if (timetrace) {t1 <- proc.time();cat("[Heuristic ")}

  if (timetrace) s1 <- proc.time()[3]
  nwl <- autosearch(initnw,
                    data,prior,
                    maxiter,
                    trylist,
                    trace=trace,timetrace=TRUE,smalldf=smalldf)
  trylist <- nwl$trylist
  nwl <- nwfunique(nwfsort(nwl$nwl))

  if (timetrace) {
    s2 <- proc.time()[3]
    sauto <- s2-s1
    spert <- 0
    suniq <- 0
  }
  if (restart>0) {
    for (i in 1:restart) {
      if (timetrace) s3 <- proc.time()[3]
      nw <-
        perturb(initnw,data,prior,degree,trylist=trylist,timetrace=FALSE,smalldf=smalldf)
      trylist <- nw$trylist
      nw <- nw$nw
      if (timetrace) {
        s4 <- proc.time()[3]
        spert <- spert + s4-s3
      }
      if (elementin(nw,nwl)) next
      if (trace) {
        plot(nw)
        title("New network")
      }
      
      if (timetrace)
        s5 <- proc.time()[3]
      newnwl <- autosearch(nw,data,prior,maxiter,
                           trylist=trylist,trace=trace,timetrace=TRUE,smalldf=smalldf)
      trylist <- newnwl$trylist
      newnwl <- newnwl$nwl
      if (timetrace) {
      s6 <- proc.time()[3]
      sauto <- sauto + s6-s5
    }
      nwl <- c(nwl,newnwl)
     if (timetrace) s7 <- proc.time()[3]
      nwl <- nwfunique(nwfsort(nwl),timetrace=FALSE,equi=FALSE)
      if (timetrace) {
        s8 <- proc.time()[3]
        suniq <- suniq + s8 - s7
      }
  } ## for i
#    nwl <- nwfsort(nwl)
} ## if restart
  class(nwl) <- "networkfamily"
  if (initnw$n<15) antal <- paste(numbermixed(initnw$nc,initnw$nd))
  else antal <- "many"

  cat("Tried",length(nwl),"out of",antal,"networks\n")
  if (timetrace) {
    t2 <- proc.time()
    cat((t2-t1)[1],"]\n")
    cat("Perturb:",spert,",Autosearch:",sauto,",Unique:",suniq,"\n")
  }
  
  list(nw=nwl,trylist=trylist)
}
