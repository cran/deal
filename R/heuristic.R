## heuristic.R
## Author          : Claus Dethlefsen
## Created On      : Sun Jan 13 11:23:16 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Mon Aug 25 11:30:45 2003
## Update Count    : 141
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
  function(initnw,data,prior=jointprior(network(data)),
           maxiter=100,restart=10,degree=initnw$n,
           trylist= vector("list",initnw$n),trace=TRUE,
           timetrace=TRUE,saveall=FALSE,removecycles=FALSE)
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
                      trace=trace,timetrace=TRUE,saveall=saveall,
                      removecycles=removecycles)
    
    nw <- nwl$nw
    trylist <- nwl$trylist
#    nwl <- nwfunique(nwfsort(nwl$nwl))
    tabel <- nwl$tabel
    
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
                perturb(initnw,data,prior,degree,trylist=trylist,timetrace=TRUE)
            trylist <- nw$trylist

            nw <- nw$nw
            ms <- modelstreng(nw)
            if (timetrace) {
                s4 <- proc.time()[3]
                spert <- spert + s4-s3
            }
            if (!is.na(match(ms,tabel[,1]))) next
#            if (elementin(nw,nwl)) next
            tabel <- rbind(tabel,cbind(ms,nw$score))
            if (trace) {
                plot(nw)
                title("New network")
            }
            
            if (timetrace)
                s5 <- proc.time()[3]
            newnwl <- autosearch(nw,data,prior,maxiter,
                                 trylist=trylist,trace=trace,timetrace=TRUE,saveall=saveall,removecycles=removecycles)
            trylist <- newnwl$trylist
#            newnwl <- newnwl$nwl
            tabel <- rbind(tabel,newnwl$tabel)
#            nw <- newnwl$nw
            if (timetrace) {
                s6 <- proc.time()[3]
                sauto <- sauto + s6-s5
            }
#            nwl <- c(nwl,newnwl)
            if (timetrace) s7 <- proc.time()[3]
#            nwl <- nwfunique(nwfsort(nwl),timetrace=FALSE,equi=FALSE)
            tabel <- tabel[!duplicated(tabel[,1]),]
            tabel <- tabel[sort.list(tabel[,2]),]
            if (timetrace) {
                s8 <- proc.time()[3]
                suniq <- suniq + s8 - s7
            }
        } ## for i
        ##    nwl <- nwfsort(nwl)
    } ## if restart
#    class(nwl) <- "networkfamily"
    if (initnw$n<15) antal <- paste(numbermixed(initnw$nc,initnw$nd))
    else antal <- "many"
    
    cat("Tried",nrow(tabel),"out of approx.",antal,"networks\n")
#    cat("Tried",length(nwl),"out of",antal,"networks\n")
    if (timetrace) {
        t2 <- proc.time()
        cat((t2-t1)[1],"]\n")
        cat("Perturb:",spert,",Autosearch:",sauto,",Unique:",suniq,"\n")
    }


    thebest <- as.network(tabel[1,],initnw)
    thebest <- learn(thebest,data,prior)$nw
    list(nw=thebest,tabel=tabel,trylist=trylist)
    #list(nw=nwl,trylist=trylist)
}
