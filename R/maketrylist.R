## maketrylist.R
## Author          : Claus Dethlefsen
## Created On      : Fri Jan 11 10:54:00 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Sun Sep 15 08:14:13 2002
## Update Count    : 193
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


maketrylist <-
  function(initnw,data,prior=jointprior(network(data)),timetrace=FALSE) {
    
    if (timetrace) {t1 <- proc.time();cat("[Maketrylist ")}

    tryl <- list()
    for (i in 1:initnw$n) {
      nwl <- list(initnw)
      for (j in setdiff(1:initnw$n,i)) {
#        for (k in 1:length(nwl)) {
        newnet <- lapply(nwl,insert,j,i,data,prior)
        newnet <- lapply(newnet,function(x) x$nw)
#        line()
#        print(newnet)
#        line()
#        cat("NOT NULL\n")
#        print(newnet[newnet!=NULL])
#          newnet <- insert(nwl[[k]],j,i,data,prior)
#        for (k in 1:length(newnet)) {
#          if (length(newnet[[k]])>0) {
#            nwl[length(nwl)+1] <- list(newnet[[k]])
#            nodelist[length(nodelist)+1] <- list(newnet[[k]]$nodes[[i]])
#          }

        nwl <- c(nwl,newnet[!unlist(lapply(newnet,is.null))])
        
#      class(nwl) <- "networkfamily"
#      line()
#      cat("nwl=\n")
 #     print(nwl)

      ##    tryl[[i]] <- nwl
#      nodelist <- list()
#      for (k in 1:length(nwl))
#        nodelist[length(nodelist)+1] <- list(nwl[[k]]$nodes[[i]])
      nodelist <- lapply(nwl,function(x) x$nodes[[i]])
      ## 

      
      tryl[[i]] <- nodelist
    }
  }
      
      
      
    if (timetrace) {
      t2 <- proc.time()
      cat((t2-t1)[1],"]\n")
    }
    tryl
  }


