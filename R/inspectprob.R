## inspectprob.R
## Author          : Claus Dethlefsen
## Created On      : Sun Feb 03 15:02:14 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Sun Sep 15 08:08:53 2002
## Update Count    : 17
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

inspectprob <-  function(nw,scale=10,unitscale=1.3,cexscale=10,rotate=pi/4,length=.25,...) {

  ## arguments are the same as for plot.network.
  
  par(mfrow=c(1,1))  
  plot(nw,scale,unitscale,cexscale,rotate,length,...)
  title("Inspect/Change initial probability distribution")

  points(0,0,cex=cexscale+4,pch=5)
  text(0,0,"Click here to stop")

  mode <- "Inspect"
  
  newnet <- nw
  quit   <- FALSE
  unit   <- 2*pi/nw$n
  where  <- 0.7*scale* cbind(
                             cos(unit*(1:nw$n)+rotate),
                             sin(unit*(1:nw$n)+rotate))
  where <- rbind(where,c(0,0))
  where <- rbind(where,c(scale-1,scale-1))
  where <- rbind(where,c(scale-1,scale-3))
  
  nlist  <- names(nw$nodes)
  while(!quit) {

    if (mode=="Inspect") {
      bgadd <- "black"; fgadd <- "white";
      bgrem <- "white"; fgrem <- "black";
    }
    if (mode=="Change") {
      bgadd <- "white"; fgadd <- "black";
      bgrem <- "black"; fgrem <- "white";
    }

    symbols(scale-1,scale-1,rectangles=matrix(c(2,1),1),add=TRUE,bg=bgadd)
    text(scale-1,scale-1,"Inspect",col=fgadd)
    symbols(scale-1,scale-3,rectangles=matrix(c(2,1),1),add=TRUE,bg=bgrem)
    text(scale-1,scale-3,"Change",col=fgrem)

    from <- identify(where[,1],where[,2],rep("",nw$n+3),n=1)

    if (from==nw$n+1) break
    if (from==nw$n+2) { mode <- "Inspect"; next }
    if (from==nw$n+3) { mode <- "Change"; next }


    if (mode=="Change")
      {
        line()
        cat(mode, "node",nlist[from],"\n")
        print(nw$nodes[[from]]$prob)
        cat("Want to change node",nlist[from],"\n")
        cat("Not yet implemented, sorry...\n")
      }
    else if(mode=="Inspect")
      {
        line()
        cat(mode, "node",nlist[from],"\n")
        print(nw$nodes[[from]]$prob)
      }


    plot(newnet,scale,unitscale,cexscale,rotate,length,...)
    title("Inspect/Change initial probability distribution")
    points(0,0,cex=cexscale+4,pch=5)
    text(0,0,"Click here to stop")

    ##    switch(menu(c("stop","continue?\n"))+1,quit<-T,quit<-T,quit<-F)
  }
  plot(newnet,scale,unitscale,cexscale,rotate,length,...)

  newnet
}

