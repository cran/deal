## drawnetwork.R
## Author          : Claus Dethlefsen
## Created On      : Fri Nov 30 22:05:59 2001
## Last Modified By: Claus Dethlefsen
## Last Modified On: Mon Sep 16 18:43:34 2002
## Update Count    : 216
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

## Ideen er, at man udfra det trivielle netværk kan tegne pile fra og
## til punkter (vha. identify og locator). Hvis man tegner en ulovlig
## pil kan man enten vende den eller undlade den.

## retunerer et netvaerk (med en prob)


drawnetwork <-  function(nw,df,prior,trylist=rep(list(NULL),nw$n),scale=10,unitscale=1.3,cexscale=10,rotate=pi/4,length=.25,nocalc=FALSE,smalldf=NA,...) {

  ## arguments are the same as for plot.network. Well, not
  ## anymore...
  ## nocalc=T: don't calculate scores (for use with 'specifynetwork')
  
  par(mfrow=c(1,1))  
  plot(nw,scale,unitscale,cexscale,rotate,length,showban=TRUE,...)

  points(0,0,cex=cexscale+4,pch=5)
  text(0,0,"Click here to stop")


  mode <- "Add"
  banmode <- FALSE
  if (length(nw$banlist)>0)
      banlist <- nw$banlist
  else
      banlist <- matrix(0,0,2)

##  print(banlist)
  
  newnet <- nw
  quit   <- FALSE
  unit   <- 2*pi/nw$n
  where  <- 0.7*scale* cbind(
                             cos(unit*(1:nw$n)+rotate),
                             sin(unit*(1:nw$n)+rotate))
  where <- rbind(where,c(0,0))
  where <- rbind(where,c(scale-1,scale-1))
  where <- rbind(where,c(scale-1,scale-3))
  where <- rbind(where,c(scale-1,scale-5))
  
  nlist  <- names(nw$nodes)
  while(!quit) {

##      print(newnet$banlist)
      
    if (mode=="Add") {
      bgadd <- "black"; fgadd <- "white";
      bgrem <- "white"; fgrem <- "black";
    }
    if (mode=="Remove") {
      bgadd <- "white"; fgadd <- "black";
      bgrem <- "black"; fgrem <- "white";
    }

    if (banmode) {
        bgban <- "black"; fgban <- "white";}
    else {
        bgban <- "white"; fgban <- "black"; }
        
    
    symbols(scale-1,scale-1,rectangles=matrix(c(2,1),1),add=TRUE,bg=bgadd)
    text(scale-1,scale-1,"Add",col=fgadd)
    symbols(scale-1,scale-3,rectangles=matrix(c(2,1),1),add=TRUE,bg=bgrem)
    text(scale-1,scale-3,"Remove",col=fgrem)

    symbols(scale-1,scale-5,rectangles=matrix(c(2,1),1),add=TRUE,bg=bgban)
    text(scale-1,scale-5,"Ban",col=fgban)

    from <- identify(where[,1],where[,2],rep("",nw$n+4),n=1)

    if (from==nw$n+1) break
    if (from==nw$n+2) { mode <- "Add"; next }
    if (from==nw$n+3) { mode <- "Remove"; next }
    if (from==nw$n+4) { banmode <- !banmode;next }

    cat(mode, "arrow from",nlist[from],"to ")
    to <- identify(where[,1],where[,2],rep("",nw$n+4),n=1)
    
    if (to==nw$n+1) break
    if (to==nw$n+2) { mode <- "Add"; next }
    if (to==nw$n+3) { mode <- "Remove"; next }
    if (to==nw$n+4) { banmode <- !banmode;next }
    
    cat(nlist[to],"\n")

    if (!banmode) {
        if (mode=="Add") {
            tempnet <-
        insert(newnet,from,to,df,prior,nocalc,trylist=trylist,smalldf=smalldf)
        }
        else if(mode=="Remove")
            tempnet <- remover(newnet,from,to,df,prior,nocalc,trylist=trylist,smalldf=smalldf)
        
        
        if (length(tempnet$nw)>0) {
#            cat("going to test for cycles:",cycletest(tempnet$nw),"\n")
            if (!cycletest(tempnet$nw)) {
                newnet <- tempnet
                trylist <- newnet$trylist
                newnet <- newnet$nw
            }        
            else
                cat("Oh, no - you created a cycle. Try again\n")
        }
        else cat("something happened\n")
    }
    else {
##        cat("banmode is on...\n")
           if (mode=="Add") {
#               cat("Trying to add",from,"->",to,"to banlist\n")
               ## hvad skal der gælde nu?
               ## from!=to
               ## pil lovlig, dvs ej fra cont. til diskret
               ## pil ej eksistere i netværk
               ## pil ej eksistere i banlist
               ## pil ej danne cycle i banlist-netværk
                 if (from==to) {
                     cat("Can't add the arrow:",from,"->",to,"\n")
                     next
                 }
                 else if (nw$nodes[[to]]$type=="discrete" &
                          nw$nodes[[from]]$type=="continuous")
                 {
                     cat("Arrow (",from,"->",to,") illegal\n")
                     next
                 }
               else if (!is.na(match(from,newnet$nodes[[to]]$parents))) {
                   cat("Can't add arrow(",from,"->",to,")\n",
                       "it's already in the graph\n")
                   next
               }
#               else if (!is.na(match(from,banlist[,1]))&
#                        !is.na(match(to,banlist[,2]))) {
#                   cat("Can't add arrow(",from,"->",to,")\n",
#                       "already in banlist\n")
#                   next
#               }
#                              cat("Bingo\n")
               banlist <- rbind(banlist,c(from,to))
#               print(banlist)
           }
           else if(mode=="Remove") {
               ##        cat("Trying to remove",from,"->",to,"from banlist\n")
               if (!nrow(banlist)>0) {
                   ## cat("nothing in banlist\n")
                   next
               }
               idx <- (1:nrow(banlist))[banlist[,1]==from]
               if (!length(idx)>0) {
                   ## cat("Not in banlist\n")
                   next
               }
               if (!is.na(match(to,banlist[idx,2]))) {
                   ## cat("removing from banlist\n")
                   banlist <- banlist[-idx[match(to,banlist[idx,2])],]
                   banlist <- matrix(banlist,ncol=2)
                   next
               }
               
               ##  cat("Its not in the banlist\n")
                   

           }
    }

    
    newnet$banlist <- banlist
    plot(newnet,scale,unitscale,cexscale,rotate,length,showban=TRUE,...)
    points(0,0,cex=cexscale+4,pch=5)
    text(0,0,"Click here to stop")

    ##    switch(menu(c("stop","continue?\n"))+1,quit<-T,quit<-T,quit<-F)
  }
  plot(newnet,scale,unitscale,cexscale,rotate,length,showban=TRUE,...)

  list(nw=newnet,trylist=trylist)
}

