## splash.R
## Author          : Claus Dethlefsen
## Created On      : Mon Dec 10 21:42:07 2001
## Last Modified By: Claus Dethlefsen
## Last Modified On: Sun Sep 15 08:19:49 2002
## Update Count    : 36
## Status          : Fine
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



splash <- function() {
  x11(width=5,height=3)
  ##  X11()
  plot(0,0,ylim=c(-50,50),xlim=c(-100,100),type="n",axes=FALSE,xlab="",ylab="",bg="skyblue1")
  for (i in 1:10) 
    text(i,i,"DEAL",cex=4,col="black")
  text(9,9,"DEAL",cex=4,col="white")
  box(cex=40,col="skyblue1")
  for (i in 1:1000000) { }
  dev.off()
  invisible()
}
