## generic.R
## Author          : Claus Dethlefsen
## Created On      : Mon Nov 19 20:48:24 2001
## Last Modified By: Claus Dethlefsen
## Last Modified On: Sun Sep 15 08:06:41 2002
## Update Count    : 77
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



line <- function(s="-",n=60) cat(rep(s,n),"\n",sep="")

prob <- function(object,...) {
  UseMethod("prob")
}

cond <- function(object,...) {
  UseMethod("cond")
}

learn <- function(object,...) {
  UseMethod("learn")
}

.First.lib <- function(lib, pkg)
{
    library.dynam("deal", package = pkg, lib.loc = lib)
    if (!exists("nosplash")) {
        if (interactive()) splash()
        nosplash <<- TRUE
    }
    cat("\n")
    cat("-------------------------------------------------------------\n")
    cat(package.description("deal", lib = lib, field="Title"))
    cat("\n")
    ver <- package.description("deal", lib = lib, field="Version")
    maint<- package.description("deal", lib = lib, field="Maintainer")
    built<- package.description("deal", lib = lib, field="Built")
    URL  <- package.description("deal", lib = lib, field="URL")
    cat(paste("DEAL, version", ver,  "is now loaded\n"))
    cat("Copyright (C) 2002, Susanne G. Bøttcher and Claus Dethlefsen\n")
    cat("Maintained by",maint,"\n")
    cat("Webpage:",URL,"\n")
    cat("\nBuilt:",built,"\n")
    cat("-------------------------------------------------------------\n")
    cat("\n")
  return(invisible(0))
}

.Last.lib <- function(lib) {
  cat("Thank you for using Deal\n")
  return(invisible(0))
}

