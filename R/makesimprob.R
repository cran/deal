## makesimprob.R
## Author          : Claus Dethlefsen
## Created On      : Tue Feb 26 13:25:44 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Wed Sep 18 08:46:13 2002
## Update Count    : 139
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

## SKAL OGSÅ FÅ SSH TIL AT AFHÆNGE AF idx og j

## Det skal ikke være j. Man skal lave findex og få niveauerne af de
## diskrete variable og bruge dem (!). Mere tænkearbejde!

makesimprob <- function(nw,
                        s2=function(idx,cf) {
                          cf <- as.vector(cf)
                          xs <- (1:length(cf))
                          log(xs%*%cf+1)
                        },
                        m0=function(idx,cf) {
                          cf <- as.vector(cf)
                          xs <- (1:length(cf))^2
                          .69*(xs%*%cf)
                          },
                        m1=function(idx,cf) {
                          cf <- as.vector(cf)
                          xs <- (1:length(cf))*10
                          idx*(cf%*%xs)
                          }) {
  ## sets up (and asks user for) probablities to simulate from
  ##
  ## Idea: let s2 and m1 depend on the node-index and on j
  ## Perhaps passing functions as arguments?
  ##
  ## Discrete variables are organised as follows
  ## The table always has the node itself as the first one. The
  ## remaining (conditioning) are sorted according to their index. We
  ## let the probabilities be equal.

#  cat("(makesimprob: Der er vist noget der ikke summer rigtigt til 1)\n")
  for (nid in 1:nw$n) {

    node <- nw$nodes[[nid]]
    parents <- node$parents
if (nw$nd>0)    dparents<- sort(intersect(parents,nw$discrete))
    else dparents <- c()
if (nw$nc>0)    cparents<- sort(intersect(parents,nw$continuous))

#    line()
#    cat("Node: ",node$name,"\n")

#    cat("dparents=",dparents,"\t cparents=",cparents,"\n")

    if (length(dparents)>0) {
      Dim <- c()
      dnames <- list(node$levelnames)
#      dnames <- list()      
      for (i in dparents) {
        Dim <- c(Dim,nw$nodes[[i]]$levels)
        dnames <- c(dnames,list(nw$nodes[[i]]$levelnames))
      }
#      cat("Dim=\n");      print(Dim)
      TD <- prod(Dim)
#      cat("TD=",TD,"\n")

      ## dan alle teksterne i den rigtige rækkefølge
      lvek <- c()
      for (i in 1:TD) {
        cf <- findex( i, Dim, FALSE)
        label <- ""
        for (j in 1:ncol(cf)) {
          label <- paste(label, nw$nodes[[dparents[j]]]$levelnames[cf[1,j]]
      ,sep=":")
        }
        lvek <- c(lvek,label)
      }

      
    }
    else {
      dnames <- list(node$levelnames)
#      dnames <- list()      
      TD  <- 1
      Dim <- c()
    }

    
    if (node$type=="continuous") {
      M <- matrix(NA,TD,1+1+length(cparents))

      if (length(dparents)>0) rownames(M) <- lvek

      colnames(M) <- c("s2","m0",names(nw$nodes[cparents]))

      for (it in 1:nrow(M)) {
        ifelse(TD>1,cf <- findex( it, Dim, FALSE), cf <- 1)        
        M[it,1] <- s2(nid,cf)
        M[it,2] <- m0(nid,cf)
        if (length(cparents)>0) {
          for (itt in 1:length(cparents))
            M[it,2+itt] <- m1(nid,cf)
        }
      }
      
#      print(M)
#      cat("Here, we should have a module for changing these probs.\n")
      
      nw$nodes[[nid]]$simprob <- M
    }
    else if (node$type=="discrete") {

      Dim <- c(node$levels,Dim)
#      cat("Dim:\n");print(Dim)
      simtab <- array(1/prod(Dim),dim=Dim)
      dimnames(simtab) <- dnames
#      print(simtab)
#      cat("Here, we should have a module for changing these probs.\n")

      ## added 7/3 2002
#      print(simtab)
      if (length(node$parents)>0)
        simtab <- prop.table(simtab,2:(length(node$parents)+1))
#      print(simtab)
      
      nw$nodes[[nid]]$simprob <- simtab
    }
    else stop("Type is wrong")
  }
  
  nw
}
