## numbermixed.R
## Author          : Claus Dethlefsen
## Created On      : Sat Mar 02 11:37:20 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Thu Mar 14 10:47:48 2002
## Update Count    : 13
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


numbermixed <- function(nd,nc) {
  ## number of mixed networks with nd discrete and nc continuous nodes
  ## (see Bøttcher (2002))

    robinson <- function(n) {
        ## The Robinson (1977) recursive formula for the number of possible
        ## DAG's that contain n nodes
        if (n==0) return(1)
        if (n==1) return(1)
        else {
            res <- 0
            for (i in 1:n) {
                res <- res + (-1)^(i+1) * choose(n,i) * 2^(i*(n-i)) * robinson(n-i)
            }
        }
        res
    }
    
    
    robinson(nd)*robinson(nc)*2^(nd*nc)
}
