## timeslice.R --- 
## Author          : Claus Dethlefsen
## Created On      : Thu Mar 14 13:33:22 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Sun Sep 15 08:20:14 2002
## Update Count    : 65
## Status          : Unknown, Use with caution!
###############################################################################

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

timeslice <- function(df, tvar, id=0, time=0, dropid=TRUE) {
    ## df  : dataframe
    ## time: column number(s) in df indexing time (this is just dropped)
    ## id  : column number in df indexing individual group
    ## tvar: vector of columns in df that are timevarying
    ##
    ## result: a dataframe consisting of the columns of df that are
    ## not mentioned in the arguments and two columns for each tvar
    ## variable. These are the neigbouring pairs of observations with
    ## regard to time. Observations are not paired across individuals (id).

    ## NOTE: na.omit should be applied _after_ this function -- not before.

    ## We fill in NA's in non-time varying variables.

    if (!id==0) idlist <- unique(df[,id])
    else idlist <- 0

    ## make smalldf
    if (!id==0) {
        smalldf <- df[1,]
        for (idx in idlist) {
            this <- df[df[,id]==idx,]
            smalldf <- rbind(smalldf,this[1,])
        }
        if (dropid)
            smalldf <- smalldf[-1,-id]
        else
            smalldf <- smalldf[-1,]
    }
    else
        smalldf <- df
    
    ## make bigdf
    bigdf <- df[1,]
    bigdf <- cbind(bigdf,matrix(1,1,length(tvar)*2))

    nvn <- names(df)
    for (nn in tvar) 
        nvn <- c(nvn,paste(nvn[nn],"_t-1",sep=""),paste(nvn[nn],"_t",sep=""))

    names(bigdf) <- nvn
        
    if (idlist) {
        for (ib in idlist) {
            cat(ib," ")
            this <- df[df[,id]==ib,]
            this.df <- this[-1,]

            ## added 18/3
##            this.df[2:nrow(this.df),] <- NA

            for (tv in tvar) 
                this.df <-
                    cbind(this.df,this[-nrow(this),tv],this[-1,tv])

            
            names(this.df) <- nvn
            bigdf <- rbind(bigdf,this.df)
            
        }
    } ## if
    else {  ## no id
##        cat("no id info\n")
        
        this <- df
        this.df <- this[-1,]
        
        for (tv in tvar) 
            this.df <-
                cbind(this.df,this[-nrow(this),tv],this[-1,tv])

        colnames(this.df) <- nvn
        bigdf <- rbind(bigdf,this.df)
        
    }


    ## drop columns: time + tvar
    droplist <- tvar
    
    if (!time==0)
        droplist <- c(droplist,time)
    if (dropid)
        droplist <- c(droplist,id)
        
    list(bigdf=bigdf[-1,-c(tvar,time,id)],smalldf=smalldf)
}
