## rats.R --- 
## Author          : Claus Dethlefsen
## Created On      : Mon Mar 11 15:22:48 2002
## Last Modified By: Claus Dethlefsen
## Last Modified On: Mon Sep 16 16:01:49 2002
## Update Count    : 40
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

## op <- par(ask = interactive(), mfrow = c(1,1))

data(rats)
rats.df      <- rats

cat("Draw the prior DAG.\n",
"To insert an arrow from node 'A' to node 'B',\n",
"first click node 'A' and then click node 'B'.\n",
"When the DAG is finished, click 'stop'\n",
"\n",
"Then, inspect the local probability distributions\n",
"by clicking on the nodes. Finish by clicking 'stop'\n")
rats  <- network(rats.df,specifygraph=TRUE,inspectprob=TRUE)


#save this rats object
rats.orig <- rats
rats.prior <- jointprior(rats,12)

rats <- learn(rats,rats.df,rats.prior)$nw
rats.empty <- learn(network(rats.df),rats.df,rats.prior)$nw
rats.empty$banlist <- rats$banlist

line()
cat("Now, draw your favorite network. Notice how the\n",
    "network score changes. When bored, click stop\n",
    "and see how the search tries to find the network\n",
    "with highest score. The search algorithm is greedy\n",
    "search with random restart.\n")
newrat  <- drawnetwork(rats.empty,rats.df,rats.prior)$nw


hiscorelist <- heuristic(newrat,rats.df,rats.prior,restart=10,degree=7,trace=TRUE)

op <- par(ask=TRUE)
cat("Now, we have tried out several networks\n")
cat("Ready to see the Hiscorelist?\n")

print(hiscorelist$nw)
plot(hiscorelist$nw)

par(op)
rats.empty$banlist <- newrat$banlist
allrats <- networkfamily(rats.df,rats.empty,rats.prior)
op <- par(ask=TRUE)
cat("We have now generated all",numbermixed(2,2),"networks\n")

print(allrats$nw)
plot(nwfsort(allrats$nw))

par(op)
