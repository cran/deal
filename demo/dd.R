#library(deal)  ## invoke DEAL
#library(tcltk)
#library(dynamicGraph)

#source("networkclass.R")
#source("dealObjects.R")

#source("usermenus.R")

data(ksl)      ## read data (included in DEAL)

## specify prior network
ksl.nw  <- network(ksl)

## make joint prior distribution
ksl.prior <- jointprior(ksl.nw,64)

## ban arrows towards Sex and Year
mybanlist <- matrix(c(5,5,6,6,7,7,9,
                    8,9,8,9,8,9,8),ncol=2)
banlist(ksl.nw) <- mybanlist                  

## learn the initial network
ksl.nw <- learn(ksl.nw,ksl,ksl.prior)$nw

ksl.best <- readnet(file("ksl.net"))
ksl.best <- learn(ksl.best,ksl,ksl.prior)$nw

mynet <- newnetwork(ksl.best,data=ksl,prior=ksl.prior)

dynamic.Graph(mynet)

