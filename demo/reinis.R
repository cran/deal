library(deal)

data(reinis)

## specify prior network
reinis.nw  <- network(reinis)

## make joint prior distribution
reinis.prior <- jointprior(reinis.nw,128)

## learn the initial network
reinis.nw <- learn(reinis.nw,reinis,reinis.prior)$nw

mynet <- newnetwork(reinis.nw,data=reinis,prior=reinis.prior)

dynamic.Graph(mynet)
