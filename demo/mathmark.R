library(deal)

data(mathmark,package="gRbase")

## specify prior network
mathmark.nw  <- network(mathmark)

## make joint prior distribution
mathmark.prior <- jointprior(mathmark.nw,5)

## learn the initial network
mathmark.nw <- learn(mathmark.nw,mathmark,mathmark.prior)$nw

mynet <- newnetwork(mathmark.nw,data=mathmark,prior=mathmark.prior)

dynamic.Graph(mynet)
