library(deal)  ## invoke DEAL

data(ksl)      ## read data (included in DEAL)

## transform BMI to log(BMI)
ksl$BMI <- log(ksl$BMI)
names(ksl)[4] <- "logBMI"

## create factors for discrete variables
ksl$Sex  <- factor(ksl$Sex)
ksl$Year <- factor(ksl$Year)
ksl$Smok <- factor(ksl$Smok)
ksl$Alc  <- factor(ksl$Alc)
ksl$Work <- factor(ksl$Work)

## specify prior network
ksl.nw  <- network(ksl)

## make joint prior distribution
ksl.prior <- jointprior(ksl.nw)

## ban arrows towards Sex and Year
banlist <- matrix(c(5,5,6,6,7,7,9,
                    8,9,8,9,8,9,8),ncol=2)
ksl.nw$banlist <- banlist                  

## learn the initial network
ksl.nw <- learn(ksl.nw,ksl,ksl.prior)$nw

## Do structural search 
thebest <- autosearch(ksl.nw,ksl,ksl.prior,
                      saveall=FALSE,
                      trace=TRUE)$nw
 
## perturb 'thebest' and rerun search twice.
hiscorelist <- heuristic(thebest,ksl,
                         ksl.prior,
                         restart=2,degree=10,
                         trace=TRUE)

thebest2 <- hiscorelist$nw

savenet(thebest2, "ksl.net")
