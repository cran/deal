ksl <- read.table("ksl.txt",header=TRUE)
ksl$BMI <- log(ksl$BMI)
names(ksl)[4] <- "logBMI"

## create factors for discrete variables
ksl$Sex  <- factor(ksl$Sex)
ksl$Year <- factor(ksl$Year)
ksl$Smok <- factor(ksl$Smok)
ksl$Alc  <- factor(ksl$Alc)
ksl$Work <- factor(ksl$Work)
