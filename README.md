[![CRAN version](http://www.r-pkg.org/badges/version/deal)](https://cran.r-project.org/package=deal)

deal
----
### Learning Bayesian Networks with Mixed Variables

The [**deal** package](https://cran.r-project.org/package=deal) 
offers R functions that estimate Bayesian networks with continuous and/or discrete variables can be learned and compared from data. 
It includes several methods for analysing data using Bayesian networks with variables of discrete and/or continuous types but restricted to conditionally Gaussian networks. Construction of priors for network parameters is supported and their parameters can be learned from data using conjugate updating. The network score is used as a metric to learn the structure of the network and forms the basis of a heuristic search strategy. deal has an interface to Hugin. 
The method is described in further detail in [Boettcher and Dethlefsen (2003)](https://www.jstatsoft.org/article/view/v008i20).

### Installation

The released and tested version of **deal** is available at the
[Comprehensive R Archive Network)](https://cran.r-project.org/package=GMCM).
It is installed from within R by running 

```R
install.packages("deal")
```

To install the latest version of **deal** directly from GitHub, run 

```R
#install.packages("devtools")
devtools::install_github("ClausDethlefsen/deal")
```

Ensure that you have the [package development prerequisites](https://support.posit.co/hc/en-us/articles/200486498-Package-Development-Prerequisites) 
if you wish to install the package from the source. For previous versions of **deal**, visit the [archive at CRAN.](https://cran.r-project.org/src/contrib/Archive/deal/)

### References

  1. Boettcher, S., & Dethlefsen, C. (2003). deal: A Package for Learning Bayesian Networks. Journal of Statistical Software, 8(20), 1 - 40. doi:http://dx.doi.org/10.18637/jss.v008.i20 
