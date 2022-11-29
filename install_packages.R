#This quick bit of code will ensure you have all the packages needed:

rm(list = ls())
#Set up the needed packages:
packages <- c("tidyverse", "INLA", "sf", "spdep", "orderly",
              "pkgdepends")
miss_pkgs <- packages[!packages %in% installed.packages()[,1]]
if(length(miss_pkgs) > 0){
  install.packages(miss_pkgs)
}

#If INLA package doesn't install you may need to run this:
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)


#This will load the packages if needed
#invisible(lapply(packages, library, character.only = TRUE))


rm(miss_pkgs, packages)

#Prior to installing RStan, you need to configure your R installation 
#to be able to compile C++ code. Follow the link below for your
#respective operating system for more instructions:
#https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Windows

#First you need Tools42
#Download and install from here:
#https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html

#he version of RStan currently available on CRAN is not compatible with R4.2
#Instead, you will need to install the preview of rstan 2.26 using:
install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

