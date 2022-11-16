#This quick bit of code will ensure you have all the packages needed:

rm(list = ls())
#Set up the needed packages:
packages <- c("tidyverse", "INLA", "rstan", "sf", "spdep", "orderly",
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
