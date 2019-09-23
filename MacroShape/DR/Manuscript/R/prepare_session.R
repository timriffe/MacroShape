# make sure all dependencies installed
# the script modified from one found here:
#
# https://github.com/ikashnitsky/demres-geofacet/blob/master/R/session-preparation.R
################################################################################

# install pacman to streamline further package installation
if (!require("pacman", character.only = TRUE)){
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}

# Some packages to install from CRAN:
pkgs <- c(
  "here",
  "HMDHFDplus",
  "data.table",
  "reshape2",
  "colorspace",
  "devtools"
)

# A package to install from github:
gphgs <- c("DistributionTTD")

# install the missing packages from CRAN
# only runs if at least one package is missing
if(!sum(!p_isinstalled(pkgs))==0){
  p_install(
    package = pkgs[!p_isinstalled(pkgs)], 
    character.only = TRUE
  )
}

# install from github if necessary
if (!p_isinstalled(gphgs)){
  library(devtools)
  install_github("timriffe/DistributionTTD", subdir = "DistributionTTD/R/DistributionTTD")
}

# load the packages
p_load(pkgs, character.only = TRUE)
p_load(gphgs, character.only = TRUE)

# end
