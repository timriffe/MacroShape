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

# library(data.table)
# library(reshape2)
# library(here)
# library(RColorBrewer)
# library(colorspace)
# these are the required packages
pkgs <- c(

)

gphgs <- c("DistributionTTD")

# install the missing packages
# only runs if at least one package is missing
if(!sum(!p_isinstalled(pkgs))==0){
  p_install(
    package = pkgs[!p_isinstalled(pkgs)], 
    character.only = TRUE
  )
}

# load the packages
p_load(pkgs, character.only = TRUE)

