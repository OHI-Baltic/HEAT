
# ----------------------------
#
#   install required packages
#
# ----------------------------

# install the oxygen debt functions
devtools::install_github("ices-tools-dev/oxydebt")

# install other dependencies used in the analysis
pkgs <- c("mgcv", "lubridate", "MASS", "sp", "rgdal", "raster", "dplyr")
install.packages(pkgs)
