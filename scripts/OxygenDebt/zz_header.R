# ----------------------------
#
#   load packages and clear workspace
#
# ----------------------------

header <-
function(phase = c("data", "input", "model", "output"))
{
  phase <- match.arg(phase)
  # phase specific packages -------------------
  pkg <-
    switch(phase,
      data = c("oxydebt", "dplyr", "rgeos"),
      input = c("oxydebt", "lubridate"),
      model = c("oxydebt", "mgcv", "lubridate", "MASS", "dplyr"),
      output = character(0)
    )

  # common packages -------------------
  pkg <- unique(c(pkg, "sp", "rgdal", "raster", "gplots", "viridis"))

  # install the oxygen debt functions
  if (!"oxydebt" %in% installed.packages()) {
    devtools::install_github("ices-tools-dev/oxydebt", quiet = TRUE)
  }

  # install other dependencies used in the analysis
  newpkg <- setdiff(pkg, installed.packages())
  if (length(newpkg)) install.packages(newpkg, dependencies = TRUE, quiet = TRUE)

  # load packages -------------------
  tmp <- sapply(pkg, require, character.only = TRUE, quietly = TRUE)

  # clear workspace -------------------
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}




