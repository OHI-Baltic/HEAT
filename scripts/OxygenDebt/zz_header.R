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
      data = c( "dplyr", "rgeos"),
      input = c( "lubridate"),
      model = c( "mgcv", "lubridate", "MASS", "dplyr"),
      output = character(0)
    )

  # common packages -------------------
  pkg <- unique(c(pkg, "sp", "rgdal", "raster", "gplots", "viridis"))

  # install other dependencies used in the analysis
  newpkg <- setdiff(pkg, installed.packages())
  if (length(newpkg)) install.packages(newpkg, dependencies = TRUE, quiet = TRUE)

  # load packages -------------------
  tmp <- sapply(pkg, require, character.only = TRUE, quietly = TRUE)

  # load utils into oxydebt_funs space
  tmp <-
    sapply(dir("scripts/OxygenDebt/utils"),
           function(x) {
           sys.source(paste0("scripts/OxygenDebt/utils/", x), envir = environment(header))
           })

  # clear workspace -------------------
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
}




