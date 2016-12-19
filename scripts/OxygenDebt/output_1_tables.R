
# ----------------------------
#
#   Table indicators
#
#     * load indicator surfaces
#
# ----------------------------

# load packages etc.
header("output")

# start timer
t0 <- proc.time()

# load gam fits ('gams')
check <- load("analysis/output/OxygenDebt/gam_surfaces.RData")
if (check != "pars") {
  stop("Error loading gam fits!\n\tTry rerunning model_3_indicators.R")
}
rm(check)
