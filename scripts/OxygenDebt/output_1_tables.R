
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
check <- load("analysis/output/OxygenDebt/gam_predictions.RData")
if (check != "pars") {
  stop("Error loading gam predictions!\n\tTry rerunning model_3_indicators.R")
}
rm(check)


sapply(pars, function(parsy) {
  with(parsy, tapply(oxygendebt, Basin, mean, na.rm = TRUE))
})

