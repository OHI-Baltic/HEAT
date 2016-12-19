
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


tab1 <-
  sapply(pars, function(parsy) {
    with(parsy, tapply(oxygendebt, Basin, mean, na.rm = TRUE))
  })

# write out
write.csv(tab1, file = "analysis/output/OxygenDebt/indicator_table.csv", row.names = FALSE)

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))
