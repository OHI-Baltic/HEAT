
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

# load gam predictions ('pars')
check <- load("analysis/output/OxygenDebt/gam_predictions.RData")
if (check != "surfaces") {
  stop("Error loading gam predictions!\n\tTry rerunning model_3_spatial_predictions.R")
}
rm(check)


# create table of indicators by year and basin
ES_y <- with(surfaces, tapply(oxygendebt, list(Basin, Year), mean, na.rm = TRUE))
names(dimnames(ES_y)) <- c("Basin", "Year")
out_y <- do.call(expand.grid, c(dimnames(ES_y), KEEP.OUT.ATTRS = FALSE))
out_y$ES <- c(ES_y)


# create table of indicators by basin
ES <- with(surfaces, tapply(oxygendebt, list(Basin), mean, na.rm = TRUE))
names(dimnames(ES)) <- "Basin"
out <- do.call(expand.grid, c(dimnames(ES), KEEP.OUT.ATTRS = FALSE))
out$ES <- c(ES)

ES_SD <- with(surfaces, tapply(oxygendebt, list(Basin, Year), mean, na.rm = TRUE))
ES_SD <- apply(ES_SD, 1, sd)
out$ES_SD <- c(ES)


# add in auxilliary information
# read profile fits
profiles <- read.csv("analysis/output/OxygenDebt/profiles.csv")
N_y <- with(profiles, tapply(O2def_slope_below_halocline, list(Basin, Year), function(x) sum(!is.na(x))))
out_y$N <- c(N_y)

N <- with(profiles, tapply(O2def_slope_below_halocline, list(Basin), function(x) sum(!is.na(x))))
out$N <- c(N)

# inspect
if (FALSE) {
  out
  out_y

  # check
  cbind(with(out_y, tapply(N, Basin, sum)), out$N)
}


# write out
write.csv(out, file = "analysis/output/OxygenDebt/indicator_table.csv", row.names = FALSE)
write.csv(out_y, file = "analysis/output/OxygenDebt/indicator_table_by_year.csv", row.names = FALSE)

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))
