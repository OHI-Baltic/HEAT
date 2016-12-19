
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
  stop("Error loading gam predictions!\n\tTry rerunning model_3_spatial_predictions.R")
}
rm(check)

# create table of indicators by year
ES_tab <-
  sapply(pars, function(parsy) {
    with(parsy, tapply(oxygendebt, Basin, mean, na.rm = TRUE))
  })
ES_tab <- cbind.data.frame(Basin = row.names(ES_tab), ES_tab)
rownames(ES_tab) <- NULL

# expand into data table form
ES_tab <- tidyr::gather(ES_tab, "Year", "ES", -1)

# add in auxilliary information
# read profile fits
profiles <- read.csv("analysis/output/OxygenDebt/profiles.csv")
N_tab <- with(profiles, tapply(O2def_slope_below_halocline, list(Basin, Year), function(x) sum(!is.na(x))))
N_tab <- cbind.data.frame(Basin = row.names(N_tab), N_tab)
rownames(N_tab) <- NULL

# expand into data table form
N_tab <- tidyr::gather(N_tab, "Year", "N", -1)
N_tab[is.na(N_tab)] <- 0

# join
out <- dplyr::left_join(ES_tab, N_tab, by = c("Basin", "Year"))

# write out
write.csv(out, file = "analysis/output/OxygenDebt/indicator_table_by_year.csv", row.names = FALSE)

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))
