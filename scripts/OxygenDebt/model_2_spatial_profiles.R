# ----------------------------
#
#   Model spatial profiles
#
#     * load profile parameters and fit spatial temporal models
#
# ----------------------------

# load packages etc.
header("model")

# start timer
t0 <- proc.time()

# read profile fits
profiles <- read.csv("analysis/output/OxygenDebt/profiles.csv")

# ----------------------------
#
#  model spatial distribution of halocline
#
# ----------------------------

# select which data is reliable
profiles <- profiles[!is.na(profiles$depth_change_point2.se),]
# only use profiles that are based on an estimate of the lower halocline estimate with +- 20m accuracy
profiles <- profiles[profiles$depth_change_point2.se < 10,]
# only use profiles that are based on an estimate of the salinity difference estimate with +- 10 accuracy
profiles <- profiles[profiles$sali_dif.se < 5,]

# convert to factor
profiles$Basin <- factor(profiles$Basin)
levels(profiles$Basin)

#lapply(profiles, summary, na.rm = TRUE)

# set model
form <- val ~ s(x, y, k = 20) +
            s(yday, bs = "cc", k = 6, by = Basin) +
            Basin*factor(Year)

# fit gams
what <- c("sali_surf", "sali_dif", "halocline", "depth_gradient",
          "depth_change_point1", "depth_change_point2",
          "O2def_below_halocline", "O2def_slope_below_halocline")
gams <-
  lapply(what,
         function(what) {
           #cat("\rfitting", what, rep(" ", 50)); flush.console()
           profiles$val <- profiles[[what]]
           gam(form, data = profiles, drop.unused.levels = FALSE)
         })
names(gams) <- what

# save
save(gams, file = "analysis/output/OxygenDebt/gam_fits.RData")

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))
