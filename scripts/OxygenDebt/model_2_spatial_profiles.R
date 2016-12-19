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

# convert to factor
profiles$Basin <- factor(profiles$Basin)

# ----------------------------
#
#  model spatial distribution of halocline
#
# ----------------------------

# set model
#form <- val ~ s(x, y, k = 20) +
#              s(yday, bs = "cc", k = 6, by = Basin) +
#              s(Basin, bs = "re") +
#              s(Year, m = 1, k = 5, by = Basin)

form <- val ~ te(x, y, Year, k = c(20, 5), d = c(2,1), m = c(2,1), bs = c("tp", "tp")) +
              te(x, y, yday, k = c(10, 6), d = c(2,1), bs = c("tp", "cc"))

# fit gams
what <- c("sali_surf", "sali_dif", "halocline", "depth_gradient",
          "O2def_below_halocline", "O2def_slope_below_halocline")
gams <-
  lapply(what,
         function(what) {
           profiles$val <- profiles[[what]]
           gam(form, data = profiles, drop.unused.levels = FALSE)
         })
names(gams) <- what

# save
save(gams, file = "analysis/output/OxygenDebt/gam_fits.RData")

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))
