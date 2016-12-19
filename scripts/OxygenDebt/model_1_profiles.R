# ----------------------------
#
#   Model profiles
#
#     * load oxy data and fit salinity and oxygen profiles
#
# ----------------------------


# load packages etc.
header("model")

# start timer
t0 <- proc.time()

# create directories
if (!dir.exists("analysis/output")) dir.create("analysis/output")
if (!dir.exists("analysis/output/OxygenDebt")) dir.create("analysis/output/OxygenDebt")

# read in data
oxy <- read.csv("analysis/input/OxygenDebt/oxy_clean.csv")

# run all fits
profiles <-
  do.call(rbind,
          lapply(unique(oxy$ID),
                 function(id) doonefit_full(subset(oxy, ID == id), ID = id, debug = TRUE)))

# join all profile level variables onto profiles
out <- unique(dplyr::select(oxy, -Depth, -Type,
                                 -Temperature, -Salinity, -Oxygen, -Hydrogen_Sulphide,
                                 -Oxygen_ml, -Oxygen_deficit))
rownames(out) <- paste(out$ID)

# join this onto the profiles
profiles <- cbind.data.frame(profiles, out[paste(profiles$ID),-1])

# select which data is reliable
profiles <- profiles[!is.na(profiles$depth_change_point2.se),]
# only use profiles that are based on an estimate of the lower halocline estimate with +- 20m accuracy
profiles <- profiles[-which(profiles$depth_change_point2.se > 10),]
# only use profiles that are based on an estimate of the salinity difference estimate with +- 10 accuracy
profiles <- profiles[-which(profiles$sali_dif.se > 5),]
# drop off badly estmated O2 def slopes
profiles <- profiles[-which(profiles$O2def_slope_below_halocline > 100),]

# check
if (FALSE) {
  lapply(profiles, summary)

  helcom <- rgdal::readOGR("data/OxygenDebt/shapefiles", "helcom_areas")
  sp::plot(helcom, col = gplots::rich.colors(nrow(helcom), alpha = 0.5))
  points(makeSpatial(profiles), cex = 0.5)
}

# write out
write.csv(file = "analysis/output/OxygenDebt/profiles.csv", profiles, row.names = FALSE)

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))

