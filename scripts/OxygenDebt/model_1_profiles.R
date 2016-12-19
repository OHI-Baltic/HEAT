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

# write out
write.csv(file = "analysis/output/OxygenDebt/profiles.csv", profiles, row.names = FALSE)

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))

