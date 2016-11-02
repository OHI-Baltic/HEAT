# ----------------------------
#
#   Read in data
#
# ----------------------------

# load packages etc.
source("scripts/header.R")


# read in data
oxy <- read.csv("model/input.csv")

# run all fits
profiles <-
  do.call(rbind,
          lapply(unique(oxy$ID),
                 function(id) doonefit_full(subset(oxy, ID == id), ID = id, debug = TRUE)))

# join year, month, day, lat, long onto profiles
out <- unique(oxy[c("ID", "Assessment_Unit", "Year", "Month", "Day", "Latitude", "Longitude")])
rownames(out) <- paste(out$ID)

# join this onto the profiles
profiles <- cbind.data.frame(profiles, out[paste(profiles$ID),-1])

# write out
write.csv(file = "output/profiles.csv", profiles, row.names = FALSE)

