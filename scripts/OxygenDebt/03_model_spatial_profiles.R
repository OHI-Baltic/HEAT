# ----------------------------
#
#   Read in data
#
# ----------------------------

# load packages etc.
source("scripts/OxygenDebt/header.R")

# read profile fits
profiles <- read.csv("output/profiles.csv")

# read assessment unit shape file
assessmentArea <- rgdal::readOGR("input", "AssessmentUnit_20112016Polygon")
names(assessmentArea) <- c("Assessment_Unit", "Basin")
# keep only SEA units ?
assessmentArea <- assessmentArea[assessmentArea$Assessment_Unit %in% unique(profiles$Assessment_Unit),]

# read depth profile
bathy <- raster("input/GMT_GEBCO_08_BalticSea.asc")
proj4string(bathy) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# trim to extent of assessment units
bathy <- raster::mask(bathy, assessmentArea)
# don't predict over depths less than..
bathy[bathy > -40] <- NA

# ----------------------------
#
#  Inspect spatial distribution of halocline
#
# ----------------------------

gam2raster <- function(g, r) {
  # get x and y values
  e <- extent(r)
  rx <- seq(e[1]+xres(r)/2, e[2]-xres(r)/2, length = ncol(r))
  ry <- seq(e[4]-yres(r)/2, e[3]+yres(r)/2, length = nrow(r))

  # make sure and match x and y to unpacking order of raster values
  pred <-
    data.frame(Longitude = rep(rx, nrow(r)),
               Latitude = rep(ry, each = ncol(r)),
               yday = 1,
               Year = 2015)
  mask <- which(!is.na(r[]))
  pred <- pred[mask,]
  names(r) <- "layer"
  r[] <- NA

  # predict
  X <- predict(g, newdata = pred, type = "lpmatrix")
  b <- coef(g)
  b[grep("yday",names(b))] <- 0
  b[grep("Year",names(b))] <- 0
  r[mask] <- X%*%b
  r
}

## fit surface to profile

# select which data is reliable
profiles <- profiles[!is.na(profiles$depth_change_point2.se),]
# only use profiles that are based on an estimate of the lower halocline estimate with +- 20m accuracy
profiles <- profiles[profiles$depth_change_point2.se < 10,]
# only use profiles that are based on an estimate of the salinity difference estimate with +- 10 accuracy
profiles <- profiles[profiles$sali_dif.se < 5,]

#lapply(profiles, summary, na.rm = TRUE)

# set up covariates
profiles$date <- ymd(with(profiles, paste(Year, Month, Day, sep = "-")))
profiles$yday <- yday(profiles$date)

# set model
form <- y ~ s(Longitude, Latitude, k = 20) + 
            s(yday, bs = "cc", k = 6, by = Basin) + 
            factor(Year)*factor(Basin)

# fit gam and rasterise and save
for (what in c("sali_surf", "sali_dif", "halocline", "depth_gradient", "O2def_below_halocline", "O2def_slope_below_halocline")) {
  cat("\rfitting", what, rep(" ", 50)); flush.console()
  profiles$y <- profiles[[what]]
  g <- gam(form, data = profiles)
  r <- gam2raster(g, bathy)
  assign(paste0("r", what), r)
  writeRaster(r, filename = paste0("output/", what, ".tif"), format = "GTiff", overwrite = TRUE)
}


# NOTES:
# * predict over x, y for each year:
# *     depth changepoint 2 (lower halocline depth)
# *     O2_deficit at lower halocline
# *     O2_slope slope
#
# From these surfaces, compute volume specific oxygen deficit.
# *    CHECK WITH SAS CODE:  numerically integrate over depth, using bathymetry.
# *         calculate O2 decifit by 1m^3, from depth changepoint 2 to bathymetric depth.
# *         compute volume of prediction "transect"
# *         finally compute total O2 deficit / total volume
# *         this results in a surface of volume specific oxygen debt.

# * Other useful quantities are - oxygen deficit at maximum depth.

# * 