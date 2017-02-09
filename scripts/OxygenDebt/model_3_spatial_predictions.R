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

# load gam fits ('gams')
check <- load("analysis/output/OxygenDebt/gam_fits.RData")
if (check != "gams") {
  stop("Error loading gam fits!\n\tTry rerunning model_2_spatial_profiles.R")
}
rm(check)


# read helcom assessment areas
helcom <- rgdal::readOGR("data/OxygenDebt/shapefiles", "helcom_areas", verbose = FALSE)

# read depth layer (spatial points) for prediction
bathy <- rgdal::readOGR("data/OxygenDebt/shapefiles", "helcom_bathymetry", verbose = FALSE)

# drop regions not in models!
if ("Basin" %in% names(gams[[1]]$var.summary)) {
  bathy <- bathy[bathy$Basin %in% levels(gams[[1]]$var.summary$Basin),]
}


# quick plot
if (FALSE) {
  sp::plot(bathy, col = rev(viridis::magma(50, alpha=0.5))[cut(bathy$depth, 50)], pch = ".")
  sp::plot(helcom, add = TRUE, border = "red")
}

# NOTES:
# * predict over x, y (noting region) for each year:
# *     depth changepoint 2 (lower halocline depth)
# *     O2_deficit at lower halocline
# *     O2_slope slope

# create prediction data
surfaces <- dplyr::rename(data.frame(bathy), x = coords.x1, y = coords.x2)
surfaces$yday <- 1 # predict at january 1st - this will be dropped later anyway

if (FALSE) {
  # reduce size of prediction grid for testing
  surfaces <- surfaces[sample(1:nrow(surfaces), 1000),]
}

# create prediction data for each year
surfaces <-
  do.call(rbind,
    lapply(2011:2016, function(y) cbind(surfaces, Year = y))
  )

# do predictions
surfaces <-
  do.call(cbind.data.frame,
    c(list(surfaces),
      sapply(gams,
        function(g)
        {
          # set seasonal to zero for annual variation only
          g$coefficients[grep("yday",names(coef(g)))] <- 0
          # make predictions
          exp(predict(g, newdata = surfaces))
        },
        simplify = FALSE)
      )
  )

# calculate derived quantities
surfaces$depth_change_point1 <- surfaces$halocline - 1.0 * surfaces$depth_gradient
surfaces$depth_change_point2 <- surfaces$halocline + 1.0 * surfaces$depth_gradient

# check
if (FALSE) {
  summary(surfaces)
}

# From these surfaces, compute volume specific oxygen deficit.
# *    CHECK WITH SAS CODE:  numerically integrate over depth, using bathymetry.
# *         calculate O2 decifit by 1m^3, from depth changepoint 2 to bathymetric depth.
# *         compute volume of prediction "transect"
# *         finally compute total O2 deficit / total volume
# *         this results in a surface of volume specific oxygen debt.

# * Other useful quantities are - oxygen deficit at maximum depth.

surfaces$oxygendebt <-
  sapply(1:nrow(surfaces),
    function(i) {
      # dont predict if halocline is below max depth
      if (surfaces$halocline[i] >= surfaces$depth[i]) return (NA_real_)
      # predict oxygen debt at 1m intervals and take the average
      depths <- seq(surfaces$halocline[i], surfaces$depth[i], by = 1)
      mean(oxy_profile(depths, surfaces[i,])) # sum(o2) / volume
    })


# checks
if (FALSE) {
  summary(surfaces)

  # plot surfaces to check
  tmp <- surfaces[surfaces$Year == 2011,]
  sp::coordinates(tmp) <- c("x", "y")
  sp::proj4string(tmp) <- sp::CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

  sp::plot(tmp, col = rev(viridis::magma(50, alpha=0.5))[cut(tmp$halocline, 50)], pch = 16, cex = 0.5)
  sp::plot(helcom, add = TRUE, border = "red")
  title(main = "Halocline depth")

  sp::plot(tmp, col = rev(viridis::magma(50, alpha=0.5))[cut(tmp$O2def_below_halocline, 50)], pch = 16, cex = 0.5)
  sp::plot(helcom, add = TRUE, border = "red")
  title(main = "O2def below halocline")

  sp::plot(tmp, col = rev(viridis::magma(50, alpha=0.5))[cut(tmp$oxygendebt, 50)], pch = 16, cex = 0.5)
  sp::plot(helcom, add = TRUE, border = "red")
  title(main = "Volume specific oxygen debt")

}

# write out --------------

save(surfaces, file = "analysis/output/OxygenDebt/gam_predictions.RData")


# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))

