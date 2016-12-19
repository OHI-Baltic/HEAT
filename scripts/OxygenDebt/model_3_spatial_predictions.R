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

#names(gams)
#g <- gams[[3]]
predict_surfaces <- function(g) {

  # predict
  # create prediction data
  pred <- dplyr::rename(data.frame(bathy), x = coords.x1, y = coords.x2)
  pred$yday <- 1

  pred_one_year <- function(y) {
    # create design matrix for spatial  and annual variation only
    Xs <- predict(g, newdata = dplyr::mutate(pred, Year = y), type = "lpmatrix")
    Xs[,grep("yday",colnames(Xs))] <- 0

    # make predictions
    c(Xs %*% coef(g))
  }

  out <- sapply(2010:2015, pred_one_year)
  colnames(out) <- 2010:2015

  # return
  out
}

# fit surfaces for each
what <- c(#"sali_surf", "sali_dif",
          "halocline", "depth_gradient",
          "O2def_below_halocline", "O2def_slope_below_halocline")

surfaces <-
  sapply(what,
    function(what) predict_surfaces(gams[[what]]),
    simplify = FALSE)

# calculate derived quantities
surfaces$depth_change_point1 <- surfaces$halocline - 1.0 * surfaces$depth_gradient
surfaces$depth_change_point2 <- surfaces$halocline + 1.0 * surfaces$depth_gradient

# check
if (FALSE) {
  lapply(surfaces, summary)
}

# From these surfaces, compute volume specific oxygen deficit.
# *    CHECK WITH SAS CODE:  numerically integrate over depth, using bathymetry.
# *         calculate O2 decifit by 1m^3, from depth changepoint 2 to bathymetric depth.
# *         compute volume of prediction "transect"
# *         finally compute total O2 deficit / total volume
# *         this results in a surface of volume specific oxygen debt.

# * Other useful quantities are - oxygen deficit at maximum depth.
pars <-
  sapply(paste(2010:2015),
    function(y) {
      out <-
        data.frame(halocline = surfaces$halocline[,y],
                   depth_change_point1 = surfaces$depth_change_point1[,y],
                   depth_change_point2 = surfaces$depth_change_point2[,y],
                   O2def_below_halocline       = surfaces$O2def_below_halocline[,y],
                   O2def_slope_below_halocline = surfaces$O2def_slope_below_halocline[,y],
                   depth = bathy$depth,
                   Basin = bathy$Basin,
                   x = sp::coordinates(bathy)[,1],
                   y = sp::coordinates(bathy)[,2])

      # only make predctions where depth extends below halocline
      out <- out[out$halocline < out$depth,]

      out$oxygendebt <-
        sapply(1:nrow(out),
               function(i) {
                 depths <- seq(out$halocline[i], out$depth[i], by = 1)
                 mean(oxy_profile(depths, out[i,])) # sum(o2) / volume
               })
      out
    },
    simplify = FALSE)

# checks
if (FALSE) {
  lapply(pars, summary)

  # plot surfaces to check
  tmp <- pars[["2010"]]
  sp::coordinates(tmp) <- c("x", "y")
  sp::proj4string(tmp) <- sp::CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

  sp::plot(tmp, col = rev(viridis::magma(50, alpha=0.5))[cut(tmp$halocline, 50)], pch = ".")
  sp::plot(helcom, add = TRUE, border = "red")

  sp::plot(tmp, col = rev(viridis::magma(50, alpha=0.5))[cut(tmp$O2def_below_halocline, 50)], pch = ".")
  sp::plot(helcom, add = TRUE, border = "red")

  sp::plot(tmp, col = rev(viridis::magma(50, alpha=0.5))[cut(tmp$oxygendebt, 50)], pch = ".")
  sp::plot(helcom, add = TRUE, border = "red")

  sapply(pars, function(x) summary(x$oxygendebt))
}

# write out --------------

save(pars, file = "analysis/output/OxygenDebt/gam_predictions.RData")


# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))

