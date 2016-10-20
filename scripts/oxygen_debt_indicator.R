
# ----------------------------
#
#   Some flags
#
# ----------------------------

interactive <- TRUE

# ----------------------------
#
#   read oxygen and salinty data and merge
#
# ----------------------------

cleanColumnNames <- function(x) {
  x <- gsub("ï»¿", "", x) # clean odd characters in stationID
  x <- gsub("\\[[^\\]]*\\]|\\:.*$|\\.", "", x, perl=TRUE) # remove text between [...]
  #  \[                       # '['
  #    [^\]]*                 # any character except: '\]' (0 or more
  #                           # times (matching the most amount possible))
  #    \]                     # ']'
  #    \\:.*$                 # remove everything after colon
  #    \\.                    # remove .
  x <- gsub("[ ]+$", "", x)   # now remove trailing space
  x <- gsub("[ ]+", "_", x) # replace spaces with _
  x
}

read.stationsamples <- function(fname) {
  out <- read.table(fname, sep = "\t", header = TRUE, na.string = "NULL")
  clean_names <- strsplit(readLines(fname, n = 1), "\t")[[1]]
  names(out) <- cleanColumnNames(clean_names)
  out
}

# read bottle data and clean column names
ice <- read.stationsamples("input/StationSamplesICE.txt")

# read CTD data and clean column names
ctd <- read.stationsamples("input/StationSamplesCTD.txt")

# merge and keep only SEA units?
if (interactive) {
  intersect(names(ctd), names(ice))
}
#oxy <- dplyr::full_join(ice, ctd)
oxy <- ice[intersect(names(ctd), names(ice))] # keep only ice for now
oxy <- oxy[grepl("SEA-", oxy$Assessment_Unit),]

# rm non required data
rm(ice, ctd, cleanColumnNames, read.stationsamples)

if (interactive) {
  # inspect
  str(oxy)
}

# ----------------------------
#
#  Read in spatial data
#
# ----------------------------

# read assessment unit shape file
assessmentArea <- rgdal::readOGR("input", "AssessmentUnit_20112016Polygon")
names(assessmentArea) <- c("Assessment_Unit", "Basin")
# keep only SEA units ?
assessmentArea <- assessmentArea[grepl("SEA-", assessmentArea$Assessment_Unit),]

# make a spatial data.frame from oxy
makeSpatialOxy <- function(x) {
  x <- unique(x[c("StationID", "Latitude", "Longitude")])
  sp::coordinates(x) <- ~ Longitude + Latitude
  sp::proj4string(x) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  x
}
spoxy <- makeSpatialOxy(oxy)

# read depth profile
require(raster)
bathy <- raster::raster("input/GMT_GEBCO_08_BalticSea.asc")
sp::proj4string(bathy) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# trim to extent of assessment units
bathy <- raster::mask(bathy, assessmentArea)

if (interactive) {
  # plot regions with names
  sp::plot(assessmentArea, col = gplots::rich.colors(17, alpha = 0.5))
  text(sp::coordinates(assessmentArea),
       labels = paste(assessmentArea$Assessment_Unit, assessmentArea$Basin, sep = "\n"),
       cex = .7)

  # plot regions with station locations
  sp::plot(assessmentArea, col = gplots::rich.colors(17, alpha = 0.5))
  sp::plot(spoxy, cex = 0.5, add = TRUE, pch = 1)

  # plot bathymetry
  raster::plot(bathy, col = colorRampPalette(c("darkblue", "lightblue"))(255))
}


# ----------------------------
#
#  tidy data and form new variables
#
# ----------------------------

# add a sampleid column ?
#oxy$event_id <- with(data.frame(oxy), paste(Latitude, Longitude, Year, Month, Day, Hour, Minute, sep = "-"))
#oxy$event_id <- paste(as.numeric(as.factor(oxy$event_id)))
#length(unique(oxy$event_id))
# [1] 12462
#length(unique(oxy$StationID))
# [1] 12465

# drop missing observations ?
oxy <- subset(oxy, !is.na(Salinity) | !is.na(Oxygen))

# oxygen observations are in ml/l - convert to mg/l
oxy$Oxygen_ml <- oxy$Oxygen
oxy$Oxygen <- oxy$Oxygen_ml * 1.428

# compute max (non NA) sample depth
oxy$max_depth <- c(with(oxy, tapply(Depth, StationID, max))[paste(oxy$StationID)])

# compute oxygen deficit
O2satFun <- function(temp) {
  tempabs <- temp + 273.15
  exp(-173.4292 + 249.6339 * (100/tempabs) +
        143.3483 * log(tempabs/100) - 21.8492 * (tempabs/100) +
        (-0.033096 + 0.014259 * (tempabs/100) - 0.0017000 * (tempabs/100)^2)
  ) * 1.428  # * Oxygen saturation in mg/l
}
oxy$Oxygen_deficit <- O2satFun(oxy$Temperature) - oxy$Oxygen

if (interactive) {
  # inspect
  str(oxy)
}

# ----------------------------
#
#  merge table defnining surface by area
#
# ----------------------------

# read in auxilliary info
aux <- read.table("input/auxilliary.txt", sep = "\t", header = TRUE)

# merge
oxy <- dplyr::left_join(oxy, aux)

if (interactive) {
  # inspect
  str(oxy)
}

# ----------------------------
#
#  fit salinity profiles and oxygen profiles
#
# ----------------------------

## The profile estimation procedure

# profile functions
sali_profile <- function(depth, pars) {
  # requires a vector 'pars' with names: sali_surf, sali_dif, halocline, depth_gradient
  with(pars, sali_surf + sali_dif * pnorm( (depth-halocline)/depth_gradient ))
}

# define oxygen profile
oxy_profile <- function(depth, pars) {
  # requires a vector 'pars' with names:
  #     depth_change_point1, depth_change_point2
  #     O2def_below_halocline, O2def_slope_below_halocline
  mod1 <- function(x) {
    with(pars, rep(0, length(x)))
  }
  mod2 <- function(x) {
    if (is.na(pars$O2def_slope_below_halocline)) {
      rep(NA, length(x))
    } else {
      with(pars, approx(c(depth_change_point1, depth_change_point2), c(0, O2def_below_halocline), x)$y)
    }
  }
  mod3 <- function(x) {
    if (is.na(pars$O2def_slope_below_halocline)) {
      rep(NA, length(x))
    } else {
      with(pars, (x - depth_change_point2) * O2def_slope_below_halocline + O2def_below_halocline)
    }
  }

  ifelse(depth < pars$depth_change_point1, mod1(depth),
    ifelse(depth < pars$depth_change_point2, mod2(depth),
                                        mod3(depth)))
}

if (interactive) {
  tail(sort(table(oxy$StationID)),10)
#  450156  450151  450152  450153  450154  450148  450150  450155  450149 7563137
#      64      70      81      82      83      85      85      85      87     105
  data <- subset(oxy, StationID == "7563137")
}

# profile estimation function
doonefit <- function(data) {
  # output container
  fit <- data.frame(StationID = data$StationID[1],
                    sali_surf = NA, sali_dif = NA, halocline = NA, depth_gradient = NA,
                    depth_change_point1 = NA, depth_change_point2 = NA,
                    O2def_below_halocline = NA, O2def_slope_below_halocline = NA)

  # only proceed if there are enough obs to estimate surface salinity
  wk <- with(data, Salinity[Depth >= surfacedepth1 & Depth <= surfacedepth2])
  if (sum(!is.na(wk)) < 2) {
    return(fit)
  }

  # compute sali_surf (this should change by basin)
  data$sali_surf <- fit$sali_surf <- with(data, mean(Salinity[Depth >= surfacedepth1 & Depth <= surfacedepth2], na.rm = TRUE))

  # strip off data for salinity modelling
  sal <- subset(data, !is.na(Salinity) & Depth > 20 & max_depth >= 80)
  if (nrow(sal) < 4) {
    return(fit)
  }

  # fit salinity profile
  model <- try(
    nls(Salinity ~ sali_surf + sali_dif * pnorm( (Depth-halocline)/depth_gradient ),
        data = data,
        lower = list(sali_dif = -0.5, halocline =  40, depth_gradient =   1),
        start = list(sali_dif =  4  , halocline =  50, depth_gradient =  10),
        upper = list(sali_dif =  Inf, halocline = 120, depth_gradient = 100),
        algorithm = "port"), silent = TRUE)
  if (inherits(model, "try-error")) {
    return(NULL)
  }
  fit[names(coef(model))] <- coef(model)

  # calculate change points
  fit$depth_change_point1 <- with(fit, halocline - 1.0 * depth_gradient)
  fit$depth_change_point2 <- with(fit, halocline + 1.0 * depth_gradient)

  # compute oxygen deficit
  O2 <- subset(data, !is.na(Oxygen_deficit))

  # estimate O2 level in surface layer
  #fit$O2def_seg1_level <- mean(O2$Oxygen_deficit[O2$Depth < fit$depth_change_point1])

  # calculate O2 for second change point
  id <- which(O2$Depth > fit$depth_change_point2)[1]
  if (is.na(id) | nrow(O2) < 2) {
    # no data above change point
    return(fit)
  }
  id <- id  + -1:0
  if (diff(O2$Depth[id]) > 30) {
    # use depth
    fit$O2def_below_halocline <- approx(O2$Depth[id], O2$Oxygen_deficit[id], xout = fit$depth_change_point2)$y
  } else {
    # use salinity curve
    sali_preds <- sali_profile(O2$Depth[id], fit)
    sali_change_point2 <- sali_profile(fit$depth_change_point2, fit)
    fit$O2def_below_halocline <- approx(sali_preds, O2$Oxygen_deficit[id], xout = sali_change_point2)$y
  }

  # estimate model for 3rd segment
  O2 <- subset(O2, Depth > fit$depth_change_point2)

  # if not enough data return
  if (nrow(O2) < 3) {
    return(fit)
  }
  # recenter data to easily estimate the slope
  O2$Oxygen_deficit <- O2$Oxygen_deficit - fit$O2def_below_halocline
  O2$Depth <- O2$Depth - fit$depth_change_point2
  seg3_model <- lm(Oxygen_deficit ~ Depth - 1, data = O2)
  fit$O2def_slope_below_halocline <- pmax(0,coef(seg3_model))

  fit
}

if (interactive) {
  # inpect the results from one fit

  tail(sort(table(oxy$StationID)),10)
  #  450156  450151  450152  450153  450154  450148  450150  450155  450149 7563137
  #      64      70      81      82      83      85      85      85      87     105
  data <- subset(oxy, StationID == "7563137")
  data <- subset(oxy, StationID == "450149")
  fit <- doonefit(data)
  fit

  # predict salinty and oxygen across depth
  pdata <- data.frame(Depth = seq(0, 200, length = 100))
  pdata$fitted_salinity <- sali_profile(pdata$Depth, fit)
  pdata$fitted_oxygen <- oxy_profile(pdata$Depth, fit)

  # plot the salinity data and estimated salinity profile
  require(lattice)
  require(latticeExtra)
  p <-
    xyplot(Depth ~ Salinity, data = data,
         type = "p", cex = 0.5, strip = FALSE, ylim = c(200, -10), asp = 4, xlim = c(-1, 20),
         #scales = list(x = list(draw = FALSE)),
         ylab = "Depth (m)", xlab = "", col = "red") +
    xyplot(Depth ~ fitted_salinity, type = "l", data = pdata, col = "red") +
    xyplot(depth_change_point1 ~ sali_profile(depth_change_point1, fit), data = fit, col = "darkgreen", pch = 16) +
    xyplot(depth_change_point2 ~ sali_profile(depth_change_point2, fit), data = fit, col = "darkgreen", pch = 16) +
    xyplot(Depth ~ Oxygen_deficit, data = data, col = "blue") +
    xyplot(Depth ~ fitted_oxygen, type = "l", data = pdata, col = "blue")
  print(p)
}

# run all fits
profiles <-
  do.call(rbind,
          lapply(unique(oxy$StationID),
                 function(id) doonefit(subset(oxy, StationID == id))))

if (interactive) {
  # inspect
  str(profiles)
}


# ----------------------------
#
#  Inspect spatial distribution of halocline
#
# ----------------------------

require(mgcv)
gam2raster <- function(g, r) {
  # get x and y values
  e <- extent(r)
  rx <- seq(e[1]+xres(r)/2, e[2]-xres(r)/2, length = ncol(r))
  ry <- seq(e[4]-yres(r)/2, e[3]+yres(r)/2, length = nrow(r))

  # make sure and match x and y to unpacking order of raster values
  pred <-
    data.frame(Longitude = rep(rx, nrow(r)),
               Latitude = rep(ry, each = ncol(r)))
  mask <- which(!is.na(r[]))
  pred <- pred[mask,]
  names(r) <- "layer"

  r[] <- NA
  r[mask] <- predict(g, newdata = pred)
  r
}


# merge covariates to profiles
profiles <- dplyr::left_join(profiles, unique(oxy[c("StationID", "Latitude", "Longitude", "Year", "Month", "Assessment_Unit")]))

# make spatial ?
spprofiles <- profiles
sp::coordinates(spprofiles) <- ~ Longitude + Latitude
sp::proj4string(spprofiles) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

## fit surface to profile
# surface salinity
gsali_surf <- gam(log(sali_surf) ~ te(Longitude, Latitude, k = c(9, 9)),
                        data = profiles)
rsali_surf <- gam2raster(gsali_surf, bathy)

# salinity differences
gsali_dif <- gam(log(sali_dif) ~ te(Longitude, Latitude, k = c(9, 9)),
                       data = profiles)
rsali_dif <- gam2raster(gsali_dif, bathy)

# halocline depth
ghalocline <- gam(log(halocline) ~ te(Longitude, Latitude, k = c(9, 9)),
                        data = profiles)
rhalocline <- gam2raster(ghalocline, bathy)

# halocline gradient
gdepth_gradient <- gam(log(depth_gradient) ~ te(Longitude, Latitude, k = c(9, 9)),
                        data = profiles)
rdepth_gradient <- gam2raster(gdepth_gradient, bathy)

# oxygen debt
gO2def <- gam(O2def_below_halocline ~ te(Longitude, Latitude, k = c(5, 5)),
              data = profiles)
rO2def <- gam2raster(gO2def, bathy)

# oxygen declining rate
gO2def_slope <- gam(O2def_slope_below_halocline ~ te(Longitude, Latitude, k = c(5, 5)),
                       data = profiles)
rO2def_slope <- gam2raster(gO2def_slope, bathy)




if (interactive) {
  sp::plot(assessmentArea)
  sp::plot(spprofiles[!is.na(spprofiles$O2def_slope_below_halocline),], add=TRUE)

  # plot some fits
  cols <- gplots::rich.colors(255)
  fun <- exp
  plot(fun(rsali_surf), col = cols)
  plot(fun(rsali_dif), col = cols)
  plot(fun(rhalocline), col = cols)
  plot(bathy, col = grey(0.5), legend=FALSE)
  #max(profiles$depth_gradient, na.rm = TRUE)
  # [1] 59.31805
  plot(fun(rdepth_gradient), col = cols, zlim = c(0, 60), add = TRUE)
  plot(rO2def, col = cols)
  plot(bathy, col = grey(0.5), legend = FALSE)
  plot(rO2def, col = cols, zlim = c(0,10), add = TRUE)
  plot(rO2def_slope, col = cols)
}







