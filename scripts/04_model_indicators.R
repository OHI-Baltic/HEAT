# ----------------------------
#
#   Read in data
#
# ----------------------------

# load packages etc.
source("scripts/header.R")


# read profile fits -----------------------------

profiles <- read.csv("output/profiles.csv")

# keep only the profiles with oxgen debt at base of halocline
profiles <- profiles[!is.na(profiles$O2def_below_halocline),]
# only use profiles that are based on an estimate of the lower halocline estimate with +- 20m accuracy
profiles <- profiles[profiles$depth_change_point2.se < 10,]
# only use profiles that are based on an estimate of the salinity difference estimate with +- 10 accuracy
profiles <- profiles[profiles$sali_dif.se < 5,]

# set up useful covariates
profiles$date <- ymd(with(profiles, paste(Year, Month, Day, sep = "-")))
profiles$yday <- yday(profiles$date)

# keep only useful columns
profiles$y <- profiles$O2def_below_halocline
profiles <- profiles[c("ID", "y", "Assessment_Unit", "Year", "yday", "Latitude", "Longitude")]


#  Read in spatial data ----------------------------

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



# model oxygen debt ----------------------------

# the model
form <- y ~ te(Year, Latitude, Longitude, k = c(6, 25), d = c(1,2), m = c(1,2)) +         # spatial-temporal trend
            te(yday, Latitude, Longitude, k = c(6, 25), d = c(1,2), bs = c("cc", "tp"))   # spatial-seasonal trend

# temporary covars
profiles$fArea <- factor(profiles$Assessment_Unit)
profiles$fYear <- factor(profiles$Year)

# fit the model that is the basis of the indicator
g <- gam(form, data = profiles)

# predict over bathy extent and integrate over space and season for each assessment unit, given a year

# the indicator prediction function -------------------------------

getYearIndicator <- function(year, yday = seq(1, 365, length.out = 12), r = bathy) {

  # set up spatial extent to integrate over
  e <- extent(r)
  rx <- seq(e[1]+xres(r)/2, e[2]-xres(r)/2, length = ncol(r))
  ry <- seq(e[4]-yres(r)/2, e[3]+yres(r)/2, length = nrow(r))

  # make sure and match x and y to unpacking order of raster values
  pred <-
    data.frame(Longitude = rep(rx, nrow(r)),
               Latitude = rep(ry, each = ncol(r)),
               yday = 1,
               Year = 1)
  pred$mask <- !is.na(r[])
  # thin
  pred <- pred[seq(1, nrow(pred), by = 50),]
  pred <- pred[pred$mask,]
  # assign each point to an assessment unit
  p <- pred
  coordinates(p) <- ~ Longitude + Latitude
  proj4string(p) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  pred$Assessment_Unit <- over(p, assessmentArea)$Assessment_Unit
  pred <- pred[!is.na(pred$Assessment_Unit),]
  pred$fArea <- factor(pred$Assessment_Unit)

  # get all covars
  pred$Year <- year
  pred$fYear <- factor(pred$Year, levels = levels(profiles$fYear))

  pred2 <-
    do.call(rbind,
            lapply(yday,
                   function(i) within(pred, {yday = i})))

  # predict
  X <- predict(g, newdata = pred2, type = "lpmatrix")
  b <- coef(g)
  Vp <- g$Vp
  A <- model.matrix(~ Assessment_Unit - 1, data = pred2)
  colnames(A) <- gsub("Assessment_Unit", "", colnames(A))
  A <- sweep(A, 2, colSums(A), "/")
  tAX <- t(A) %*% X
  # mean(Xb) = t(A)Xb
  # var(Xb) = t(A)X Vp t(X)A
  Vax <- tAX %*% (Vp %*% t(tAX))
  list(mean = drop(t(t(A) %*% X %*% b)),
       se = sqrt(diag(Vax)))
}

# short function to sum up number of data points per asessmnt unit fot a given year
getN <- function(year) {
  c(table(
      factor(profiles$Assessment_Unit[profiles$Year == year],
           levels = sort(unique(profiles$Assessment_Unit)))))
}

# calculate indicator ---------------------------------------

# run over years
indicators <-
  do.call(rbind,
          lapply(sort(unique(profiles$Year)), function(y) {
            out <- getYearIndicator(y)
            N <- getN(y)[names(out$mean)] # make sure order is correct
            data.frame(Assessment_Unit = names(out$mean),
                       Period = y,
                       ES = out$mean,
                       STD = out$se,
                       N = N,
                       row.names = 1:length(N))
          }))

# write indicators -------------------------------------------

write.csv(file = "output/indicators.csv", indicators, row.names = FALSE)
