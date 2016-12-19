# ----------------------------
#
#   plot data, models and fits
#
# ----------------------------

if (FALSE) {

# load packages etc.
source("scripts/OxygenDebt/header.R")

# make sure figures folder exists
dir.create("figures")

# read data -------------------------------------------

oxy <- read.csv("model/input.csv")
profiles <- read.csv("output/profiles.csv")

# read assessment unit shape file
assessmentArea <- rgdal::readOGR("input", "AssessmentUnit_20112016Polygon")
names(assessmentArea) <- c("Assessment_Unit", "Basin")
# keep only SEA units ?
assessmentArea <- assessmentArea[grepl("SEA-", assessmentArea$Assessment_Unit),]

# make a spatial data.frame from oxy
#makeSpatialOxy <- function(x) {
#  x <- unique(x[c("StationID", "Latitude", "Longitude")])
#  coordinates(x) <- ~ Longitude + Latitude
#  proj4string(x) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#  x
#}
#spoxy <- makeSpatialOxy(oxy)

# read depth profile
#bathy <- raster::raster("input/GMT_GEBCO_08_BalticSea.asc")
#sp::proj4string(bathy) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# trim to extent of assessment units
#bathy <- raster::mask(bathy, assessmentArea)


# assessment map ------------------------------------------

pdf("figures/assessment_areas.pdf", onefile = TRUE, paper = "a4")
  # plot regions with names
  plot(assessmentArea, col = gplots::rich.colors(17, alpha = 0.5), border = grey(0.4))
  text(sp::coordinates(assessmentArea),
       labels = paste(assessmentArea$Assessment_Unit, assessmentArea$Basin, sep = "\n"),
       cex = 0.4)

  # plot regions with station locations
  #plot(assessmentArea, col = gplots::rich.colors(17, alpha = 0.5))
  #plot(spoxy, cex = 0.5, add = TRUE, pch = 1)

  # plot bathymetry
  #plot(bathy, col = colorRampPalette(c("darkblue", "lightblue"))(255))
dev.off()





# data and model plots ------------------------------------------

# functions ----------------

plot_data <- function(wk, xlim, ylim, col = c("red", "blue")) {
  plot(wk$Salinity, wk$Depth, col = col[1],
       cex = ifelse(wk$Depth < 20, 0.3, 1), # highlight data used in fit
       xlim = xlim, ylim = ylim,
       ann = FALSE, axes = FALSE
  )
  points(wk$Oxygen_deficit, wk$Depth, col = col[2])
  text(xlim[2] - range(xlim)*.1, ylim[2] - range(ylim)*.1, labels = j, adj = 1)
  box(col = grey(0.7))
}

plot_model <- function(wk, fit, xlim, ylim) {
  plot_data(wk, xlim, ylim,
            col = paste0(colorRampPalette(c("red", "blue"))(2), "44"))

  # predict
  pdata <- data.frame(Depth = seq(20, ylim[1], length = 100))
  pdata$fitted_salinity <- sali_profile(pdata$Depth, fit)
  pdata$fitted_oxygen <- oxy_profile(pdata$Depth, fit)

  lines(pdata$fitted_salinity, pdata$Depth, col = "red", lwd = 2)
  lines(pdata$fitted_oxygen, pdata$Depth, col = "blue", lwd = 2)
}

# data plots -----------------------

pdf(paste("figures/data_plots.pdf"), onefile = TRUE, paper = "a4")

  for (i in sort(unique(oxy$Assessment_Unit))) {
    par(mfrow = c(5, 4), mar = c(0,0,0,0), oma = c(1.5,0,0,0))
    data <- subset(oxy, Assessment_Unit == i)

    # plot params by assessment unit
    ylim <- c(max(data$Depth) + 10, -10)
    xlim <- range(-1, data$Oxygen_deficit+1, data$Salinity+1, na.rm = TRUE)

    count <- 0
    for (j in unique(data$ID)) {
      wk <- subset(data, ID == j)
      # plotting
      plot_data(wk, xlim, ylim)
      # titles
      count <- count+1
      if ((count %% prod(par("mfrow"))) == 1) {
        mtext(i, side = 1, outer = TRUE, line = 0.5)
      }
    }
  }
dev.off()

# model plots -----------------------

pdf(paste("figures/model_plots.pdf"), onefile = TRUE, paper = "a4")

for (i in sort(unique(oxy$Assessment_Unit))) {
  par(mfrow = c(5, 4), mar = c(0,0,0,0), oma = c(1.5,0,0,0))

  data <- subset(oxy, Assessment_Unit == i)
  profs <- subset(profiles, Assessment_Unit == i)

  # plot params by assessment unit
  ylim <- c(max(data$Depth) + 10, -10)
  xlim <- range(-1, data$Oxygen_deficit+1, data$Salinity+1, na.rm = TRUE)

  count <- 0
  for (j in unique(data$ID)) {
    wk <- subset(data, ID == j)
    fit <- subset(profs, ID == j)
    # plotting
    plot_model(wk, fit, xlim, ylim)
    # titles
    count <- count+1
    if ((count %% prod(par("mfrow"))) == 1) {
      mtext(i, side = 1, outer = TRUE, line = 0.5)
    }
  }
}
dev.off()

# reliable model plots -----------------------

# select which data is reliable
profiles <- profiles[!is.na(profiles$depth_change_point2.se),]
# only use profiles that are based on an estimate of the lower halocline estimate with +- 20m accuracy
profiles <- profiles[profiles$depth_change_point2.se < 10,]
# only use profiles that are based on an estimate of the salinity difference estimate with +- 10 accuracy
profiles <- profiles[profiles$sali_dif.se < 5,]

pdf(paste("figures/reliable_model_plots.pdf"), onefile = TRUE, paper = "a4")

oxy <- oxy[oxy$ID %in% profiles$ID,]
for (i in sort(unique(oxy$Assessment_Unit))) {
  par(mfrow = c(5, 4), mar = c(0,0,0,0), oma = c(1.5,0,0,0))

  data <- subset(oxy, Assessment_Unit == i)
  profs <- subset(profiles, Assessment_Unit == i)

  # plot params by assessment unit
  ylim <- c(max(data$Depth) + 10, -10)
  xlim <- range(-1, data$Oxygen_deficit+1, data$Salinity+1, na.rm = TRUE)

  count <- 0
  for (j in unique(data$ID)) {
    wk <- subset(data, ID == j)
    fit <- subset(profs, ID == j)
    # plotting
    plot_model(wk, fit, xlim, ylim)
    # titles
    count <- count+1
    if ((count %% prod(par("mfrow"))) == 1) {
      mtext(i, side = 1, outer = TRUE, line = 0.5)
    }
  }
}
dev.off()

# reload oxy and profiles
oxy <- read.csv("model/input.csv")
profiles <- read.csv("output/profiles.csv")


# spatial model plots ------------------------------------------

cols <- gplots::rich.colors(255)
rast_fnames <- dir("output", pattern = ".tif")

pdf(paste("figures/spatial_model_plots.pdf"), onefile = FALSE, paper = "a4")
  for (i in rast_fnames) {
    x <- raster(paste0("output/", i))
    plot(x, col = cols, main = i)
    plot(assessmentArea, add = TRUE)
  }
dev.off()
rm(x)


# indicator plots ------------------------------------------

indicators <- read.csv("output/indicators.csv")
# merge in target
aux <- read.dbexport("input/auxilliary.txt")
indicators <- left_join(indicators, aux[c("Assessment_Unit", "ET")])

plot_indicator <- function(wk, xlim, ylim, main) {
  plot(wk$Period, wk$ES,
       xlim = xlim, ylim = ylim,
       ann = FALSE, axes = FALSE,
       type = "p", pch = 16
  )
  arrows(wk$Period, y0 = wk$ES - wk$STD*2, y1 = wk$ES + wk$STD*2, angle = 90, code = 3, length = 0.1)
  text(xlim[2] - diff(range(xlim))*.02, ylim[2] - diff(range(ylim))*.02, labels = main, adj = 1)
  box(col = grey(0.7))
  #axis(1)
  axis(2, las = 2, col = grey(0.7))
  abline(h = wk$ET[1], col = "red")
}

# data plots -----------------------

pdf(paste("figures/indicator_plots.pdf"), onefile = TRUE, paper = "a4")

par(mfrow = c(3, 3), mar = c(1,3,0,0), oma = c(1.5,0,0,0))

ylim <- range(indicators$ES - 2*indicators$STD, indicators$ES + 2*indicators$STD)
xlim <- range(indicators$Period) + 0.5*c(-1,1)

for (i in unique(indicators$Assessment_Unit)) {
  wk <- subset(indicators, Assessment_Unit == i)
  # plotting
  plot_indicator(wk, xlim, ylim, main = i)
}

par(mfrow = c(3, 3), mar = c(1,3,0,0), oma = c(1.5,0,0,0))

ylim <- range(0, 15)
xlim <- range(indicators$Period) + 0.5*c(-1,1)

for (i in unique(indicators$Assessment_Unit)) {
  wk <- subset(indicators, Assessment_Unit == i)
  # plotting
  plot_indicator(wk, xlim, ylim, main = i)
}

dev.off()


}

