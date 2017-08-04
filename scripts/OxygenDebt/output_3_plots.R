# ----------------------------
#
#   plot data, models and fits
#
# ----------------------------

# load packages etc.
header("output")


# read data -------------------------------------------

oxy <- read.csv("analysis/input/OxygenDebt/oxy_clean.csv")
profiles <- read.csv("analysis/output/OxygenDebt/profiles.csv")

# read assessment unit shape file
helcom <- rgdal::readOGR("data/OxygenDebt/shapefiles", "helcom_areas", verbose = FALSE)

# make a spatial data.frame from oxy and profiles
spoxy <- makeSpatial(oxy)
spprofiles <- makeSpatial(profiles)

# read depth profile
bathy <- rgdal::readOGR("data/OxygenDebt/shapefiles", "helcom_bathymetry", verbose = FALSE)




# assessment map ------------------------------------------

pdf("analysis/output/OxygenDebt/spatial_plots.pdf", onefile = TRUE, paper = "a4")

  # plot regions with names
  sp::plot(helcom, col = gplots::rich.colors(nrow(helcom), alpha = 0.5), border = grey(0.4))
  text(sp::coordinates(helcom),
       labels = helcom$Basin,
       cex = 0.7)

  # plot regions with station locations
  sp::plot(helcom, col = gplots::rich.colors(nrow(helcom), alpha = 0.5))
  sp::plot(spprofiles, cex = 0.5, add = TRUE, pch = 1, col = "darkred")

  # plot bathymetry
  sp::plot(bathy, col = gplots::rich.colors(50, alpha=0.5)[cut(bathy$depth, 50)], pch = ".")
  sp::plot(helcom, border = "red", add = TRUE)

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

pdf(paste("analysis/output/OxygenDebt/data_plots.pdf"), onefile = TRUE, paper = "a4")

  for (i in sort(unique(oxy$Basin))) {
    par(mfrow = c(5, 4), mar = c(0,0,0,0), oma = c(1.5,0,0,0))
    data <- subset(oxy, Basin == i)

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

pdf(paste("analysis/output/OxygenDebt/model_plots.pdf"), onefile = TRUE, paper = "a4")

for (i in sort(unique(profiles$Basin))) {
  par(mfrow = c(5, 4), mar = c(0,0,0,0), oma = c(1.5,0,0,0))

  data <- subset(oxy, Basin == i)
  profs <- subset(profiles, Basin == i)

  # plot params by assessment unit
  ylim <- c(max(data$Depth) + 10, -10)
  xlim <- range(-1, data$Oxygen_deficit+1, data$Salinity+1, na.rm = TRUE)

  count <- 0
  for (j in unique(profs$ID)) {
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




# spatial model plots ------------------------------------------

# load gam predictions ('pars')
check <- load("analysis/output/OxygenDebt/gam_predictions.RData")
if (check != "surfaces" & FALSE) {
  warning("Error loading gam predictions!\n\tSkipping spatial plots for now.")
} else {
  rm(check)

  cols <- rev(viridis::magma(50))
  tmp <- makeSpatial(surfaces[surfaces$Year == 2015,])
  what <- c("halocline",
            "depth_change_point1", "depth_change_point2",
            "O2def_below_halocline", "O2def_slope_below_halocline",
            "oxygendebt")

  key <- function(vals) {
    xy <- c(49527.26, 6252697)
    xy_width <- c(3, 1)*1.5e4
    x <- sp::GridTopology(xy, xy_width, c(1,50))
    x <- sp::as.SpatialPolygons.GridTopology(x, proj4string = sp::CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    sp::plot(x, add = TRUE, border = NA, col = cols)
    text((sp::coordinates(x) + rep(c(xy_width[1]*2, 0), each = 50))[seq(1, 50, length = 8),],
         label = round(seq(min(vals), max(vals), length = 8), 2),
         cex = 0.5)
  }

  pdf(paste("analysis/output/OxygenDebt/spatial_model_plots.pdf"), onefile = TRUE, paper = "a4")
  for (i in what) {
    sp::plot(tmp, col = cols[cut(tmp[[i]], 50)], pch = ".", main = i)
    sp::plot(helcom, add = TRUE, border = "red")
    key(tmp[[i]])
  }
  dev.off()

}


if (FALSE) {
## DON'T RUN

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

