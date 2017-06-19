
# load gam predictions ('surfaces')
check <- load("analysis/output/OxygenDebt/gam_predictions.RData")
if (check != "surfaces") {
  stop("Error loading gam predictions!\n\tTry rerunning model_3_spatial_predictions.R")
}
rm(check)



# read in data from previous assessment
dat <- read.csv("../Jacob_data/basin_trends_Carstensen_baltic_proper.csv")
dat$O2debt <- dat$O2debt/1000000
dat$Lhalo_salinity <- dat$Lhalo_salinity/1000000
dat$uhalo_salinity <- dat$uhalo_salinity/1000000
dat$bottom_salinity <- dat$bottom_salinity/1000000
dat$surface_salinity <- dat$surface_salinity/1000000

# read in regression parameters
aux <- read.csv("data/OxygenDebt/auxilliary.csv")

# load nutrient data
Ninput <- read.csv("data/OxygenDebt/nitrogen.csv", row.names = 1)
Ninput <- Ninput[,"BP"]/1000
names(Ninput) <- 1995:2015

# load baltic inflow data
mbi <- read.csv("data/OxygenDebt/MajorBalticInflows.csv")
mbi_years <- min(mbi$year_end, na.rm = TRUE):max(mbi$year_end, na.rm = TRUE)
mbi <- mbi[mbi$year_end %in% mbi_years,]
mbi <- tapply(mbi$intensity, factor(mbi$year_end, levels = mbi_years), sum)
mbi[is.na(mbi)] <- 0

# calculate BV freq
dat$Nbv <- sqrt( 9.8 / (1 + 0.001 * (dat$sali_surf + dat$sali_dif/2)) *
                      0.001 * dat$sali_dif / (2*dat$depth_gradient))


# aggregate spatial estimates to basin level:
summarise <- function(x, func = mean) {
  filt <- surfaces$Basin == "Baltic Proper"
  bylist <- surfaces$Year[filt]
  c(tapply(x[filt], bylist, func, na.rm = TRUE))
}

datnew <-
  data.frame(
    year = unique(surfaces$Year),
    O2rate_below = summarise(surfaces$O2def_slope_below_halocline),
    O2_lower = NA,
    O2def_lower = summarise(surfaces$O2def_below_halocline),
    sali_dif = summarise(surfaces$sali_dif),
    bot_temp = NA,
    sali_surf = summarise(surfaces$sali_surf),
    depth_gradient = summarise(surfaces$depth_gradient),
    halocline = summarise(surfaces$halocline),
    Lhalo_volume = summarise(surfaces$lhalo, sum)/1000,
    uhalo_volume = summarise(surfaces$uhalo, sum)/1000,
    bottom_volume = summarise(surfaces$bottom, sum)/1000,
    surface_volume = summarise(surfaces$surface, sum)/1000,
    Lhalo_salinity = summarise(surfaces$lhalo_salinity, sum)/1000000,
    uhalo_salinity = summarise(surfaces$uhalo_salinity, sum)/1000000,
    bottom_salinity = summarise(surfaces$bottom_salinity, sum)/1000000,
    surface_salinity = summarise(surfaces$surface_salinity, sum)/1000000,
    Lhalo_O2debt = summarise(surfaces$lhalo_O2debt, sum)/1000000,
    uhalo_O2debt = summarise(surfaces$uhalo_O2debt, sum)/1000000,
    bottom_O2debt = summarise(surfaces$bottom_O2debt, sum)/1000000,
    surface_O2debt = summarise(surfaces$surface_O2debt, sum)/1000000,
    Lhalo_area = NA,
    uhalo_area = NA,
    bottom_area = NA,
    surface_area = NA,
    O2debt = NA,
    hypoxic_volume = NA,
    cod_rep_volume = NA,
    hypoxic_area = NA
  )

dat$Nbv <- sqrt( 9.8 / (1 + 0.001 * (dat$sali_surf + dat$sali_dif/2)) *
                      0.001 * dat$sali_dif / (2*dat$depth_gradient))

datnew$Nbv <- sqrt( 9.8 / (1 + 0.001 * (datnew$sali_surf + datnew$sali_dif/2)) *
                      0.001 * datnew$sali_dif / (2*datnew$depth_gradient))

datnew$O2debt <- (datnew$Lhalo_O2debt + datnew$bottom_O2debt)

dat$O2debt <- dat$bottom_O2debt + dat$Lhalo_O2debt
# volume specific O2 debt * 1000 t get in mg pr litre
dat$O2debt_volsp <- dat$O2debt / (dat$Lhalo_volume + dat$bottom_volume) * 1000

datnew$O2debt_volsp <- datnew$O2debt / (datnew$Lhalo_volume + datnew$bottom_volume) * 1000



what <- "sali_surf"
what <- "surface_salinity"
#what <- "O2rate_below"
#what <- "depth_gradient"
#what <- "halocline"
what <- "O2def_lower"
what <- "Lhalo_volume"
what <- "surface_volume"
#what <- "bottom_volume"
what <- "Lhalo_salinity"
what <- "surface_salinity"
what <- "uhalo_O2debt"
what <- "surface_O2debt"
what <- "O2debt"
what <- "O2debt_volsp"
what <- "Nbv"
plot(dat$year, dat[[what]], pch = 16,
     xlim = c(1900, 2016),
     ylim = range(dat[[what]], datnew[[what]], na.rm = TRUE),
     main = what)
points(datnew$year, datnew[[what]], pch = 17, col = "red")


# correct for anthropogenic

dat$mbi <- NA
dat$mbi[dat$year %in% 1900:2012] <- mbi[paste(1900:2012)]

dat$Ninput <- NA
dat$Ninput[dat$year %in% 1995:2012] <- Ninput[paste(1995:2012)]
dat$Ninput[dat$year %in% 1900:1994] <- seq(100, 500, length.out = 95)

# investigate model at 1900
attach(auxbp)

par(mfrow = c(2,2))
dat$oxy_loss <- a_0 +
            a_N * dat$Ninput / (dat$Lhalo_volume + dat$bottom_volume)
plot(dat$year, dat$oxy_loss)

dat$oxy_gain <- a_MBI * dat$mbi +
            a_salinity * (dat$bottom_salinity + dat$Lhalo_salinity)/10 +
            dat$O2debt_volsp / dat$Nbv
plot(dat$year, dat$oxy_gain)

plot(dat$year, dat$oxy_gain - dat$oxy_loss)
abline(h = 0, col = "red")


# plot of covariates
par(mfrow = c(1,1))
#par(mfrow = c(2,2), mar = c(0,2,0,0) + 1)
#plot(dat$year, O2debt_volsp, main = "Nitrogen")
plot(dat$year, dat$O2debt_volsp, main = "O2 debt")
plot(dat$year, (dat$bottom_salinity + dat$Lhalo_salinity), main = "Salinity")

dat$O2debt_pred <-
  a_0 * dat$Nbv +
    a_N * dat$Ninput * 1000 / (dat$Lhalo_volume + dat$bottom_volume) * dat$Nbv -
    a_MBI * dat$mbi * dat$Nbv -
    a_salinity * (dat$bottom_salinity + dat$Lhalo_salinity) * dat$Nbv

dat$O2debt_pred_mix <-
  a_0 * dat$Nbv -
    a_MBI * dat$mbi * dat$Nbv -
    a_salinity * (dat$bottom_salinity + dat$Lhalo_salinity) * dat$Nbv

dat$O2debt_pred_mix / dat$O2debt_pred


# uncorrected oxygen debt
x <- dat$year[dat$year %in% 1900:2012]
y <- dat$O2debt_volsp[dat$year %in% 1900:2012]
smoothy <-
  sapply(seq_along(y),
         function(i) {
           if (is.na(y[i])) return(NA)
           id <- -2:2 + i;
           id <- id[id > 0];
           mean(y[id], na.rm = TRUE)
         })
plot(x, y,
     main = "Uncorrected",
     pch = 16, col = "darkblue", cex = 1.2,
     ylab = "", xaxs = "i", yaxs = "i", xpd = NA,
     ylim = c(5, 12), xlim = c(1900, 2012), las = 1)
lines(x, smoothy, lwd = 3)

mean(y[x %in% 2006:2010])

# corrected oxygen debt - what I think it should be
x <- dat$year[dat$year %in% 1900:2012]
y <- dat$O2debt_volsp - a_0 * dat$Nbv -
       #a_MBI * dat$mbi * dat$Nbv -
       #a_salinity * (dat$bottom_salinity + dat$Lhalo_salinity)/10 * dat$Nbv
y <- y[dat$year %in% 1900:2012]
smoothy <-
  sapply(seq_along(y),
         function(i) {
           if (is.na(y[i])) return(NA)
           id <- -2:2 + i;
           id <- id[id > 0];
           mean(y[id], na.rm = TRUE)
         })
plot(x, y,
     main = "Corrected",
     pch = 16, col = "darkblue", cex = 1.2,
     ylab = "", xaxs = "i", yaxs = "i", xpd = NA,
     #ylim = c(5, 12),
     xlim = c(1900, 2012), las = 1)
lines(x, smoothy, lwd = 3)


# corrected oxygen debt - another try ...
x <- dat$year[dat$year %in% 1900:2012]
y <- dat$O2debt_volsp -
       a_MBI * dat$mbi * dat$Nbv -
       a_salinity * (dat$bottom_salinity + dat$Lhalo_salinity)/10 * dat$Nbv
y <- y[dat$year %in% 1900:2012]
smoothy <-
  sapply(seq_along(y),
         function(i) {
           if (is.na(y[i])) return(NA)
           id <- -2:2 + i;
           id <- id[id > 0];
           mean(y[id], na.rm = TRUE)
         })
plot(x, y,
     main = "Corrected",
     pch = 16, col = "darkblue", cex = 1.2,
     ylab = "", xaxs = "i", yaxs = "i", xpd = NA,
     #ylim = c(5, 12),
     xlim = c(1900, 2012), las = 1)
lines(x, smoothy, lwd = 3)






d3 <- data.frame(y = dat$O2debt_volsp / dat$Nbv,
                 N = dat$Ninput / (dat$Lhalo_volume + dat$bottom_volume) * 10,
                 S = dat$bottom_salinity + dat$Lhalo_salinity,
                 mbi = dat$mbi*10)
summary(m1 <- lm(y ~ N + S + mbi, data = d3))

plot(residuals(m1) * dat$Nbv)

plot(dat$year)


plot(datayears, O2debt_pred/20)
points(dat$year, dat$O2debt_volsp, pch = 16, col = "blue")

if (FALSE) {


plot(dat$year, dat$Nbv,
     pch = 16, col = "blue", las = 1)





plot(dat$year, dat$O2debt/1e6,
     pch = 16, col = "blue", las = 1)

# maybe lower halocline should not be included...?
plot(dat$year, dat$hypoxic_area,
     pch = 16, col = "blue", las = 1)

# Hypoxic volume
plot(dat$year, dat$Lhalo_volume + dat$bottom_volume,
     pch = 16, col = "blue", las = 1, type = "l",
     ylim = c(0, max(dat$Lhalo_volume + dat$bottom_volume, na.rm = TRUE)))

points(dat$year, dat$hypoxic_volume,
     pch = 16, col = "blue")

# integrated salinity
plot(dat$year, dat$bottom_salinity + dat$Lhalo_salinity,
     pch = 16, col = "blue", las = 1,
     #ylim = range(dat$bottom_salinity, dat$surface_salinity + dat$uhalo_salinity, na.rm = TRUE))
     ylim = c(15, 75))

points(dat$year, dat$surface_salinity + dat$uhalo_salinity,
     pch = 16, col = "blue")


# volume specific debt
plot(dat$year, dat$O2debt / (dat$Lhalo_volume + dat$bottom_volume) * 1000,
     pch = 16, col = "blue", las = 1)

plot(dat$year, dat$O2debt / (dat$Lhalo_volume + dat$bottom_volume) / 1e3,
     pch = 16, col = "blue", las = 1)


# ----------------------------------
# plot old and new data
# ----------------------------------

dat <- read.csv("../Jacob_data/basin_trends_Carstensen_baltic_proper.csv")

plot(dat$year, dat$O2debt/1e6,
     pch = 16, col = "blue",
     las = 1,
     ylab = "Oxygen debt", xlab = "Year")



}
