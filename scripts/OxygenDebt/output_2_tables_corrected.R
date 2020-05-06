# ----------------------------
#
#   Correct oxygen debt for
#
#     * load indicator surfaces
#
# ----------------------------

# load packages etc.
header("output")

# start timer
t0 <- proc.time()

# get assessment period
#config <- jsonlite::fromJSON("data/OxygenDebt/config.json")
config <- list()
config[["years"]] <- 2000:2019

# load gam predictions ('pars')
#check <- load("analysis/output/OxygenDebt/gam_predictions.RData")
check <- load("/mnt/data/ellie/bhi_share/BHI 2.0/Goals/CW/EUT/HEATOutput/analysis/output/OxygenDebt/gam_predictions.RData")
if (check != "surfaces") {
  stop("Error loading gam predictions!\n\tTry rerunning model_3_spatial_predictions.R")
}
rm(check)

# read in regression parameters
aux_info <- read.csv("data/OxygenDebt/auxilliary.csv")

# load baltic inflow data
# load baltic inflow data
mbi <- read.csv("data/OxygenDebt/MajorBalticInflows.csv")
mbi <- tapply(mbi$intensity, mbi$year_end, sum)
mbi <- data.frame(year = as.numeric(names(mbi)),
                  mbi = c(unname(mbi)))

# add in Ninput
# load nutrient data
Ninput <- read.csv("data/OxygenDebt/nitrogen.csv")
Ninput <- dplyr::rename(Ninput, year = Year)

make_datnew <- function(Basin) {
  
  # aggregate spatial estimates to basin level:
  summarise <- function(x, func = mean, Basin = "Baltic Proper") {
    filt <- surfaces$Basin == Basin
    bylist <- surfaces$Year[filt]
    c(tapply(x[filt], bylist, func, na.rm = TRUE))
  }
  
  datnew <-
    data.frame(
      year = unique(surfaces$Year),
      O2rate_below = summarise(surfaces$O2def_slope_below_halocline, Basin = Basin),
      O2_lower = NA,
      O2def_lower = summarise(surfaces$O2def_below_halocline, Basin = Basin),
      sali_dif = summarise(surfaces$sali_dif, Basin = Basin),
      bot_temp = NA,
      sali_surf = summarise(surfaces$sali_surf, Basin = Basin),
      depth_gradient = summarise(surfaces$depth_gradient, Basin = Basin),
      halocline = summarise(surfaces$halocline, Basin = Basin),
      Lhalo_volume = summarise(surfaces$lhalo, sum, Basin = Basin)/1000,
      uhalo_volume = summarise(surfaces$uhalo, sum, Basin = Basin)/1000,
      bottom_volume = summarise(surfaces$bottom, sum, Basin = Basin)/1000,
      surface_volume = summarise(surfaces$surface, sum, Basin = Basin)/1000,
      Lhalo_salinity = summarise(surfaces$lhalo_salinity, sum, Basin = Basin)/1000000,
      uhalo_salinity = summarise(surfaces$uhalo_salinity, sum, Basin = Basin)/1000000,
      bottom_salinity = summarise(surfaces$bottom_salinity, sum, Basin = Basin)/1000000,
      surface_salinity = summarise(surfaces$surface_salinity, sum, Basin = Basin)/1000000,
      Lhalo_O2debt = summarise(surfaces$lhalo_O2debt, sum, Basin = Basin)/1000000,
      uhalo_O2debt = summarise(surfaces$uhalo_O2debt, sum, Basin = Basin)/1000000,
      bottom_O2debt = summarise(surfaces$bottom_O2debt, sum, Basin = Basin)/1000000,
      surface_O2debt = summarise(surfaces$surface_O2debt, sum, Basin = Basin)/1000000,
      Lhalo_area = NA,
      uhalo_area = NA,
      bottom_area = NA,
      surface_area = NA,
      O2debt = NA,
      hypoxic_volume = NA,
      cod_rep_volume = NA,
      hypoxic_area = NA
  )

  # brunt vaisala approx
  # this is the one used in PNAS 2014 paper:
  #surfaces$Nbv <- sqrt( 9.8 / (1 + 0.0008 * (surfaces$sali_surf + surfaces$sali_dif/2)) *
  #                      0.68 * 0.0008 * surfaces$sali_dif / (2*surfaces$depth_gradient))
  # this is the one used in TARGREV:
  datnew$Nbv <- sqrt( 9.8 / (1 + 0.001 * (datnew$sali_surf + datnew$sali_dif/2)) *
                      0.001 * datnew$sali_dif / (2*datnew$depth_gradient))

  # calculate O2debt
  datnew$O2debt <- (datnew$Lhalo_O2debt + datnew$bottom_O2debt)

  # volume specific O2 debt * 1000 t get in mg pr litre
  datnew$O2debt_volsp <- datnew$O2debt / (datnew$Lhalo_volume + datnew$bottom_volume) * 1000

  # major baltic inflows
  datnew <- dplyr::left_join(datnew, mbi)
  datnew$mbi[is.na(datnew$mbi)] <- 0

  # nitrogen input
  if (Basin == "Baltic Proper") {
    datnew <- dplyr::left_join(datnew, dplyr::rename(Ninput, Ninput = BAP)[c("year", "Ninput")])
    datnew$offset <- 6.895179
    datnew$bvmean <- 0.0350
  } else {
    datnew <- dplyr::left_join(datnew, dplyr::rename(Ninput, Ninput = BOB)[c("year", "Ninput")])
    datnew$offset <- 3.893
    datnew$bvmean <- 0.0696
  }
  # subset to assessment period
  datnew <- datnew[datnew$year %in% config$years,]

  # return
  datnew
}


get_aux <- function(Basin) {
  aux_info[aux_info$Basin == Basin,]
}




# create table of indicators by year and by year and basin

#basins <- unique(surfaces$Basin)
basins <- c("Baltic Proper", "Bornholm Basin")
#Basin <- "Bornholm Basin"


ES_y <-
  sapply(basins,
         function(Basin) {
           newdata <- make_datnew(Basin)
           aux <- get_aux(Basin)

           o2N <-
             cbind(dplyr::select(newdata, year),
                   mbi = -1 * newdata$mbi,
                   bv = newdata$Nbv,
                   N = newdata$Ninput / (newdata$Lhalo_volume + newdata$bottom_volume) / 50,
                   sal = -1 * (newdata$Lhalo_salinity + newdata$bottom_salinity) / (newdata$Lhalo_volume + newdata$bottom_volume) * 1000,
                   o2 = (newdata$Lhalo_O2debt + newdata$bottom_O2debt) / (newdata$Lhalo_volume + newdata$bottom_volume) * 1000
             )

           X <- with(o2N, cbind(bv, N*bv, mbi*bv, sal*bv))
           pred <- c(X %*% unlist(aux[,c("a_0", "a_N", "a_MBI", "a_salinity")]))
           res <- o2N$o2 - pred

           (with(o2N, N*bv) *  aux$a_N + res)/o2N$bv * newdata$bvmean + newdata$offset
         })
ES_y <- t(ES_y)
colnames(ES_y) <- config$years
names(dimnames(ES_y)) <- c("Basin", "Year")
out_y <- do.call(expand.grid, c(dimnames(ES_y), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))

ES <- rowMeans(ES_y)
out <- do.call(expand.grid, c(list(Basin = names(ES)), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))

# add indicator estimate
out_y$ES <- c(ES_y)
out$ES <- c(ES)

# add indicator SD - approximate as a 20% cv
ES_SD_y <- out_y$ES * 0.2
out_y$ES_SD <- c(ES_SD_y)

ES_SD <- apply(ES_y, 1, sd)
out$ES_SD <- c(ES_SD)

# read profile fits
profiles <- read.csv("/mnt/data/ellie/bhi_share/BHI 2.0/Goals/CW/EUT/HEATOutput/analysis/output/OxygenDebt/profiles.csv")

# add in auxilliary information
ES_N_y <- with(profiles, tapply(O2def_slope_below_halocline, list(Basin, Year), function(x) sum(!is.na(x))))
out_y$ES_N <- c(ES_N_y[basins,])

ES_N <- with(profiles, tapply(O2def_slope_below_halocline, list(Basin), function(x) sum(!is.na(x))))
out$ES_N <- c(ES_N[basins])

# now expand to AssessmentUnitID
library(sp)
helcom <- rgdal::readOGR("data/OxygenDebt/shapefiles", "helcom_areas", verbose = FALSE)
helcom <- helcom[helcom$Basin %in% out$Basin,]
SEA <- rgdal::readOGR("/mnt/data/ellie/bhi_share/BHI 2.0/Goals/CW/EUT/HEATShapefiles/AssessmentUnit_20112016", "AssessmentUnit_20112016Polygon", verbose = FALSE)
SEA <- SEA[grep("SEA", SEA$Code),]
SEA <- spTransform(SEA, CRS(proj4string(helcom)))

if (FALSE) {
  plot(helcom, col = gplots::rich.colors(nrow(helcom), alpha = 0.5), border = NA)
  plot(SEA, border = "red", lwd = 2, add = TRUE)
  text(coordinates(SEA), labels = SEA$Code, cex = 0.6, col = "red")
  text(coordinates(helcom), labels = helcom$Basin, cex = 0.6)
}

lookup <- as.data.frame(SEA)
names(lookup) <- c("AssessmentUnitID", "AssessmentUnitName")
lookup$AssessmentUnitID <- as.integer(gsub("SEA-", "", lookup$AssessmentUnitID))
lookup$Basin <- c(NA_character_,     # 001,
                  NA_character_,     # 002,
                  NA_character_,     # 003,
                  NA_character_,     # 004,
                  NA_character_,     # 005,
                  "Arkona Basin",     # 006
                  "Bornholm Basin",  # 007
                  "Baltic Proper",   # 008
                  "Baltic Proper",   # 009
                  "Baltic Proper",   # 010
                  NA_character_,     # 011
                  "Baltic Proper",   # 012
                  "Baltic Proper",   # 013
                  "Bothnian Sea",    # 014
                  "Bothnian Sea",    # 015
                  NA_character_,     # 016
                  "Bothnian Bay")    # 017

# convert SEA=001 to 1

out <- dplyr::right_join(lookup, out, by = "Basin")
out_y <- dplyr::right_join(lookup, out_y, by = "Basin")

out <- out[order(out$AssessmentUnitID),]
out_y <- out_y[order(out_y$AssessmentUnitID, out_y$Year),]

# inspect
if (FALSE) {
  out
  out_y
}


# write out
write.csv(out, file = "/mnt/data/ellie/bhi_share/BHI 2.0/Goals/CW/EUT/HEATOutput/analysis/output/OxygenDebt/corrected_indicator_table.csv", row.names = FALSE)
write.csv(out_y, file = "/mnt/data/ellie/bhi_share/BHI 2.0/Goals/CW/EUT/HEATOutput/analysis/output/OxygenDebt/corrected_indicator_table_by_year.csv", row.names = FALSE)

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))
