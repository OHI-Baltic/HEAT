# Susa: Here the major inflows are used
# I hope this doesn't mean that they are incl in the calc.
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

## get assessment period
# config <- jsonlite::fromJSON("data/OxygenDebt/config.json")
## we don't have config.json, 
## config is created in calculate_oxygen_debt.R
## 00_header.R was edited to keep it in the environment through calculations


# load gam predictions ('pars')
if(file.exists(file.path(dirmain, "analysis/output/OxygenDebt/gam_predictions.RData"))){
  check <- load("analysis/output/OxygenDebt/gam_predictions.RData")
} else {
  check <- load(file.path(bhiremote,  "HEATOutput", "analysis", "output", "OxygenDebt", "gam_predictions.RData"))
}
if (check != "surfaces") {
  stop("Error loading gam predictions!\n\tTry rerunning model_3_spatial_predictions.R")
}
rm(check)

# read in regression parameters
aux_info <- read.csv("data/OxygenDebt/auxilliary.csv")

# load baltic inflow data
# mbi <- read.csv("data/OxygenDebt/MajorBalticInflows.csv")
# mbi_years <- min(mbi$year_end, na.rm = TRUE):max(mbi$year_end, na.rm = TRUE)
# mbi <- mbi[mbi$year_end %in% mbi_years,]
# mbi <- tapply(mbi$intensity, factor(mbi$year_end, levels = mbi_years), sum)
# mbi[is.na(mbi)] <- 0


make_datnew <- function(Basin) {
  # Is this where the information is aggregated to Basin level?
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
      hypoxic_area = NA,
      # mbi = 0,
      Ninput = NA
    )

  # brunt vaisala approx
  # this is the one used in PNAS 2014 paper:
  # surfaces$Nbv <- sqrt( 9.8 / (1 + 0.0008 * (surfaces$sali_surf + surfaces$sali_dif/2)) *
  #                      0.68 * 0.0008 * surfaces$sali_dif / (2*surfaces$depth_gradient))
  # this is the one used in TARGREV:
  datnew$Nbv <- sqrt( 9.8 / (1 + 0.001 * (datnew$sali_surf + datnew$sali_dif/2)) *
                      0.001 * datnew$sali_dif / (2*datnew$depth_gradient))

  # calculate O2debt
  datnew$O2debt <- (datnew$Lhalo_O2debt + datnew$bottom_O2debt)

  # volume specific O2 debt * 1000 t get in mg pr litre
  datnew$O2debt_volsp <- datnew$O2debt / (datnew$Lhalo_volume + datnew$bottom_volume) * 1000

  # major baltic inflows
  # datnew$mbi[datnew$year %in% as.numeric(names(mbi))] <- mbi[as.numeric(names(mbi)) %in% datnew$year]

  # subset to assessment period
  datnew <- datnew[datnew$year %in% config$years,]

  # return
  datnew
}


get_aux <- function(Basin) {
  aux_info[aux_info$Basin == Basin,]
}



if (FALSE) {
  newdata <- make_datnew("Baltic Proper")

  # uncorrected oxygen debt
  x <- newdata$year
  y <- newdata$O2debt_volsp
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
       ylab = "", las = 1)
  lines(x, smoothy, lwd = 3)
}

# if (FALSE) {
#   newdata <- make_datnew("Baltic Proper")
#   aux <- get_aux("Baltic Proper")
# 
#   # corrected oxygen debt - what I think it should be
#   x <- newdata$year
#   y <- newdata$O2debt_volsp -
#          aux$a_MBI * newdata$mbi * newdata$Nbv -
#          aux$a_salinity * (newdata$bottom_salinity + newdata$Lhalo_salinity)/10 * newdata$Nbv
# 
#   plot(x, y,
#        main = "Corrected",
#        pch = 16, col = "darkblue", cex = 1.2,
#        ylab = "Volume specific O2 debt", las = 1)
# 
#   mean(y)
# }

# create table of indicators by year and by year and basin

basins <- unique(surfaces$Basin)

ES_y <-
  sapply(basins,
         function(Basin) {
           newdata <- make_datnew(Basin) # here an issue
           aux <- get_aux(Basin)
           newdata$O2debt_volsp
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
profiles <- read.csv("analysis/output/OxygenDebt/profiles.csv")


# add in auxilliary information
ES_N_y <- with(profiles, tapply(O2def_slope_below_halocline, list(Basin, Year), function(x) sum(!is.na(x))))
out_y$ES_N <- c(ES_N_y[basins,])

ES_N <- with(profiles, tapply(O2def_slope_below_halocline, list(Basin), function(x) sum(!is.na(x))))
out$ES_N <- c(ES_N[basins])

# now expand to AssessmentUnitID
library(sp)
helcom <- rgdal::readOGR("data/OxygenDebt/shapefiles", "helcom_areas", verbose = FALSE)
helcom <- helcom[helcom$Basin %in% out$Basin,]
# read AssessmentUnit polygons
if(!any(grepl(list.files("data/OxygenDebt/shapefiles"), pattern = "AssessmentUnit_20112016"))){
  ## if assessment polygons shapefile does not exist locally
  ## e.g. if working on gunvor and never copied to or created bathy in data/OxygenDebt/shapefiles folder
  SEA <- rgdal::readOGR(file.path(bhiremote, "HEATShapefiles", "AssessmentUnit_20112016"), "AssessmentUnit_20112016Polygon", verbose = FALSE)
} else {
  ## if assessment polygons shapefile exists locally
  SEA <- rgdal::readOGR("data/OxygenDebt/shapefiles", "AssessmentUnit_20112016Polygon", verbose = FALSE)
}
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

## WRITE OUTPUT ---
out_filename <- sprintf("uncorrected_indicator_table_%s_%s.csv", config$years[1], config$years[length(config$years)])
out_y_filename <-sprintf("uncorrected_indicator_table_by_year_%s_%s.csv", config$years[1], config$years[length(config$years)])
## remote
# out_fp_remote <- file.path(bhiremote, "HEATOutput", "analysis", "output")
# if(file.exists(file.path(bhiremote, "HEATOutput", "analysis"))){
#   if(!file.exists(out_fp_remote)){dir.create(out_fp_remote)}
#   write.csv(out, file = file.path(out_fp_remote, "OxygenDebt", out_filename), row.names = FALSE)
#   write.csv(out_y, file = file.path(out_fp_remote, "OxygenDebt", out_y_filename), row.names = FALSE)
# }
## local
write.csv(out, file = file.path("analysis", "output", "OxygenDebt", out_filename), row.names = FALSE)
write.csv(out_y, file = file.path("analysis", "output", "OxygenDebt", out_y_filename), row.names = FALSE)


# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))
