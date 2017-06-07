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

# load gam predictions ('pars')
check <- load("analysis/output/OxygenDebt/gam_predictions.RData")
if (check != "surfaces") {
  stop("Error loading gam predictions!\n\tTry rerunning model_3_spatial_predictions.R")
}
rm(check)

# load regression par estimates
aux <- read.csv("data/OxygenDebt/auxilliary.csv")

# load nutrient and MBI data
Ninput <- read.csv("data/OxygenDebt/nitrogen.csv")
MBI <- read.csv("data/OxygenDebt/MajorBalticInflows.csv")

# create table of indicators by year and by year and basin
ES_y <- with(surfaces, tapply(oxygendebt, list(Basin, Year), mean, na.rm = TRUE))
names(dimnames(ES_y)) <- c("Basin", "Year")
out_y <- do.call(expand.grid, c(dimnames(ES_y), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))

ES <- with(surfaces, tapply(oxygendebt, list(Basin), mean, na.rm = TRUE))
names(dimnames(ES)) <- "Basin"
out <- do.call(expand.grid, c(dimnames(ES), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))

# add indicator estimate
out_y$ES <- c(ES_y)
out$ES <- c(ES)

# add indicator SD
ES_SD_y <- with(surfaces, tapply(oxygendebt, list(Basin, Year), sd, na.rm = TRUE))
out_y$ES_SD <- c(ES_SD_y)

ES_SD <- with(surfaces, tapply(oxygendebt, list(Basin), sd, na.rm = TRUE))
out$ES_SD <- c(ES_SD)

# read profile fits
profiles <- read.csv("analysis/output/OxygenDebt/profiles.csv")

# add in auxilliary information
ES_N_y <- with(profiles, tapply(O2def_slope_below_halocline, list(Basin, Year), function(x) sum(!is.na(x))))
out_y$ES_N <- c(ES_N_y)

ES_N <- with(profiles, tapply(O2def_slope_below_halocline, list(Basin), function(x) sum(!is.na(x))))
out$ES_N <- c(ES_N)

# now expand to AssessmentUnitID
library(sp)
helcom <- rgdal::readOGR("data/OxygenDebt/shapefiles", "helcom_areas", verbose = FALSE)
helcom <- helcom[helcom$Basin %in% out$Basin,]
SEA <- rgdal::readOGR("data/OxygenDebt/shapefiles", "AssessmentUnit_20112016Polygon", verbose = FALSE)
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

  # check
  cbind(with(out_y, tapply(ES_N, Basin, sum)), out$ES_N)
}


# write out
write.csv(out, file = "analysis/output/OxygenDebt/indicator_table.csv", row.names = FALSE)
write.csv(out_y, file = "analysis/output/OxygenDebt/indicator_table_by_year.csv", row.names = FALSE)

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))
