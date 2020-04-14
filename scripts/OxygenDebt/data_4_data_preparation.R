# ----------------------------
#
#   Prepare raw data for analysis
#
#     * read raw data from folder - 'data/OxygenDebt'
#     * prepare data for analysis and write to folder - 'input/OxygenDebt'
#
# ----------------------------

# load packages etc.
header("data")

# start timer
t0 <- proc.time()

# create input folder
if (!dir.exists("analysis")) dir.create("analysis")
if (!dir.exists("analysis/input")) dir.create("analysis/input")
if (!dir.exists("analysis/input/OxygenDebt")) dir.create("analysis/input/OxygenDebt")

# read in data ----------------------------------------------------------

# read CTD data and remove non SEA samples
ctd <-  data.table::fread("https://raw.githubusercontent.com/OHI-Science/bhi-prep/master/data/CW/eutrophication/v2019/intermediate/ox_merged_rawdata.csv")
ctd <- ctd[grepl("SEA-", ctd$Assessment_Unit),]

# read dat and remove non SEA samples
bot <- data.table::fread("https://raw.githubusercontent.com/OHI-Science/bhi-prep/master/data/CW/eutrophication/v2019/intermediate/ox_merged_rawdata.csv")
bot <- bot[grepl("SEA-", bot$Assessment_Unit),]

# join
oxy <- dplyr::full_join(ctd, bot, suffix = c(".ctd", ".bot"),
                        by = c("Year", "Month", "Day", "Hour", "Minute",
                               "Latitude", "Longitude",
                               "Cruise", "Depth"))
rm(ctd, bot)

# apply rules for which data to take:
#   keep ctd temperature over bottle, unless ctd is missing (i.e. is NA)
keep_x <- function(x, y) ifelse(is.na(x), y, x)
oxy$Oxygen <- keep_x(oxy$Oxygen.ctd, oxy$Oxygen.bot)
oxy$Temperature <- keep_x(oxy$Temperature.ctd, oxy$Temperature.bot)
oxy$Salinity <- keep_x(oxy$Salinity.ctd, oxy$Salinity.bot)
oxy$Type <- ifelse(is.na(oxy$Oxygen.ctd), "BOT", "CTD")
rm(keep_x)

# keep only data for the years given in the config file
# config <- jsonlite::fromJSON("data/OxygenDebt/config.json")
oxy <- oxy[oxy$Year %in% config$years,]

# create profile ID
oxy$ID <- as.integer(factor(
  apply(
    oxy[c("Year", "Month", "Day", "Hour", "Minute", "Latitude", "Longitude", "Cruise")],
    1,
    paste, 
    collapse = "_"
  )
))
# sort by ID
oxy <- oxy[order(oxy$ID),]

# retain check for duplicate observations
# todo:
#   * retain a log of duplicates for checking by ICES or institutes / experts
#   * could provide a report of duplicates / near duplicates.
#

# ----------------------------
#
#  retain only columns we need
#
# ----------------------------

oxy <- oxy[c("ID", "Year", "Month", "Day",
             "Latitude", "Longitude", "Depth", "Type",
             "Temperature", "Salinity", "Oxygen", "Hydrogen_Sulphide")]

# ----------------------------
#
# convert oxy to utm34
#
# ----------------------------

# duplicate Long and Lat, for conversion
oxy[c("x", "y")] <- data.frame(x = oxy$Longitude, y = oxy$Latitude)
# make into spatialDataFrame and define coordinage reference system (CRS)
sp::coordinates(oxy) <- ~ x + y
sp::proj4string(oxy) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# transform to umt34
oxy <- sp::spTransform(oxy, sp::CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


# ----------------------------
#
#  read in new helcom shape to add assessment basin information
#
# ----------------------------

# read helcom indicator modelling areas
# helcom <- rgdal::readOGR("data/OxygenDebt/shapefiles", "helcom_areas", verbose = FALSE)
helcom_sf <- sf::st_read("data/OxygenDebt/shapefiles", "helcom_areas")
helcom <- sf::as_Spatial(helcom_sf)

# join points with polygons
oxy.helcom <- sp::over(oxy, helcom)

# convet back to dataframe
oxy <- as.data.frame(oxy)
oxy[names(helcom)] <- oxy.helcom
rm(helcom, oxy.helcom)

# ----------------------------
#
#  merge auxiliary info table
#
# ----------------------------

# read in auxilliary info
aux <- read.csv("data/OxygenDebt/auxilliary.csv")

# merge
oxy <- dplyr::left_join(oxy, aux, by = "Basin")
rm(aux)

# drop regions that have no auxilliary info
oxy <- oxy[!is.na(oxy$surfacedepth1),]

# ----------------------------
#
#  write out data
#
# ----------------------------

write.csv(file = "analysis/input/OxygenDebt/oxy.csv", oxy, row.names = FALSE)

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))

