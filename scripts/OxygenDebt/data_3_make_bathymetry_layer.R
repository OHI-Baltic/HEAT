# ----------------------------
#
#   make depth spatial points layer
#
#     * use baltsem data to create a spatial points layer of depths
#
# ----------------------------

# load packages etc.
header("data")

# start timer
t0 <- proc.time()

# read raw depth points
bathy <- read.csv("data/OxygenDebt/BALTIC_BATHY_BALTSEM.csv")
names(bathy) <- cleanColumnNames(names(bathy))
bathy <- dplyr::rename(bathy, depth = dybde)
bathy <- bathy[c("x", "y", "depth")]

# make into spatial points dataframe (note implicit utm34 in BALTIC_BATHY_BALTSEM.csv)
sp::coordinates(bathy) <- c("x", "y")
sp::proj4string(bathy) <- sp::CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# trim to extent of assessment units
helcom <- rgdal::readOGR("data/OxygenDebt/shapefiles", "helcom_areas", verbose = FALSE)
bathy <- bathy[rgeos::gIntersects(bathy, rgeos::gUnaryUnion(helcom), byid = TRUE)[1,], ]

# join points with new helcom polygons
bathy$Basin <- sp::over(bathy, helcom)$Basin

# check
if (FALSE) {
  sp::plot(bathy, col = gplots::rich.colors(50, alpha=0.5)[cut(bathy$depth, 50)], pch = ".")
  sp::plot(helcom, border = "red", add = TRUE)
}

# write
rgdal::writeOGR(bathy[c("depth", "Basin")], "data/OxygenDebt/shapefiles", "helcom_bathymetry", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# add to zip
zip("data/OxygenDebt/zips/helcom_bathymetry.zip",
    paste0("data/OxygenDebt/shapefiles/",
           dir("data/OxygenDebt/shapefiles", pattern = "^helcom_bathymetry*"))
)

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))
