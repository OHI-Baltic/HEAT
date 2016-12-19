# ----------------------------
#
#   make assessment area
#
#     * merge helcom assessment areas and balsem areas
#
# ----------------------------

# load packages etc.
header("data")

# start timer
t0 <- proc.time()

# ----------------------------
#
#  Create new shapefile for helcom areas
#
# ----------------------------

# read helcom and drop non SEA areas
helcom <- readOGR("data/OxygenDebt/shapefiles", "AssessmentUnit_20112016Polygon", verbose = FALSE)
helcom <- helcom[grep("^SEA-", helcom$Code),]
# transform to utm34
helcom <- spTransform(helcom, sp::CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# merge areas (need to buffer a bit for a clean merge)
helcom_balsem <- gUnaryUnion(gBuffer(helcom, byid = TRUE, width = 10))

# read baltsem, and cut over helcom
baltsem <- readOGR("data/OxygenDebt/shapefiles", "Baltsem_utm34", verbose = FALSE)
helcom_balsem <- gIntersection(baltsem, helcom_balsem, byid = TRUE)
for (i in 1:length(helcom_balsem)) helcom_balsem@polygons[[i]]@ID <- paste(i)
data <-
  do.call(rbind,
          lapply(1:length(helcom_balsem),
                 function(i) over(spsample(helcom_balsem[i,], 1, type = "random"), baltsem)))
helcom_balsem <- SpatialPolygonsDataFrame(helcom_balsem, data)

# fix names
helcom_balsem$Bo_Basin <- gsub("Ã", "oe", helcom_balsem$Bo_Basin)
helcom_balsem$Bo_Basin <- iconv(helcom_balsem$Bo_Basin, "UTF-8", "ASCII", sub = "")
helcom_balsem$Bo_Basin <- gsub("oeresund", "Oeresund", helcom_balsem$Bo_Basin)
helcom_balsem$Basin <- helcom_balsem$Bo_Basin

# check
if (FALSE) {
  plot(helcom_balsem, col = gplots::rich.colors(nrow(helcom_balsem), alpha=0.5))
  text(coordinates(helcom_balsem), as.character(helcom_balsem$Basin), cex = 0.7)
}

# write
writeOGR(helcom_balsem["Basin"], "data/OxygenDebt/shapefiles", "helcom_areas", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# add to zip
zip("data/OxygenDebt/zips/helcom_areas.zip",
    paste0("data/OxygenDebt/shapefiles/",
           dir("data/OxygenDebt/shapefiles", pattern = "^helcom_areas*"))
    )

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))
