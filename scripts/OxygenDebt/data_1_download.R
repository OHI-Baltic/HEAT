# ----------------------------
#
#   download data
#
#     * download raw data to folder - 'data/OxygenDebt'
#
# ----------------------------

# load packages etc.
header("data")

# start timer
t0 <- proc.time()

# create data folders
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/OxygenDebt")) dir.create("data/OxygenDebt")
if (!dir.exists("data/OxygenDebt/zips")) dir.create("data/OxygenDebt/zips")
if (!dir.exists("data/OxygenDebt/shapefiles")) dir.create("data/OxygenDebt/shapefiles")

# download data from ftp site ------------------------------------------

# the location of the input data
ftp <- "ftp://ftp.ices.dk/dist/heat/"
# temporary file location
#ftp <- "https://raw.githubusercontent.com/ices-tools-prod/heat-ftp-tmp/master/"

# quick function to download input data and save
get_input <- function(from, force = FALSE, zipped = FALSE) {
  to <- from
  if (zipped) {
    to <- gsub("data/OxygenDebt/", "data/OxygenDebt/zips/", to)
  }
  if (!file.exists(to) || force) {
    download.file(paste0(ftp, from), destfile = to, quiet = TRUE)
  }
  if (zipped) {
    unzip(to, exdir = "data/OxygenDebt")
  }
}

# the list of input files required to calculate the indicator
get_input("data/OxygenDebt/auxilliary.csv", force = TRUE)

# download station samples and bathymetry to local folder
tmp <- sapply(c("data/OxygenDebt/BALTIC_BATHY_BALTSEM.zip",
                "data/OxygenDebt/StationSamples_20170611.zip"),
              get_input,
              zipped = TRUE)

# download nutrient input and major baltic infow data
get_input("data/OxygenDebt/nitrogen.csv", force = TRUE)
#KT	Kattegat
#DS	Danish Straits (= WEB + SOU) (= Western Baltic + Sound)
#BP	Baltic Proper
#BS	Bothnian Sea
#BB	Bothnian Bay
#GF	Gulf of Finalnd
#GR	Gulf og Riga
#BAS Baltic Sea shipping

get_input("data/OxygenDebt/MajorBalticInflows.csv", force = TRUE)

# quick function to get shapefiles ----------------------
download.ICESshape <- function(what, root = "http://gis.ices.dk/shapefiles/") {
  download.file(paste0(root, what, ".zip"),
                destfile = paste0("data/OxygenDebt/zips/", what, ".zip"),
                quiet = TRUE)

  unzip(paste0("data/OxygenDebt/zips/", what, ".zip"), exdir = "data/OxygenDebt/shapefiles")
}

# get shapefiles
download.ICESshape("AssessmentUnit_20112016", paste0(ftp, "data/OxygenDebt/"))
download.ICESshape("Baltsem_utm34", paste0(ftp, "data/OxygenDebt/"))

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))

