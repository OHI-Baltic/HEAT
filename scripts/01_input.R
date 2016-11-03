# ----------------------------
#
#   Read in data
#
# ----------------------------

# load packages etc.
source("scripts/header.R")


# download data from ftp site ------------------------------------------

# the location of the input data
ftp <- "ftp://ftp.ices.dk/dist/heat/oxygen-debt-indicator/"

# create input folder
if (!dir.exists("input")) dir.create("input")

# quick function to download input data and save
get_input <- function(what, force = FALSE) {
  if (!file.exists(what) | force) {
    download.file(paste0(ftp, what), destfile = what)
  }
}

# the list of input files required to calculate the indicator
files <- c("input/AssessmentUnit_20112016Polygon.cst",
           "input/AssessmentUnit_20112016Polygon.dbf",
           "input/AssessmentUnit_20112016Polygon.prj",
           "input/AssessmentUnit_20112016Polygon.shp",
           "input/AssessmentUnit_20112016Polygon.shx",
           "input/auxilliary.txt",
           "input/GMT_GEBCO_08_BalticSea.asc",
           "input/StationSamplesCTD.txt",
           "input/StationSamplesICE.txt")

# download to local folder
tmp <- lapply(files, get_input, force = force.download)



# read in data ----------------------------------------------------------


# read CTD data and clean column names
ctd <- read.dbexport("input/StationSamplesCTD.txt")
ctd <- ctd[grepl("SEA-", ctd$Assessment_Unit),]
ctd$Type <- "CTD"

# read bottle data and clean column names
ice <- read.dbexport("input/StationSamplesICE.txt")
ice <- ice[grepl("SEA-", ice$Assessment_Unit),]
ice$Type <- "ICE"
ice <- ice[intersect(names(ctd), names(ice))] # keep only common column names

# drop Station that appear in ctd from ice
# create a key for each dataset
key_columns <- c("Year", "Month", "Day", "Hour", "Minute", "Latitude", "Longitude", "Cruise")
ice$ID <- apply(ice[key_columns], 1, paste, collapse = "_")
ctd$ID <- apply(ctd[key_columns], 1, paste, collapse = "_")
# drop ctd stations from ice dataset
ice <- ice[!is.element(ice$ID, ctd$ID),]
# now ice data is water bottle samples without ctd support ?
ice$Type <- "WQ"
# TODO: create a backwards lookup table to find origional data source from new id

# merge
oxy <- rbind(ice, ctd)
# simplify ID
oxy$ID <- as.integer(factor(oxy$ID))
# sort by ID
oxy <- oxy[order(oxy$ID),]
rm(ice, ctd)

# retain check for duplicate observations
# todo

# only keep units 6-11, 13
form_Assessment_Unit <- function(i) gsub(" ", "0", sprintf("SEA-%3i", i))
oxy <- oxy[is.element(oxy$Assessment_Unit,
                      form_Assessment_Unit(c(6:11, 13))),]

# ----------------------------
#
#  retain only columns we need
#
# ----------------------------

oxy <- oxy[c("ID", "Assessment_Unit", "Year",
             "Month", "Day", "Latitude", "Longitude",
             "Type", "Depth", "Temperature", "Salinity", "Oxygen")]


# ----------------------------
#
#  tidy data and form new variables
#
# ----------------------------

# compute number of oxygen observations per station
oxy$n_Oxygen <- c(with(oxy, tapply(Oxygen, ID, function(x) sum(!is.na(x))))[paste(oxy$ID)])

# keep only stations with oxygen data
oxy <- subset(oxy, n_Oxygen > 0)

# compute max (non NA) oxygen sample depth per station
oxy$max_depth <- c(with(oxy, tapply(Depth, ID, max))[paste(oxy$ID)])

# oxygen observations are in ml/l - convert to mg/l
oxy$Oxygen_ml <- oxy$Oxygen
oxy$Oxygen <- oxy$Oxygen_ml * 1.428 # or / 0.700

# compute oxygen deficit
oxy$Oxygen_deficit <- O2satFun(oxy$Temperature) - oxy$Oxygen

# checks
# supersaturation not realistic below 30 m - error
oxy$Oxygen_deficit[which(oxy$Oxygen_deficit < 0 & oxy$Depth>30)] <- NA

# create alternative oxygen observation? check sas code:
if (FALSE) {
  warning("unclear code at line 44 in file 'salinity oxygen data v2.sas'")
}

# drop missing observations ? - I think so - why keep double NA rows ?
oxy <- subset(oxy, !is.na(Salinity) | !is.na(Oxygen_deficit))


if (FALSE) {
  # inspect
  str(oxy)
  length(unique(oxy$ID))
  # report to file - the number of records read in
  which(with(unique(oxy[c("StationID", "ID")]), table(ID)) > 1)
}


# ----------------------------
#
#  merge auxiliary info table
#
# ----------------------------

# read in auxilliary info
aux <- read.dbexport("input/auxilliary.txt", sep = "\t")

# merge
oxy <- left_join(oxy, aux)
rm(aux)


# ----------------------------
#
#  write out data
#
# ----------------------------

# make sure model folder exists
dir.create("model")

write.csv(file = "model/input.csv", oxy, row.names = FALSE)

