## Running the HEAT assessment tool to Calculate Oxygen Debt for BHI

## load header function into top of search list
source("scripts/OxygenDebt/00_initialise.R")

## no config.json file available, so manually specify these as a list here

config <- list()
config[["years"]] <- 2014:2018


## refer to readme in main directory for more details on how the procedure was adjuested for BHI calculations
## e.g. original ftp server was not available for downloading data, so some changes were made:
## scripts/OxygenDebt/data_1_download.R was deleted
## shapefiles were obtained from other sources and used: 
## helcom shp was from BHI analysis elsewhere and BALTSEM shp was found...

## notes on functions: 

## O2satFun in scripts/OxygenDebt/utils/data_functions.R:
## calculates what oxygen 'should' be given temp observations,
## contrasted with is measured to give oxygen deficit observations

## scripts/OxygenDebt/utils/fit.R doonefit_full function:
## takes cleaned oxygen data with oxygen deficit observations
## determines if there are sufficient observations and calculates:
## lower bound for halocline, fit salinity profile, and depth of change points 1 and 2, 
## oxygen deficit model based on oxygen slope computed from the data... (?)


## get scripts to run
files <- paste0(
  "scripts/OxygenDebt/",
  dir("scripts/OxygenDebt/", pattern = "^(data|input|model|output)_.*[.]R$")
)

## run scripts in correct order
for (file in sort(files)) {
  source(file, echo = TRUE, keep.source = TRUE)
}
