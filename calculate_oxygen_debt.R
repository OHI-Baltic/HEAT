## Running the HEAT assessment tool to Calculate Oxygen Debt for BHI

## load header function into top of search list
source("scripts/OxygenDebt/00_initialise.R")


## no config.json file available, so manually specify these as a list here
config <- list()
config[["years"]] <- 2014:2018

## refer to readme in main directory for more details on how the procedure was adjuested for BHI calculations
## e.g. original ftp server was not available for downloading data, so some changes were made:
## scripts/OxygenDebt/data_1_download.R was deleted
## helcom shp was taken from BHI analysis elsewhere
## Baltsem_utm34 was obtained from ... and BALTIC_BATHY_BALTSEM csv file was obtained from the same source...
## auxilary file with surface depths, salmod_max_depth, and max_depth was... (recreate??)


## notes on functions: 

## O2satFun in scripts/OxygenDebt/utils/data_functions.R:
## calculates what oxygen 'should' be given temp observations,
## contrasted with is measured to give oxygen deficit observations

## scripts/OxygenDebt/utils/fit.R doonefit_full function:
## takes cleaned oxygen data with oxygen deficit observations, plus the auxillary info table
## determines if there are sufficient observations and calculates:
## lower bound for halocline, fit salinity profile, and depth of change points 1 and 2, 
## oxygen deficit model based on oxygen slope computed from the data... (???)


## get scripts to run
files <- paste0(
  "scripts/OxygenDebt/",
  dir("scripts/OxygenDebt/", pattern = "^(data|input|model|output)_.*[.]R$")
)

## order of scripts execution:
# source("scripts/OxygenDebt/data_2_make_assessment_area.R")
# source("scripts/OxygenDebt/data_3_make_bathymetry_layer.R")
# source("scripts/OxygenDebt/data_4_data_preparation.R")
# source("scripts/OxygenDebt/input_1_data_cleaning.R")
# source("scripts/OxygenDebt/model_1_profiles.R")
# source("scripts/OxygenDebt/model_2_spatial_profiles.R")
# source("scripts/OxygenDebt/model_3_spatial_predictions.R")
# source("scripts/OxygenDebt/output_1_tables.R")
# source("scripts/OxygenDebt/output_2_tables_corrected.R")

## run all scripts in correct order
for (file in sort(files)) {
  source(file, echo = TRUE, keep.source = TRUE)
}


## oxygen input data and example plot of fitted haloclines... (?) 
oxy <- read.csv("analysis/input/OxygenDebt/oxy.csv")
str(oxy)
ID <- 8492
data <- oxy[oxy$ID == ID,]
fit <- doonefit_full(data, ID = ID)
# plot(pred$Depth, pred$fit, type = "l", col = "red")
# points(pdata$Depth, pdata$Oxygen_deficit, col = "red")


