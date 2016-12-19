
# load header function into top of search list
while("tools:oxydebt_funs" %in% search()) detach("tools:oxydebt_funs")
sys.source("scripts/OxygenDebt/zz_header.R", envir = attach(NULL, name = "tools:oxydebt_funs"))

# data
source("scripts/OxygenDebt/data_1_download.R")
source("scripts/OxygenDebt/data_2_make_assessment_area.R")
source("scripts/OxygenDebt/data_3_make_bathymetry_layer.R")
source("scripts/OxygenDebt/data_4_data_preparation.R")

# modelling prep
source("scripts/OxygenDebt/input_1_data_cleaning.R")

# modelling
source("scripts/OxygenDebt/model_1_profiles.R")
source("scripts/OxygenDebt/model_2_spatial_profiles.R")
source("scripts/OxygenDebt/model_3_spatial_predictions.R")

# output
source("scripts/OxygenDebt/output_1_tables.R")
source("scripts/OxygenDebt/output_2_plots.R")
