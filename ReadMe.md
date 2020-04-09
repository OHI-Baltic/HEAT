## Running the HEAT assessment tool to Calculate Oxygen Debt for BHI

### HEAT assessment tool

The contents of this repository were coloned from [the ICES tools HEAT repository](https://github.com/ices-tools-prod/HEAT). The `calculate_oxygen_debt.R` script was created to ....

As of March 2020, the server referenced in the OxygenDebt/data_1_download.R script (ftp://ftp.ices.dk/dist/heat/) is no longer available. We  revise the code flow slightly to accomodate this, and use data downloaded manually from the [ICES data portal](https://ocean.ices.dk/HydChem/HydChem.aspx?plot=yes) and merged. The bathymetry data were obtained from (?)

### Oxygen Debt Indicator Calculation Scripts

Since the ftp server is not available the `data_1_download.R script` is not needed so we deleted it. We keep the original names of the remaining data scripts, so the names align with the original HEAT repo names. 


The `data_2_make_assessement_area.R script` was edited to used the HELCOM shapefile (renamed `helcom_areas` to match the code in the HEAT repo) already used for other BHI data preparation. The raw HECOM areas, BALTSEM and merged `helcom_balsem` shapefiles are all saved to the OxygenDebt/shapefiles directory.

In `data_4_data_preparation.R script` we read the [bottle data obtained from ICES](https://raw.githubusercontent.com/OHI-Science/bhi-prep/master/data/CW/eutrophication/v2019/intermediate/ox_merged_rawdata.csv) from github. The `auxillary.csv` with surface depth parameter is also prepared and read from.....(?)

There is also no config.json file, so specify parameters directly where would otherwise be supplied by config.json...


