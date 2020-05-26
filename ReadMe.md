## Running the HEAT assessment tool to Calculate Oxygen Debt for BHI

### HEAT assessment tool
The oxygen debt, i.e., "missing" oxygen in relation to fully oxygenated water column, calculations follow the approach introduced in HELCOM Indicator Sheets (2018) and the same threshold values (the 95th percentiles before 1940, detected using change point analysis) are used. In this approach the oxygen debt below halocline is assessed and applied as an indicator of eutrophication, such that the variations due to temperature dependent changes in O2 solubility are excluded. General additive models are applied to address temporal, seasonal and spatial variation within the basins. This approach currently allows the calculation of oxygen debt scores for the open sea Baltic Proper and Bornholm Basin. The algorithm used is not optimized for the use in other regions yet. (more information available at the HELCOM 2013 (BSEP133))

The contents of this repository were forked from [the ICES tools HEAT repository](https://github.com/ices-tools-prod/HEAT). The `calculate_oxygen_debt.R` script was created to ....

As of March 2020, the server referenced in the OxygenDebt/data_1_download.R script (ftp://ftp.ices.dk/dist/heat/) is no longer available. We  revise the code flow slightly to accomodate this, and use data downloaded manually from the [ICES data portal](https://ocean.ices.dk/HydChem/HydChem.aspx?plot=yes) and merged. The bathymetry (BALTIC_BATHY_BALTSEM) and Baltsem shapefiles (Baltsem_utm34) data was obtained from ICES together with auxiliary information about salinity profiles (auxiliary.csv) and timeseries of Baltic Inflows (MajorBalticInflows.csv) and nitrogen (nitrogen.csv)

### Oxygen Debt Indicator Calculation Scripts

Since the ftp server is not available the `data_1_download.R script` is not needed so we deleted it. We keep the original names of the remaining data scripts, so the names align with the original HEAT repo names. 


The `data_2_make_assessement_area.R script` was edited to used the HELCOM shapefile (renamed `helcom_areas` to match the code in the HEAT repo) already used for other BHI data preparation. The raw HECOM areas, shapefiles are all saved to the data/OxygenDebt/shapefiles and/or data/OxygenDebt/zips directories.

In `data_4_data_preparation.R script` we read the [bottle data](https://raw.githubusercontent.com/OHI-Science/bhi-prep/master/data/CW/eutrophication/v2019/intermediate/ox_merged_rawdata.csv) and [CTD data](https://raw.githubusercontent.com/OHI-Science/bhi-prep/master/data/CW/eutrophication/v2019/intermediate/ctd_merged_rawdata.csv) (both obtained from ICES database) from the `bhi-prep` github repo. The `auxillary.csv` with surface depth parameter is also prepared and read from a local source.

There is also no config.json file, so specify parameters directly where would otherwise be supplied by config.json...
