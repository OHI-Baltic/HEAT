
# load header function into top of search list
while("oxydebt_funs" %in% search()) detach("oxydebt_funs")
sys.source("scripts/OxygenDebt/zz_header.R", envir = attach(NULL, name = "oxydebt_funs"))

# reinstall oxydebt
devtools::document("../oxydebt")
devtools::check("../oxydebt")
devtools::install("../oxydebt")

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
source("scripts/OxygenDebt/model_3_indicators.R")

# output





# load libraries
header("data")

# read in data
oxy <- read.csv("model/input.csv", stringsAsFactors = FALSE)
profiles <- read.csv("output/profiles.csv", stringsAsFactors = FALSE)

# inpect the results from one fit

tail(sort(table(oxy$ID)),10)
# 10776 10880 10887 10778 10777 10917 10915 10882 10919 10918
#   411   416   420   426   427   438   440   444   446   456
ID <- 44
data <- oxy[oxy$ID == ID,]
debug = FALSE
i <- data$Assessment_Unit[1]
j <- ID

p <- plot_data(data)
print(p)

fit <- doonefit_full(data, ID = data$ID[1])
fit

p <- plot_fit(fit, data)
print(p)

# some examples of poorly fitting profiles
plot_profile <- function(id) {
  data <- subset(oxy, ID == paste(id))
  fit <- doonefit_full(data, ID = id)
  print(plot_fit(fit, data))
}

# poor linkage with halocline
plot_profile(10918)
#  plot_profile(277369)
# double samples?
#  plot_profile(7566602)
#  plot_profile(7566617)



gam2raster <- function(g, r) {
  # get x and y values
  e <- extent(r)
  rx <- seq(e[1]+xres(r)/2, e[2]-xres(r)/2, length = ncol(r))
  ry <- seq(e[4]-yres(r)/2, e[3]+yres(r)/2, length = nrow(r))

  # make sure and match x and y to unpacking order of raster values
  pred <-
    data.frame(Longitude = rep(rx, nrow(r)),
               Latitude = rep(ry, each = ncol(r)),
               yday = 1,
               Year = 2015)
  mask <- which(!is.na(r[]))
  pred <- pred[mask,]
  names(r) <- "layer"
  r[] <- NA

  # predict
  X <- predict(g, newdata = pred, type = "lpmatrix")
  b <- coef(g)
  b[grep("yday",names(b))] <- 0
  b[grep("Year",names(b))] <- 0
  r[mask] <- X%*%b
  r
}





