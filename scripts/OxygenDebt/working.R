

# reinstall?
devtools::document("oxydebt")
devtools::check("oxydebt")
devtools::install("oxydebt")

# load libraries
source("scripts/header.R")

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








