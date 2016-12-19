# ----------------------------
#
#   Prepare compiled data for analysis
#
#     * read compiled data from folder - 'input/OxygenDebt'
#     * prepare data for analysis and write to folder - 'model/OxygenDebt'
#
# ----------------------------

# load packages etc.
header("input")

# start timer
t0 <- proc.time()

# ----------------------------
#
#  read oxy data
#
# ----------------------------

oxy <- read.csv("analysis/input/OxygenDebt/oxy.csv")

# ----------------------------
#
#  tidy data and form new variables
#
# ----------------------------

# compute number of oxygen observations per station
oxy$n_Oxygen <- c(with(oxy, tapply(Oxygen, ID, function(x) sum(!is.na(x))))[paste(oxy$ID)])

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

# drop missing observations ? - I think so - why keep double NA rows ?
oxy <- subset(oxy, !is.na(Salinity) | !is.na(Oxygen_deficit))


if (FALSE) {
  # inspect
  str(oxy)
  length(unique(oxy$ID))
  # report to file - the number of records read in ?
  # which(with(unique(oxy[c("StationID", "ID")]), table(ID)) > 1)
}


# set up covariates for modelling
oxy$date <- ymd(with(oxy, paste(Year, Month, Day, sep = "-")))
oxy$yday <- yday(oxy$date)
oxy$Basin <- factor(oxy$Basin)

# write data --------------------

write.csv(oxy, "analysis/input/OxygenDebt/oxy_clean.csv", row.names = FALSE)

# done -------------------

message(sprintf("time elapsed: %.2f seconds", (proc.time() - t0)["elapsed"]))
