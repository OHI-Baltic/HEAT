

# read in data from previous assessment
dat1 <-
  within( read.csv("../Jacob_data/basin_trends_Carstensen_baltic_proper.csv"),
  {
    Lhalo_salinity = Lhalo_salinity/1000000
    uhalo_salinity = uhalo_salinity/1000000
    bottom_salinity = bottom_salinity/1000000
    surface_salinity = surface_salinity/1000000
    Nbv = sqrt( 9.8 / (1 + 0.001 * (sali_surf + sali_dif/2)) *
                       0.001 * sali_dif / (2*depth_gradient))
    O2debt = bottom_O2debt + Lhalo_O2debt
    # volume specific O2 debt * 1000 t get in mg pr litre
    O2debt_volsp  = O2debt / (Lhalo_volume + bottom_volume) * 1000
  })

# add in mbi
# load baltic inflow data
mbi <- read.csv("data/OxygenDebt/MajorBalticInflows.csv")
mbi <- tapply(mbi$intensity, mbi$year_end, sum)
mbi <- data.frame(year = as.numeric(names(mbi)),
                  mbi = c(unname(mbi)))

dat1 <- dplyr::left_join(dat1, mbi)
rm(mbi)

# add in Ninput
# load nutrient data
Ninput <- read.csv("data/OxygenDebt/nitrogen.csv")
Ninput <- dplyr::rename(Ninput, year = Year, Ninput = BP)
Ninput <- Ninput[c("year", "Ninput")]

dat1 <- dplyr::left_join(dat1, Ninput)
rm(Ninput)

dat1 <- dat1[dat1$year %in% 1900:2010,]



# read in data from previous assessment with covariates for anthropogenic corrections
dat2 <-
  within(read.csv("../Jacob_data/Oxygen debt_trend data from Jacob Carstensen_all_trends.csv"),
  {

  })

dat2 <- cbind(dat2["year"], dplyr::select(dat2, dplyr::ends_with("BP")))
names(dat2) <- gsub("_BP", "", names(dat2))
dat2 <- dplyr::rename(dat2, mbi = Q)

dat2 <- dat2[dat2$year %in% 1900:2010,]


# read in data from previous assessment with covariates for anthropogenic corrections
dat3 <-
  within(read.csv("../Jacob_data/Oxygen debt_trend data from Jacob Carstensen_all_trends2.csv"),
  {

  })

dat3 <- dplyr::select(dat3, year, intensity__Qt,
                            dplyr::ends_with("BP"),
                            dplyr::contains("_BP_N"))
names(dat3) <- gsub("_BP", "", names(dat3))

dat3 <- dat3[dat3$year %in% 1900:2010,]


# investigate Q intensity
# -------------------------

intensity <-
  cbind(dplyr::select(dat1, year, mbi),
        dplyr::select(dat3, intensity__Qt))

#################
# PASSED        #
#################
# some are different, but only historically
#################


# BV volume
# ----------------------------------

bv <-
  cbind(dat1[c("year", "Nbv")],
        dat3["N_BV"])

xyplot(Nbv + N_BV ~ year, data = bv, type = "l")

#################
# PASSED        #
#################


# investigate bottom volume
# ----------------------------------

bottom_vol <-
  cbind(dat1["year"],
        vol1 = dat1$Lhalo_volume + dat1$bottom_volume,
        vol2 = dat2$bottom_volume)

xyplot(vol1 + vol2 ~ year, data = bottom_vol, type = "l")


#################
# PASSED        #
#################



# investigate total nitrogen inputs
# ----------------------------------

tn <-
  cbind(dplyr::select(dat2, year, dplyr::starts_with("TN")),
        tot = dat2$TNa + dat2$TNp + dat2$TNd,
        dat3 = dat3$TNtotspec * dat2$bottom_volume * 50,
        dat1 = dat1$Ninput
  )

tn <- dplyr::mutate(tn, sum = TNd + TNa + TNp)

lattice::xyplot(dat3 + sum ~ year, data = tn, type = "l", auto.key = TRUE)

#######################################################
# ALMOST PASSED - why is it out by a factor of 50 !?! #
#######################################################



# investigate total phosphorous inputs
# ----------------------------------

tp <-
  cbind(dplyr::select(dat2, year, dplyr::starts_with("TP")),
        dat3 = dat3$TPtotspec * dat2$bottom_volume * 5
  )

tp <- dplyr::mutate(tp, sum = TPd + TPa + TPp)

lattice::xyplot(dat3 + sum ~ year, data = tp, type = "l", auto.key = TRUE)


######################################################
# ALMOST PASSED - why is it out by a factor of 5 !?! #
######################################################




# investigate bottom salinity
# ----------------------------------

sal <-
    cbind(dplyr::select(dat2, year),
          dat3 = dat3$bottom_salinityspec * dat2$bottom_volume / 1000,
          dat1 = dat1$bottom_salinity + dat1$Lhalo_salinity
  )

sal

lattice::xyplot(dat3 + dat1 ~ year, data = sal, type = "l", auto.key = TRUE)

#################
# PASSED        #
#################


# investigate o2 debt
# ----------------------------------

o2 <-
    cbind(dplyr::select(dat2, year),
          dat3 = dat3$O2debtspec * dat2$bottom_volume / 1000,
          dat1 = dat1$bottom_O2debt + dat1$Lhalo_O2debt
  )

lattice::xyplot(dat3 + dat1 ~ year, data = o2, type = "l", auto.key = TRUE)


#################
# PASSED        #
#################

# investigate the model parameters
# ----------------------------------

o2N <-
    cbind(dplyr::select(dat3, year),
          mbi = dat3$intensity__Qt,
          bv = dat3$N_BV,
          N = dat3$TNtotspec,
          sal = -1 * dat3$bottom_salinityspec,
          y = dat3$O2debtspec / dat3$N_BV,
          o2 = dat3$O2debtspec
  )

m1 <- glm(y ~ 1 + N + mbi + sal, data = o2N[complete.cases(o2N),], family = Gamma(log))
summary(m1)


# residual SE should be 0.6!




#
require(nlme)
m2 <- gls(y ~ 1 + N + mbi + sal,
          corr = corAR1(0.8, form = ~ 1),
          data = o2N[complete.cases(o2N),])
round(coef(m2), 3)



dplyr::select(aux, dplyr::starts_with("a_"))



# investigate modelled o2 debt
# ----------------------------------

# read in regression parameters
aux <- read.csv("data/OxygenDebt/auxilliary.csv")
aux <- dplyr::filter(aux, Basin == "Baltic Proper")
aux$a_0 <- 277.1

o2N <-
    cbind(dplyr::select(dat3, year),
          mbi = dat3$intensity__Qt,
          bv = dat3$N_BV,
          tn = dat3$TNtotspec,
          sal = dat3$bottom_salinityspec,
          raw = dat3$O2debtspec,
          N = dat3$O2debtspec_N,
          N2 = dat3$O2debtspec_N2,
          N3 = dat3$O2debtspec_N3,
          Nx = dat3$O2debtspec_Nx,
          Ny = dat3$O2debtspec_Ny
  )

o2N <- dplyr::mutate(o2N, test = raw - N)
#o2N

lattice::xyplot(raw + N3 ~ year, data = o2N, type = "p", auto.key = TRUE)
lattice::xyplot(bv ~ year, data = o2N, type = "p", auto.key = TRUE)

dplyr::select(o2N, N2, test)


#datnew$O2debt_volsp -
#             aux$a_MBI * datnew$mbi * datnew$Nbv -
#             aux$a_salinity * (datnew$bottom_salinity + datnew$Lhalo_salinity)/10 * datnew$Nbv

o2N <- dplyr::mutate(o2N, test1 = aux$a_0 * bv +
                                  aux$a_N * tn * bv +
                                  aux$a_MBI * mbi * bv -
                                  aux$a_salinity * sal * bv)

o2N <- dplyr::mutate(o2N, test1 = aux$a_0 * bv +
                                  aux$a_N * tn * bv +
                                  aux$a_MBI * mbi * bv -
                                  aux$a_salinity * sal * bv)

o2N <- dplyr::mutate(o2N, test2 = raw - test1)


lattice::xyplot(test2 ~ year, data = o2N, type = "l", auto.key = TRUE)

o2N





























