
# read in data from previous assessment with covariates for anthropogenic corrections
dat3 <-
  within(read.csv("../Jacob_data/Oxygen debt_trend data from Jacob Carstensen_all_trends2.csv"),
  {

  })

if (!TRUE) {
  basin <- "BO" # "BO", "BP"
  basinaux <- "Bornholm Basin" # "Baltic Proper"
} else {
  basin <- "BP" # "BO", "BP"
  basinaux <- "Baltic Proper" # Bornholm Basin" # "Baltic Proper"
}


dat3 <- dplyr::select(dat3, year, intensity__Qt,
                            dplyr::ends_with(basin),
                            dplyr::contains(paste0("_", basin, "_N")))
names(dat3) <- gsub(paste0("_",basin), "", names(dat3))



aux <- read.csv("data/OxygenDebt/auxilliary.csv")
aux <- dplyr::filter(aux, Basin == basinaux)
aux <- dplyr::select(aux, dplyr::starts_with("a_"))



# investigate the model parameters
# ----------------------------------

o2N <-
    cbind(dplyr::select(dat3, year),
          mbi = -1 * dat3$intensity__Qt,
          bv = dat3$N_BV,
          N = dat3$TNtotspec,
          sal = -1 * dat3$bottom_salinityspec,
          y = dat3$O2debtspec / dat3$N_BV,
          o2 = dat3$O2debtspec,
          o2N = dat3$O2debtspec_N,
          o2N2 = dat3$O2debtspec_N2,
          o2N3 = dat3$O2debtspec_N3,
          o2Nx = dat3$O2debtspec_Nx,
          o2Ny = dat3$O2debtspec_Ny
  )

m0 <- lm(o2 ~ bv + I(N*bv) + I(mbi*bv) + I(sal*bv) - 1, data = o2N[complete.cases(o2N),])
m1 <- lm(o2N ~ bv + I(N*bv) + I(mbi*bv) + I(sal*bv) - 1, data = o2N[complete.cases(o2N),])
m2 <- lm(o2N2 ~ bv + I(N*bv) + I(mbi*bv) + I(sal*bv) - 1, data = o2N[complete.cases(o2N),])
m3 <- lm(o2N3 ~ bv + I(N*bv) + I(mbi*bv) + I(sal*bv) - 1, data = o2N[complete.cases(o2N),])
mx <- lm(o2Nx ~ bv + I(N*bv) + I(mbi*bv) + I(sal*bv) - 1, data = o2N[complete.cases(o2N),])
my <- lm(o2Ny ~ bv + I(N*bv) + I(mbi*bv) + I(sal*bv) - 1, data = o2N[complete.cases(o2N),])

# these are identical!
logLik(m0)
logLik(m1)
logLik(mx)
logLik(my)

# these are different
logLik(m2)
logLik(m3)

same <- list(m0, m1, mx, my)
lapply(same, summary)
round(t(sapply(same, coef)), 2)

with(o2N, o2N3 - o2N2)

o2N$o2

o2N[complete.cases(o2N),]
mtest <- lm(I(o2 - o2Ny) ~ bv + I(N*bv) + I(mbi*bv) + I(sal*bv) - 1, data = o2N[complete.cases(o2N),])
summary(mtest)
round(coef(mtest), 2)
aux



mtest <- lm(I(o2 - o2Nx) ~ bv + I(N*bv) + I(mbi*bv) + I(sal*bv) - 1, data = o2N[complete.cases(o2N),])
summary(mtest)
round(coef(mtest), 2)
aux
with(o2N, plot(year, o2 - o2Nx, main = "model predictions"))
# obs - o2Nx gives the exact model parameters....
# hence o2Nx is the model residuals... the AR1 model residuals
with(o2N, plot(year, o2Nx, main = "AR1 model residuals"))
acf(o2N$o2Nx, na.action = na.pass)[1]


# this gives a model with only N
mtest <- lm(I(o2N - o2Nx) ~ bv + I(N*bv) + I(mbi*bv) + I(sal*bv) - 1, data = o2N[complete.cases(o2N),])
summary(mtest)
round(coef(mtest), 2)
aux

# this gives a model with N set to zero and a_0 set to 180.1 for BP and 165.59 for BO ...
mtest <- lm(I(o2Ny - o2Nx) ~ bv + I(N*bv) + I(mbi*bv) + I(sal*bv) - 1, data = o2N[complete.cases(o2N),])
summary(mtest)
round(coef(mtest), 2)
aux





require(nlme)

m1.gls <-
  gls(o2 ~ bv + I(N*bv) + I(mbi*bv) + I(sal*bv) - 1,
      o2N[complete.cases(o2N),],
      corr = corAR1(aux$a_AR1, form = ~ year, fixed = TRUE)
      )
m1.gls
summary(m1.gls)
cbind(round(coef(m1.gls), 2),
      unlist(aux)[1:4])

with(o2N, plot(year, o2Nx, main = "AR1 model residuals", pch = 16, col = "lightblue"))
points(o2N$year[complete.cases(o2N)], resid(m1.gls), col = "red")



# almost but there must be differences in the fitting... maybe SAS is not REML?
# pretty close though :)







# but what is N2!!! - feck knows!
with(o2N[complete.cases(o2N),], {
  plot(year, o2N3 , pch = 16, col = "lightblue")
  points(year, o2, col = "blue")
  points(year, aux$a_N * N * bv + resid(m1.gls) + 3.893, col = "red", pch = 16)
})


# no nitrogen data from 2007
with(o2N, {
  plot(year, o2N )
  points(year, o2N2, col = "red")
})

with(o2N, {
  plot(year, o2N - o2N2)
})



mN <- lm(o2N3 ~ bv + I(N*bv) + I(mbi*bv) + I(sal*bv) - 1, data = o2N)
summary(mN)
round(coef(mN), 3)

#
with(o2N, {
  plot(year, o2N3, pch = 16, col = "lightblue")
  points(year, o2N)
  #points(year, o2N3 - o2Nx - 6.895179, col = "red")
})

with(o2N, {
  plot(year,  o2N, pch = 16, col = "lightblue")
  points(year, o2N2)
})

with(o2N, {
  plot(year,  o2N - o2N2, pch = 16, col = "lightblue")
  abline(h = 0)
})



mean(dat3$O2debtspec_N3[dat3$year %in% 2007:2011])

mean(dat3$O2debtspec[dat3$year %in% 2007:2011])
