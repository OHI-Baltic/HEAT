Oxygen Debt Indicator Calculation Scripts
-----------------------------------------

``` r
# load libraries
source("scripts/OxygenDebt/header.R")

# read in data
oxy <- read.csv("model/input.csv")
profiles <- read.csv("output/profiles.csv")
```

``` r
# inpect the results from one fit

ID <- 8492
data <- oxy[oxy$ID == ID,]
fit <- doonefit_full(data, ID = ID)

p <- plot_fit(fit, data)
print(p)
```

![](ReadMe_files/figure-markdown_github/example_run-1.png)
