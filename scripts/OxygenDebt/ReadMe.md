Oxygen Debt Indicator Calculation Scripts
-----------------------------------------

The following as an example of fitting the HEAT oxygen debt profile models to CTD or water bottle data. The code below assumes that the repository has been cloned and all work is done from the root directory `.../HEAT/`, and that the data has been downloaded and prepared via:

``` r
source("scripts/OxygenDebt/01_input.R")
```

This will create the folder `model` and the file `model/input.csv`. The following code loads the `oxydebt` package for the `run_full_model` and `plot_fit` functions.

``` r
# load libraries
library(oxydebt)

# read in data
oxy <- read.csv("model/input.csv")
```

``` r
# inspect the results from one fit
ID <- 8492
data <- oxy[oxy$ID == ID,]
fit <- doonefit_full(data, ID = ID)

p <- plot_fit(fit, data)
print(p)
```

![](ReadMe_files/figure-markdown_github/example_run-1.png)
