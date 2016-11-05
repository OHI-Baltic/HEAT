Oxygen Debt Indicator Calculation Scripts
-----------------------------------------

The following as an example of fitting the HEAT oxygen debt profile models to CTD or water bottle data. The code below assumes that the repository has been cloned, and that the data has been downloaded and prepared via. This will create the folder `model` and the file \`model/input.csv'

``` r
source("scripts/OxygenDebt/01_input.R")
```

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
