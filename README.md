
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Modeling two-choice decision behavior in R

<!-- badges: start -->

<!-- badges: end -->

See [Suthaharan, P., Corlett, P.R., & Ang, Y.S. (2021). Computational
modeling of behavioral tasks: An illustration from a classic
reinforcement learning paradigm]() for more detail.

Questions? Contact Praveen Suthaharan (<praveen.suthaharan@yale.edu>).

—————

The goal of *twochoiceRL* is to educate users on how to simulate
two-choice decision behavior and, subsequently, take the simulated data
and estimate the behavioral
parameters.

## Installation

<!-- You can install the released version of twochoiceRL from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("twochoiceRL") -->

<!-- ``` -->

You can install the development version of *twochoiceRL* from
[GitHub](https://github.com/psuthaharan/twochoiceRL) with:

``` r
# install.packages("devtools")
devtools::install_github("psuthaharan/twochoiceRL")
```

## Simulation

Simulate behavior:

``` r
library(twochoiceRL)
# generate 100 individuals
# dataset1 contains behavioral data keeping only those selected-choice trials
dataset1 <- simulate_twochoiceRL(n_subj = 100, n_tr = 200, trials_unique = TRUE)

# dataset2 contains behavioral data keeping all trials (selected-choice and the non-selected-choice)
dataset2 <- simulate_twochoiceRL(n_subj = 100, n_tr = 200, trials_unique = FALSE)
```

## Visualization

Plot behavior:

``` r
# for plotting purpose - let's use dataset2
# You randomly select a participant to observe his/her behavior

# Participant 100
# View first 10 rows of data
head(dataset2$twochoiceRL[[100]],10)
#>    Trial        Value        Pr Option Action      Reward
#> 1      1  0.011472946 0.5000000      1      1  0.07437611
#> 2      1  0.000000000 0.5000000      2      1  0.07437611
#> 3      2  0.150651408 0.5024829      1      1  0.91373052
#> 4      2  0.000000000 0.4975171      2      1  0.91373052
#> 5      3  0.150651408 0.5325572      1      2 -1.39233659
#> 6      3 -0.214775990 0.4674428      2      2 -1.39233659
#> 7      4  0.028651131 0.5784314      1      1 -0.64024449
#> 8      4 -0.214775990 0.4215686      2      1 -0.64024449
#> 9      5 -0.001389443 0.5524874      1      1 -0.16609406
#> 10     5 -0.214775990 0.4475126      2      1 -0.16609406


# Visualize behavior 
plot_twochoiceRL(data = dataset2, subj = 100, colors = c("#749dae","#5c1a33"))
```

<img src="man/figures/README-plot-2.png" width="100%" />

``` r

# Visualize behavior - animated 
plot_twochoiceRL(data = dataset2, subj = 100, colors = c("#749dae","#5c1a33"), plot_type = "animate")
```

<img src="man/figures/README-plot-1.gif" width="100%" />

## Estimation

Estimate behavioral parameters from simulated data:

``` r
# Run MLE (elapsed time: ~ 6 min)
estimate_twochoiceRL(data = dataset1, method = "mle", plot = TRUE)
```

<img src="man/figures/README-estimation-1.png" width="100%" />

``` r

# Run MAP (elapsed time: ~ 5 min)
estimate_twochoiceRL(data = dataset1, method = "map", plot = TRUE)
```

<img src="man/figures/README-estimation-2.png" width="100%" />

``` r

# Run EML (elapsed time: ~ 1.4 hours)
estimate_twochoiceRL(data = dataset1, method = "eml", plot = TRUE)
```

<img src="man/figures/README-estimation-3.png" width="100%" />