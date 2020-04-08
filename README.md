
<!--  github_document:
    pandoc_args: --webtex -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# psvar

<!-- badges: start -->

<!-- badges: end -->

``` r
library(psvar)
model <- estimate_var(ramey_econ214[, c(5, 2, 4, 6)], p = 12) 
irfs <- irf_psvar(model, ramey_econ214[, 10], irhor = 50)
plot_psvar(irfs)
```

<img src="man/figures/README-use-case-1.png" width="100%" />

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("kvasilopoulos/psvar")
```
