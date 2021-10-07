
<!-- README.md is generated from README.Rmd. Please edit that file -->

# empire: Estimation of Missing Data using Penalized Iterative Regressions <a href='https://davidbiol.github.io/empire/'><img src='man/figures/logo.png' align="right" height="139"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/davidbiol/empire/workflows/R-CMD-check/badge.svg)](https://github.com/davidbiol/empire/actions)
[![Codecov test
coverage](https://codecov.io/gh/davidbiol/empire/branch/master/graph/badge.svg)](https://codecov.io/gh/davidbiol/empire?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

empire is an R package that provides a set of functions to handle
missing values, since intransitive imputation (`impute_mean`,
`impute_median`), to transitive imputation like multiple linear
regressions (`estimate_mlr`) and applying Ridge penalization
(`estimate_ridge`).

There are also some descriptive functions for a better understanding of
your data set. For example, `count_miss` gives you the number of missing
values in your data. `pos_miss` gives you the position of the missing
values inside your data set.

At last, but not least important, there are some useful functions to
simulate data that can be used to look for the effects of missing values
in a data set. For example, `sim_distr` is an amazing function that can
simulate data variables with the distribution you want.

## Installation

We have not a released version yet, however, you can install the
development version of empire:

``` r
# install.packages("remotes")
remotes::install_github("davidbiol/empire")
```

## Acknowledgement

*Logo design:* [Maria Camila Vergara
Rodríguez](https://www.instagram.com/camilavergarar/). Thank you for
such an amazing logo, we appreciate your work.
