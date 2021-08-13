
<!-- README.md is generated from README.Rmd. Please edit that file -->

# empire: Estimation of Missing Data using Penalized Iterative Regressions

<!-- badges: start -->

[![R-CMD-check](https://github.com/davidbiol/empire/workflows/R-CMD-check/badge.svg)](https://github.com/davidbiol/empire/actions)
[![Codecov test
coverage](https://codecov.io/gh/davidbiol/empire/branch/master/graph/badge.svg)](https://codecov.io/gh/davidbiol/empire?branch=master)
<!-- badges: end -->

‘empire’ is an R package that provides a set of functions to handle
missing values, since intransitive imputation (‘impute\_mean’,
‘impute\_median’), to transitive imputation (‘estimate\_mlr’) applying
Ridge penalization (‘estimate\_ridge’).

There are also some descriptive functions for a better understanding of
your data set. For example, ‘count\_miss’ gives you the number of
missing values in your data. ‘pos\_miss’ gives you the position of the
missing values inside your data set.

At last, but not least important, there are some useful functions to
simulate data that can be used to look for the effects of missing values
in a data set. For example, ‘sim\_distr’ is an amazing function that can
simulate data variables with the distribution you want.

## Installation

We have not a released version yet, however, you can install the
development version of ‘empire’:

``` r
# install.packages("remotes") # in case you don't have installed it
remotes::install_github("davidbiol/empire")
```
