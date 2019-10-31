
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmo

<!-- badges: start -->

![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.6.1-6666ff.svg)
![packageversion](https://img.shields.io/badge/Package%20version-0.0.0.9000-orange.svg?style=flat-square)
[![Travis build
status](https://travis-ci.org/hsloot/rmo.svg?branch=master)](https://travis-ci.org/hsloot/rmo)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/hsloot/rmo?branch=master&svg=true)](https://ci.appveyor.com/project/hsloot/rmo)
[![Codecov test
coverage](https://codecov.io/gh/hsloot/rmo/branch/master/graph/badge.svg)](https://codecov.io/gh/hsloot/rmo?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Last-changedate](https://img.shields.io/badge/last%20change-2019--11--01-yellowgreen.svg)](/commits/master)
<!-- badges: end -->

An R package for the *Marshall-Olkin distribution*: algorithms for the
construction, simulation and estimation.

## Motivation

While the academic literature on the Marshall-Olkin distributions is
extensive, an intuitive and easy to use implementation is still missing.
This project aims to provide an `R` package makes it simple and fun to
use Marshall-Olkin distributions for research and real-world
applications.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hsloot/rmo")
```

## Example

``` r
# library(rmo)
rmo_esm(n = 10, d = 2, intensities = c(1, 1, 1))
#>             [,1]       [,2]
#>  [1,] 0.34169337 0.30427171
#>  [2,] 0.87884026 0.24449081
#>  [3,] 0.43552687 0.43552687
#>  [4,] 0.19583119 0.19583119
#>  [5,] 0.08550973 0.35980678
#>  [6,] 0.36669012 0.47580137
#>  [7,] 0.48359983 0.35594019
#>  [8,] 0.09656357 0.09656357
#>  [9,] 0.33930297 0.57129464
#> [10,] 0.40113901 0.40113901
```

## License

MIT [Henrik Sloot]()
