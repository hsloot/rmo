
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
[![Last-changedate](https://img.shields.io/badge/last%20change-2019--10--03-yellowgreen.svg)](/commits/master)
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
#>             [,1]        [,2]
#>  [1,] 0.79519697 0.356683255
#>  [2,] 0.32481697 1.514070703
#>  [3,] 0.45829880 2.580079639
#>  [4,] 2.55953378 1.047198195
#>  [5,] 0.29107779 0.291077792
#>  [6,] 0.76341925 0.356921768
#>  [7,] 0.47154177 0.316698912
#>  [8,] 0.07369858 0.073698583
#>  [9,] 0.40066036 0.007679787
#> [10,] 0.63629435 0.856070328
```

## License

MIT [Henrik Sloot]()
