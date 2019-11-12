
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
[![Last-changedate](https://img.shields.io/badge/last%20change-2019--11--12-yellowgreen.svg)](/commits/master)
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
#>  [1,] 0.88915640 0.26291879
#>  [2,] 0.02429431 0.02429431
#>  [3,] 0.52780143 0.01175364
#>  [4,] 0.47485603 0.21375354
#>  [5,] 0.23147035 0.06674164
#>  [6,] 0.21255479 0.01682099
#>  [7,] 0.88522365 0.76053695
#>  [8,] 1.35217610 0.39032035
#>  [9,] 1.01788729 1.37484129
#> [10,] 0.36871786 0.27676996
```

## Contributing

Contribution is highly appreciated. Contribution can range from
improving the documentation, writing tests, or raising issues and
feature requests to implementing feature requests or fixing bugs. If you
consider to contribute, have a look at our [contribution
guide](.github/CONTRIBUTING.md).

## License

MIT [Henrik Sloot]()
