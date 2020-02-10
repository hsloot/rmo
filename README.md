
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmo

<!-- badges: start -->

![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.6.1-6666ff.svg)
![packageversion](https://img.shields.io/badge/Package%20version-0.1.2.0000-orange.svg?style=flat-square)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Travis build
status](https://travis-ci.org/hsloot/rmo.svg?branch=master)](https://travis-ci.org/hsloot/rmo)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/hsloot/rmo?branch=master&svg=true)](https://ci.appveyor.com/project/hsloot/rmo)
[![Codecov test
coverage](https://codecov.io/gh/hsloot/rmo/branch/master/graph/badge.svg)](https://codecov.io/gh/hsloot/rmo?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--02--10-yellowgreen.svg)](/commits/master)
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
rmo_esm(n=10L, d=2L, intensities=c(1, 1, 1))
#>              [,1]        [,2]
#>  [1,] 0.220396393 0.220396393
#>  [2,] 0.268882997 0.702705410
#>  [3,] 0.071347906 0.071347906
#>  [4,] 0.185663070 0.185663070
#>  [5,] 0.007869681 0.007869681
#>  [6,] 0.441468786 0.441468786
#>  [7,] 0.936079929 0.493162846
#>  [8,] 2.060125742 0.691868769
#>  [9,] 0.220026223 1.290835243
#> [10,] 0.035544062 1.518556226
```

## Roadmap for future development

We are planning to develop the package incrementally. The packages API
might change frequently without deprecation. The goal is to implement
prototypes of the algorithms in `R` and refactor them in compiled code
afterwards. The original `R` implementations are supposed to serve as
test functions in the unit-test harness. Our next steps are:

  - **Version 0.2**: Re-implementation of these sampling routines in
    `C++` with `Rcpp` to improve performance.

Other ideas for the future:

  - Implementation of estimation routines (**help wanted**).
  - An *OOP* implementation of the Marshall–Olkin distribution with the
    ultimate goal to facilitate a hierarchical construction of
    Marshall–Olkin distributions.

## Contributing

Contribution is highly appreciated. Contribution can range from
improving the documentation, writing tests, or raising issues and
feature requests to implementing feature requests or fixing bugs. If you
consider to contribute, have a look at our [contribution
guide](.github/CONTRIBUTING.md).

## License

MIT Henrik Sloot
