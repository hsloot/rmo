
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmo

<!-- badges: start -->

![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.4.0-6666ff.svg)
![packageversion](https://img.shields.io/badge/Package%20version-0.9-orange.svg?style=flat-square)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Last-changedate](https://img.shields.io/badge/last%20change-2024--04--29-yellowgreen.svg)](/commits/master)
[![R build
status](https://github.com/hsloot/rmo/workflows/check-full/badge.svg)](https://github.com/hsloot/rmo/actions)
[![Codecov test
coverage](https://codecov.io/gh/hsloot/rmo/branch/master/graph/badge.svg)](https://codecov.io/gh/hsloot/rmo?branch=main)
<!-- badges: end -->

An R package for constructing and simulating high-dimensional
*Marshall-Olkin (MO) distributions*, making it simple and fun to use
them for research and real-world applications. Read more about
simulating high-dimensional MO distributions in (Sloot 2022).

## Installation

You can install the development version from GitHub using the `devtools`
package with:

``` r
# install.packages("devtools")
devtools::install_github("hsloot/rmo")
```

## Basic usage

Simulating high-dimensional Marshall–Olkin distributions is made simple
with the `rpextmo()` function. Here is a basic example:

``` r
rpextmo(
  n = 10,                # number of samples
  d = 3,                 # dimension
  eta = log2(2 - 0.5),   # distribution family parameter
  family = "AlphaStable" # distribution family
)
#>             [,1]      [,2]       [,3]
#>  [1,] 1.43771730 1.4377173 1.43771730
#>  [2,] 2.05209628 1.3530518 1.91148856
#>  [3,] 0.57433836 1.3811189 1.90247610
#>  [4,] 0.75080505 1.1922738 0.54154504
#>  [5,] 1.71072303 1.7107230 0.25935362
#>  [6,] 0.50637200 0.5063720 0.50637200
#>  [7,] 1.70689790 1.7068979 0.43169613
#>  [8,] 0.07250493 0.7784406 0.07250493
#>  [9,] 1.83458658 1.3509871 1.83458658
#> [10,] 2.57508782 0.1130284 0.91314202
```

## Advanced usage

All parametric families are linked to *so-called* Bernstein functions.
The class of Bernstein functions is closed under addition, scaling, and
composite scaling. You can create new Bernstein functions and simulate
from them. Here is an example:

``` r
# Create a custom Bernstein function parametrization
alpha <- log2(2 - 0.5)
x0 <- 5e-4
bf <- SumOfBernsteinFunctions(
  first = LinearBernsteinFunction(
    scale = alpha * x0^(1 - alpha) / (1 - alpha) / gamma(1 - alpha)
  ),
  second = ScaledBernsteinFunction(
    scale = x0^(-alpha) / gamma(1 - alpha),
    original = ParetoBernsteinFunction(
      alpha = alpha,
      x0 = x0
    )
  )
)

# Simulate from the custom Bernstein function parametrization
rextmo(n = 10, d = 3, bf = bf)
#>            [,1]      [,2]      [,3]
#>  [1,] 0.8633888 1.3353381 1.3353381
#>  [2,] 2.1615788 2.1615788 2.1615788
#>  [3,] 0.2844054 0.4893745 0.4893745
#>  [4,] 1.3552345 1.3552345 0.6377810
#>  [5,] 0.1877915 0.1877915 0.1877915
#>  [6,] 0.1706188 0.1706188 0.1706188
#>  [7,] 0.4699766 3.4625694 1.3128645
#>  [8,] 0.1841238 0.1841238 0.1841238
#>  [9,] 2.5726964 1.4836676 0.7034619
#> [10,] 1.4657382 0.5681614 1.6458015
```

## Word(s) of caution

While the package is extensively tested, numerical issues may arise in
high-dimensional simulations due to very small or large numbers. We
recommend performing statistical tests suitable for your use-case to
ensure validity. For more guidance, refer to our [statistical unit
tests](https://github.com/hsloot/rmo/blob/main/tests/testthat/test-statistical-unit-test.R)
and submit an [issue
report](https://github.com/hsloot/rmo/issues/new?assignees=&labels=bug&template=statistical_problem.md&title=%5BSTAT%5D)
if you encounter statistical problems.

Additionally, note that while the package reached a certain level of
stability, it is still under active development. We aim to keep the
interface of the primary function `rpextmo()` backwards compatible, the
implementation may undergo changes.

## API documentation

The package is documented using `roxygen2` and `pkgdown`. You can find
the documentation on the [project website](reference/index.html).

## Testing

The package is tested using `testthat`. You can run the tests using the
following command after checking out the repository:

``` r
devtools::test()
```

## Changelog

The changelog is available in the [NEWS.md](NEWS.md) file.

## Contributing

Contributions to `rmo` are welcome! You contribution can be about
improving the documentation, writing tests, raising issues or feature
requests, implementing feature requests, or fixing bugs. Check out our
[contribution guide](.github/CONTRIBUTING.md) to get started.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-Sloot2022a" class="csl-entry">

Sloot, Henrik. 2022. “Implementing Markovian Models for Extendible
Marshall–Olkin Distributions.” *Dependence Modeling* 10 (1): 308–43.
<https://doi.org/10.1515/demo-2022-0151>.

</div>

</div>

## License

GPL-3 Henrik Sloot
