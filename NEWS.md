# rmo 0.9

- Rename multiple S4-methods and arguments (#121):
  - Rename S4-method `exIntensities()` to `calcExShockSizeArrivalIntensities()`
    and arguments/variables `ex_intensities` to `theta`
  - Rename S4-method `uexIntensities()` to `calcExShockArrivalIntensities()`
  - Rename S4-method `exQMatrix()` to `calcMDCMGeneratorMatrix()`
  - Rename S4-method `intensities()` to `calcShockArrivalIntensities()`
  - Rename S4-method `defaultMethod()` to `getDefaultMethodString()`
  - Rename S4-method `levyDensity()` to `getLevyDensity()`
  - Rename S4-method `stieltjesDensity()` to `getStieltjesDensity()`
  - Rename S4-method `valueOf()` to `calcIterativeDifference()`
  - Rename S4-method `valueOf0()` to `calcValue()`
  - Rename various variable, method, and argument names appropriately

- Improve extensibility by exposing `defaultMethod` and `valueOf0` (#118)

- Review and improve documentation (#116)

- Re-organize S4 code and documentation (#117)

- Remove outdated rmarkdown notebooks from development phase (#115)

- Remove convenience wrapper functions for generating shock arrival intensity
  and shock-size arrival intensity parameter (#114)

- Add `ConvexCombinationOfBernsteinFunctions` to represent convex combinations
  of Bernstein functions compactly

- Small changes of the documentation

- Improving implementation of method `exIntensities`

- Provide zero-sample-size parametrization, `n = 0`, in simulation algorithms
  (e.g. for measure setup time)

- Bugfix for consistent use of STL headers and attribute [[maybe_unused]] in C++
  code

# rmo 0.8

- **Breaking change**: Rename simulation method `*_markovian` to `*_mdcm` and
  `*_arnold` to `*_am`

- **Breaking change**: Provide a single entry point method for general MO
  sampling distributions `rmo`, exchangeable MO sampling routine `rexmo`,
  extendible MO sampling routines `rextmo`, and parametrized extendible MO
  sampling routines `rpextmo`

- Rename C++ backend classes `arnold_mo_distribution`,
`markovian_exmo_distribution`, and `armageddon_extmo_distribution` to
`am_mo_distribution`, `mdcm_exmo_distribution`, and `esm_armextmo_distribution`,
respectively

- Improve code coverage by adding snapshot tests for sampling routines

- Improve documentation

# rmo 0.7

- **Breaking change**: Remove `lambda` parameter for `PoissonBernsteinFunction`

# rmo 0.6

- **Breaking change**: Rename concept *Cuadras-Augé* to *armageddon ESM* to
  better reflect the nature of the distribution. Associated functions are
  renamed accordingly

- Bugfix to handle integration error explicitly, possibly adjusting in case of
  for very small values

- Improve numerical stability by using the explicit first-order iterated
  difference for `difference_order == 1L` in `valueOf` to avoid
  endpoint-singularity problems in numerical integration

- Bugfix in initializers and validity methods of S4 objects such that
  `validObject` can now be called with the argument `test = TRUE` without
  causing an error

- Allow pass-though of arguments `uexIntensities`, `exIntensities`, and
  `exQMatrix` from method `valueOf` to `integrate`

- Add new `CompositeScaledBernsteinFunction` class

- Add `show` method for `BernsteinFunction`-classes.

# rmo 0.5

- **Breaking change**: Rename function names of simulation algorithms. Now,
  sampling algorithms follow the format `r*mo_*` where the first `*` indicates
  the input-parameter and the second on the algorithm, e.g. `rexmo_markovian`
  has `ex_intensities` as input parameters and uses the Markovian model for the
  default counting process.

- **Breaking change**: Rearrange order in arguments of `valueOf`: instead of
  `cscale, n, k` we have `n, k, cscale`.

- Implementing new methods to generate distribution parameter from
  `BernsteinFunction` classes

# rmo 0.4

- Change input parameter for `rexmo_markovian` (which is now is scaled
  exchangeable intensity). The `ex_intensities*`-methods are similarly adjusted
  such that there should be no change necessary if these functions were used
  to create the input parameter for `rexmo_markovian`.
- The Bernstein function classes have been refactored and some new features have
  been added.

# rmo 0.3

- **Breaking change**: Rename `rmo_ex_arnold` to `rexmo_markovian`.

- Improve numerical stability of the calculation of products with large binomial
  coefficients

- Bugfix in function `is_within` to avoid undefined behavior

- Implementing the Inverse Gaussian Bernstein function

- Implementing Pareto jump simulation and the Pareto-jump compound Poisson
  Bernstein function

- Implementing the Exponential-jump compound Poisson Bernstein function

- Add S4 classes for evaluating Bernstein Functions and their higher-order
  alternating, iterated forward differences

- Provide drop-in wrapper functions to create meaningful distribution parameters

- Refactoring of C++ backend to distribution classes satisfying a multivariate
  version of the named requirement *RandomNumberDistribution*

- Refactoring of several internal functions by rewriting them in C++

- Refactoring of simulation algorithms to improve performance

- Refactoring to improve internal representation

- Refactoring of custom assertions

- Improve code coverage, e.g., by adding additional unit tests, an integration
  test, and statistical unit tests

# rmo 0.2

- Re-License under GPL-3 (because of `Rcpp` dependence)

- Properly handle case `rate == 0` in `rmo:::sample_cpp` (used in `rmo_lfm_cpp`)

- Properly handle case when compound Poisson process drifts over several
  barriers during waiting period in `rmo:::sample_cpp` (used in `rmo_lfm_cpp`)

- Re-implementation of sampling algorithms using `Rcpp`

- Use original `R`-based implementations of simulation algorithms for unit tests

- Improve code coverage for `rmo_lfm_cpp` (independence case)

# rmo 0.1

- Added pure `R` implementations of various sampling algorithms: `rmo_esm`,
`rmo_arnold`, `rmo_ex_arnold`, `rmo_lfm_cpp`, and `rmo_esm_cuadras_auge`.
