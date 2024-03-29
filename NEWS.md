# rmo 0.8.3

- Add `ConvexCombinationOfBernsteinFunctions` to represent convex combinations
  of Bernstein functions compactly.

# rmo 0.8.2

- bump version

# rmo 0.8.1

- Small changes in the documentation
- Improvement of `exIntensities`
- Allow `n = 0` in sampling algorithms (e.g. for measure setup time)
- Consistent use of STL headers and attribute [[maybe_unused]] in C++ code

# rmo 0.8

- Create single entry point method for general MO sampling distributions, exchangeable MO sampling
  routines, extendible MO sampling routines, and parametrized extendible MO sampling routines.
- Improve documentation and add snaphot test for sampling routines.

# rmo 0.7.1

- Rename sampling method `*_markovian` and `*_arnold` to `*_mdcm` and `*_am`, respectively.
- Rename internal CPP classes `arnold_mo_distribution`, `markovian_exmo_distribution`, and `armageddon_extmo_distribution` to `am_mo_distribution`, `mdcm_exmo_distribution`, and `esm_armextmo_distribution`, respectively.

# rmo 0.7

- Breaking change: remove `lambda` parameter for `PoissonBernsteinFunction`.

# rmo 0.6

- Rename *Cuadras-Augé* to *armageddon ESM* to better reflect the nature of the distribution.

# rmo 0.5.5

- Explicitly handle integration error with possible adjustment for very small values.

# rmo 0.5.4

- Use explicit first-order iterated difference for `difference_order == 1L` in `valueOf` to
  avoid endpoint-singularity problems in numerical integration.

# rmo 0.5.3

- Fix implementation of initializers and validity methods. In particular, `validObject` can now
  be called with the argument `test = TRUE` without causing an error.
- Add show method for  `BernsteinFunction`-classes.

# rmo 0.5.2

- Allow pass-though of arguments to `integrate` in functions `valueOf`,
  `uexIntensities`, `exIntensities`, and `exQMatrix`.

# rmo 0.5.1

- Add composite-scaled Bernstein function class.

# rmo 0.5.0

- Rename Cuadras-Augé and Lévy-frailty model algorithms. Now a sampling algorithm
  follows the format `r*mo_*` where the first `*` indicates the input-parameter
  and the second on the algorithm, e.g. `rexmo_markovian` has `ex_intensities` as
  input parameters and uses the Markovian model for the default counting process.

# rmo 0.4.1

- Rearrange order in arguments of `valueOf`: instead of `cscale, n, k` we
  have `n, k, cscale`.
- Add methods to generate parameter from `BernsteinFunction` classes

# rmo 0.4.0

- Change input parameter for `rexmo_markovian` (which is now is scaled
  exchangeable intensity). The `ex_intensities*`-methods are similarly adjusted
  such that there should be no change necessary if these functions were used
  to create the input parameter for `rexmo_markovian`.
- The Bernstein function classes have been refactored and some new features have
  been added.

# rmo 0.3.0

- Rename `rmo_ex_arnold` to `rexmo_markovian`.

# rmo 0.2.6

- Complete refactored C++ backend: Distribution classes satisfy a multivariate
  version of the named requirement *RandomNumberDistribution*

# rmo 0.2.5

- Include more tests
- Include statistical tests for an integration test
- Fix problem with large binomial coefficients

# rmo 0.2.4

- Implementation of the Inverse Gaussian Bernstein function

# rmo 0.2.3

- Implement Pareto jumps and Pareto CPP Bernstein function
- Implement Exponential-jump CPP Bernstein function

# rmo 0.2.2

- Improve internal representation
- Internal support library now entirely written in C++
- Make LFM more extendible

# rmo 0.2.1

- Refactoring and additional tests
- Fix problem with `int32` is `is_within` function
- Refactor custom assertions
- Add S4 classes for evaluating Bernstein Functions and their higher-order
    alternating, iterated forward differences
- Provide simple functions to create meaningful distribution parameters
- Better test cases and increased test coverage
- Refactor all sampling methods for increased performance

# rmo 0.2.0

- Reimplementation of all sampling algorithms in `Rcpp`
- Changed License to GPL-3 (because of `Rcpp`-package)
- Extended unit testing with original `R`-based implementations


# rmo 0.1.2

- Bugfix for `rmo:::sample_cpp` which is used in `rmo_lfm_cpp`. The former
implementation did not properly account for the case, when the CPP drifts
over several barriers during a waiting period.


# rmo 0.1.1

- Bugfix for `rmo:::sample_cpp` which is used in `rmo_lfm_cpp` (case `rate == 0` was not properly handled).
- Implemented more test for `rmo_lfm_cpp` in independence case

# rmo 0.1.0

- Added pure `R` implementations of various sampling algorithms: `rmo_esm`,
`rmo_arnold`, `rmo_ex_arnold`, `rmo_lfm_cpp`, and `rmo_esm_cuadras_auge`.


# rmo 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
- Sep. 28, 2019: I initialized this project today. As of now, there is no code in the repository. The next goal is to provide the necessary background documentation on the Marshall-Olkin distribution and outline a project plan. Stay tuned.
