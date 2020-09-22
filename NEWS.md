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
