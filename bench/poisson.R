pkgload::load_all()

## Number of samples
n <- 1e3

## Dimension
d <- 15

## Parameters for the Poisson distribution
lambda <- 1
eta <- 0.5
intensities <- intensities_poisson(d, lambda, eta)
ex_intensities <- ex_intensities_poisson(d, lambda, eta)

bench::mark(
  Arnold = rmo:::Rcpp__rmo_arnold(
    n, d, intensities=intensities),
  Ex_Arnold = rmo:::Rcpp__rmo_ex_arnold(
    n, d, ex_intensities=ex_intensities),
  LFM = rmo:::Rcpp__rmo_lfm_cpp(
    n, d, lambda, 0, 0, "rposval", list("value"=eta)),
  min_iterations = 100L,
  check=FALSE
)
