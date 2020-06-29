pkgload::load_all()

## Number of samples
n <- 1e3

## Dimension
d <- 15

## Parameters for the Poisson distribution
alpha <- 0.5
x0 <- 5e-4
intensities <- intensities_pareto(d, alpha, x0)
ex_intensities <- ex_intensities_pareto(d, alpha, x0)

bench::mark(
  Arnold = rmo:::Rcpp__rmo_arnold(
    n, d, intensities=intensities),
  Ex_Arnold = rmo:::Rcpp__rmo_ex_arnold(
    n, d, ex_intensities=ex_intensities),
  LFM = rmo:::Rcpp__rmo_lfm_cpp(
    n, d, 1, 0, 0, "rpareto", list("alpha"=alpha, "x0"=x0)),
  min_iterations = 100L,
  check=FALSE
)
