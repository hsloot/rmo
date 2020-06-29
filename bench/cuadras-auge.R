pkgload::load_all()

## Number of samples
n <- 1e3

## Dimension
d <- 15

## Parameters for the Cuadras-Augé distribution
alpha <- 0.6
beta <- 0.2
intensities <- intensities_cuadras_auge(d, alpha, beta)
ex_intensities <- ex_intensities_cuadras_auge(d, alpha, beta)

## Run benchmark
bench::mark(
  CuadrasAuge = rmo:::Rcpp__rmo_esm_cuadras_auge(n, d, alpha, beta),
  ExArnold = rmo:::Rcpp__rmo_ex_arnold(n, d, ex_intensities),
  LFM = rmo:::Rcpp__rmo_lfm_cpp(
    n, d, 0, beta, alpha, "rposval", list("value" = 1)),
  Arnold = rmo:::Rcpp__rmo_arnold(n, d, intensities),
  ESM = rmo:::Rcpp__rmo_esm( n, d, intensities),
  min_iterations = 100L,
  check=FALSE
)
