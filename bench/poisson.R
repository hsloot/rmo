#+ r load-all
pkgload::load_all()

#' ## Poisson jump CPP ##
#'
#' For the CPP LFM parameterisation with *fixed size* jumps, we chose the
#' parameters \( \lambda = 1 \) and \( \eta = 0.5 \).  This corresponds to a
#' mean-jump-size of \( 0.5 \).
#+ r parameters
n <- 1e3
d <- 15

lambda <- 1
eta <- 0.5

intensities <- intensities_poisson(d, lambda, eta)
ex_intensities <- ex_intensities_poisson(d, lambda, eta)

#+ r bench
bench::mark(
  Arnold = rmo:::Rcpp__rmo_arnold(
    n, d, intensities=intensities),
  ExMarkovian = rmo:::Rcpp__rexmo_markovian(
    n, d, ex_intensities=ex_intensities),
  LFM = rmo:::Rcpp__rmo_lfm_cpp(
    n, d, lambda, 0, 0, "rposval", list("value"=eta)),
  min_iterations = 100L,
  check=FALSE)
