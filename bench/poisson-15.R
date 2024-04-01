#' ## Poisson jump CPP ##
#'
#' For the CPP LFM parametrization with *fixed size* jumps, we chose the
#' parameters \( \lambda = 1 \) and \( \eta = 0.5 \).  This corresponds to a
#' mean-jump-size of \( 0.5 \).

#+ r parameters
n <- 1e3
d <- 15

eta <- 0.5

bf <- rmo::ScaledBernsteinFunction(
  scale = 1,
  original = rmo::PoissonBernsteinFunction(eta = eta)
)
intensities <- rmo::intensities(bf, d)
ex_intensities <- rmo::exIntensities(bf, d)

#+ r bench
bench::mark(
  Arnold = rmo:::Rcpp__rmo_am(
    n, d, intensities = intensities
  ),
  ExMarkovian = rmo:::Rcpp__rexmo_mdcm(
    n, d, ex_intensities = ex_intensities
  ),
  LFM = rmo:::Rcpp__rextmo_lfm(
    n, d, 1, 0, 0, "rposval", list("value" = eta)
  ),
  min_iterations = 100L,
  check = FALSE
)
