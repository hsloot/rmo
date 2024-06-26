#' ## Poisson jump CPP ##
#'
#' For the CPP LFM parametrization with *fixed size* jumps, we chose the
#' parameters \( \lambda = 1 \) and \( \eta = 0.5 \).  This corresponds to a
#' mean-jump-size of \( 0.5 \).

#+ r parameters
n <- 1e3
d <- 15

eta <- 0.5

bf <- ScaledBernsteinFunction(
  scale = 1,
  original = PoissonBernsteinFunction(eta = eta)
)
lambda <- calcShockArrivalIntensities(bf, d)
theta <- calcExShockSizeArrivalIntensities(bf, d)

#+ r bench
mark(
  Arnold = rmo(n, d, lambda, method = "AM"),
  ExMarkovian = rexmo(n, d, theta, method = "MDCM"),
  LFM = rpextmo(n, d, eta = eta, family = "Poisson", method = "LFM"),
  min_iterations = 100L,
  check = FALSE
)
