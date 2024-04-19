#' ## Pareto jump CPP ##
#'
#' For the CPP LFM parametrization with *Pareto* jumps, we chose the parameters
#' \( \lambda = 1 \), \( \alpha = 0.5 \), and \( x_0 = 5e-4 \). This corresponds
#' to a mean-jump-value of approximately \( 0.07 \).

#+ r parameters
n <- 1e3
d <- 125

lambda <- 1
alpha <- 0.5
x0 <- 5e-4

bf <- rmo::ScaledBernsteinFunction(
  scale = lambda,
  original = rmo::ParetoBernsteinFunction(alpha = alpha, x0 = x0)
)
ex_intensities <- rmo::exIntensities(bf, d)

#+ r bench
mark(
  ExMarkovian = rmo:::Rcpp__rexmo_mdcm(
      n, d, ex_intensities = ex_intensities),
  LFM = rmo:::Rcpp__rextmo_lfm(
      n, d, lambda, 0, 0, "rpareto", list("alpha" = alpha, "x0" = x0)),
  min_iterations = 100L,
  check = FALSE
)
