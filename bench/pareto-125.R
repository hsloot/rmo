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

bf <- ScaledBernsteinFunction(
  scale = lambda,
  original = ParetoBernsteinFunction(alpha = alpha, x0 = x0)
)
ex_intensities <- exIntensities(bf, d)

#+ r bench
mark(
  ExMarkovian = rexmo(n, d, ex_intensities, method = "MDCM"),
  LFM = rpextmo(
    n, d, gamma = lambda,
    eta = c("alpha" = alpha, "x0" = x0), family = "Pareto",
    method = "LFM"
  ),
  min_iterations = 100L,
  check = FALSE
)
