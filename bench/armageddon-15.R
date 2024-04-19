#' ## Only killing and drift ##
#'
#' For the CPP LFM parametrization with *no* jump, but drift and killing, we
#' chose the parameters \( \alpha = 0.6 \) (drift) and \( \beta = 0.2 \) (rate
#' killing).

#+ r parameters
n <- 1e3
d <- 15

alpha <- 0.6
beta <- 0.2

bf <- SumOfBernsteinFunctions(
  first = ConstantBernsteinFunction(constant = alpha),
  second = LinearBernsteinFunction(scale = beta)
)
intensities <- intensities(bf, d)
ex_intensities <- exIntensities(bf, d)

#+ r bench
mark(
  Armageddon = rpextmo(
    n, d, a = alpha, b = beta, family = "Armageddon", method = "ESM"
  ),
  ExMarkovian = rexmo(n, d, ex_intensities, method = "MDCM"),
  LFM = rpextmo(
    n, d, a = alpha, b = beta, family = "Armageddon", method = "LFM"
  ),
  Arnold = rmo(n, d, intensities, method = "AM"),
  ESM = rmo(n, d, intensities, method = "ESM"),
  min_iterations = 100L,
  check = FALSE
)
