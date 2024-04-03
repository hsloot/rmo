#' ## Only killing and drift ##
#'
#' For the CPP LFM parametrization with *no* jump, but drift and killing, we
#' chose the parameters \( \alpha = 0.6 \) (drift) and \( \beta = 0.2 \) (rate
#' killing). #+ r parameters
n <- 1e3
d <- 15

alpha <- 0.6
beta <- 0.2

bf <- rmo::SumOfBernsteinFunctions(
  first = rmo::ConstantBernsteinFunction(constant = alpha),
  second = rmo::LinearBernsteinFunction(scale = beta)
)
intensities <- rmo::intensities(bf, d)
ex_intensities <- rmo::exIntensities(bf, d)

#+ r bench
bench::mark(
    Armageddon = rmo:::Rcpp__rarmextmo_esm(n, d, alpha, beta),
    ExMarkovian = rmo:::Rcpp__rexmo_mdcm(n, d, ex_intensities),
    LFM = rmo:::Rcpp__rextmo_lfm(
        n, d, 0, beta, alpha, "rposval", list("value" = 1)),
    Arnold = rmo:::Rcpp__rmo_am(n, d, intensities),
    ESM = rmo:::Rcpp__rmo_esm(n, d, intensities),
    min_iterations = 100L,
    check = FALSE)
