#' ## Pareto jump CPP ##
#'
#' For the CPP LFM parametrisation with *Pareto* jumps, we chose the parameters \( \lambda = 1 \),
#' \( \alpha = 0.5 \), and \( x_0 = 5e-4 \). This corresponds to a
#' mean-jump-value of approximately \( 0.07 \).
#+ r parameters
n <- 1e3
d <- 15

lambda <- 1
alpha <- 0.5
x0 <- 5e-4

intensities <- lambda * rmo::intensities_pareto(d, alpha, x0)
ex_intensities <- lambda * rmo::ex_intensities_pareto(d, alpha, x0)

#+ r bench
bench::mark(
    Arnold = rmo:::Rcpp__rmo_am(
        n, d, intensities = intensities),
    ExMarkovian = rmo:::Rcpp__rexmo_mdcm(
        n, d, ex_intensities = ex_intensities),
    LFM = rmo:::Rcpp__rextmo_lfm(
        n, d, lambda, 0, 0, "rpareto", list("alpha" = alpha, "x0" = x0)),
    min_iterations = 100L,
    check = FALSE)
