#+ r load-all
pkgload::load_all()

#' ## Pareto jump CPP ##
#'
#' For the CPP LFM parameterisation with *Pareto* jumps, we chose the parameters \( \lambda = 1 \),
#' \( \alpha = 0.5 \), and \( x_0 = 5e-4 \). This corresponds to a
#' mean-jump-value of approximately \( 0.07 \).
#+ r parameters
n <- 1e3
d <- 125

lambda <- 1
alpha <- 0.5
x0 <- 5e-4

ex_intensities <- lambda * ex_intensities_pareto(d, alpha, x0)

#+ r bench
bench::mark(
  Ex_Arnold = rmo:::Rcpp__rmo_ex_arnold(
    n, d, ex_intensities=ex_intensities),
  LFM = rmo:::Rcpp__rmo_lfm_cpp(
    n, d, lambda, 0, 0, "rpareto", list("alpha"=alpha, "x0"=x0)),
  min_iterations = 100L,
  check=FALSE)
