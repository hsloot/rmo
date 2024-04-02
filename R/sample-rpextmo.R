#' @include sample-rmo.R sample-rexmo.R sample-rextmo.R
NULL

#' Simulate from parametrized families of extendible MO distributions
#'
#' Draws `n` iid `d`-variate samples from a *parametrized family of extendible
#' MO distribution*.
#'
#' @inheritParams rextmo
#' @param a A non-negative double representing the *killing-rate* \eqn{a} of the
#'   *Bernstein function*.
#' @param b A non-negative double representing the *drift* \eqn{b} of the
#'   *Bernstein function*.
#' @param gamma a positive double representing the scaling factor of the
#'   integral part of the *Bernstein function*.
#' @param eta A numeric vector representing the distribution family's
#'   parameters, see the Details section.
#' @param family A string representing the parametrized family.
#'    Use "Armageddon" for the *Armageddon* family, "Poisson" for the
#'    *Poisson family*, "Pareto" for the *Pareto family*, "Exponential" for the
#'    *Exponential family*, "AlphaStable" for the *\eqn{\alpha}-stable family*,
#'    "InverseGaussian" for the *Inverse-Gaussian family*, "Gamma" for
#'    the *Gamma family*, see the Details section.
#' @param method A string representing which sampling algorithm should be used.
#'   Use "MDCM" for the *Markovian death-set model*, "LFM" for the
#'   *Lévy–frailty model*, "AM" for the *Arnold model*, and "ESM" for the
#'   *exogenous shock model* (in case of the *Armageddon family*, the algorithm
#'   is optimized to consider only finite shocks). We recommend using the *ESM*
#'   only for small dimensions; the *AM* can be used up to dimension
#'   \eqn{30}.
#'
#' @return
#' `rpextmo` returns a numeric matrix of size `n` x `d`. Each row corresponds to
#' an independently and identically (iid) distributed sample from a `d`-variate
#' *parametrized extendible Marshall–Olkin distribution* with the specified
#' parameters.
#'
#' @details
#' A *parametrized ext. MO distribution* is a family of *ext. MO distributions*,
#' see [rextmo()], corresponding to *Bernstein functions* of the form
#' \deqn{
#'     \psi{(x)}
#'         = 1_{\{ x > 0 \}} a + b x + \gamma \cdot
#'             \int_{0}^{\infty}{ {[1 - e^{-ux}]} \nu{(\mathrm{d}u)} },
#'                 \quad x \geq 0 ,
#' }
#' or
#' \deqn{
#'    \psi{(x)}
#'        = 1_{\{ x > 0 \}} a + b x + \gamma \cdot
#'            \int_{0}^{\infty}{ \frac{x}{x + u} \sigma{(\mathrm{d}u)} },
#'                \quad x \geq 0 ,
#' }
#' where \eqn{a, b \geq 0}. The \eqn{\nu} and \eqn{\sigma} represent the
#' *Lévy measure* and *Stieltjes measure*, respectively. At least one of the
#' following conditions must hold: \eqn{a > 0}, \eqn{b > 0}, or
#' \eqn{\nu \not\equiv 0} (resp. \eqn{\sigma \not\equiv 0}).
#'
#' ## Families
#'
#' All implemented families are listed in the following; some re-combinations
#' are possible, see [ScaledBernsteinFunction-class],
#' [SumOfBernsteinFunctions-class], and
#' [CompositeScaledBernsteinFunction-class].
#'
#' ### Armageddon family
#' We have \eqn{\nu = \sigma \equiv 0} and Bernstein function
#' \deqn{
#'     \psi{(x)}
#'         = 1_{\{ x > 0\}} a + b x ,
#'             \quad x \geq 0 ,
#' }
#' see [ConstantBernsteinFunction-class] and [LinearBernsteinFunction-class].
#'
#' ### Poisson family
#' We have \eqn{\eta > 0}, Bernstein function
#' \deqn{
#'     \psi{(x)}
#'         = 1_{\{ x > 0\}} a + b x + \gamma \cdot {[1 - e^{-\eta x}]},
#'             \quad x \geq 0 ,
#' }
#' and Lévy measure
#' \deqn{
#'     \nu{(\mathrm{d}u)}
#'         = \delta_{\{ \eta \}}{(\mathrm{d}u)} ,
#' }
#' see [PoissonBernsteinFunction-class].
#'
#' ### Pareto family
#' We have \eqn{\eta \in \mathbb{R}^2} with
#' \eqn{\eta_1 \in {(0, 1)}, \eta_2 > 0} and Lévy measure
#' \deqn{
#'     \nu{(\mathrm{d}u)}
#'         = \eta_{1} \eta_{2}^{\eta_{1}} \cdot
#'          u^{-\eta_{1}-1}  1_{\{ u > \eta_{2}\}} \mathrm{d}u ,
#' }
#' see [ParetoBernsteinFunction-class].
#'
#' ### Exponential family
#' We have \eqn{\eta > 0}, Bernstein function
#' \deqn{
#'     \psi{(x)}
#'         = 1_{\{ x > 0\}} a + b x + \gamma \cdot \frac{x}{x + \eta} ,
#'             \quad x \geq 0 ,
#' }
#' and Lévy measure
#' \deqn{
#'     \nu{(\mathrm{d}u)}
#'         = \eta e^{-\eta u} \mathrm{d}u ,
#' }
#' and Stieltjes measure
#' \deqn{
#'     \sigma{(\mathrm{d}u)}
#'         = \delta_{\{ \eta \}}{(\mathrm{d}u)} ,
#' }
#' see [ExponentialBernsteinFunction-class].
#'
#' ### \eqn{\alpha}-stable family
#' We have \eqn{\eta \in {(0, 1)}}, Bernstein function
#' \deqn{
#'     \psi{(x)}
#'         = 1_{\{ x > 0\}} a + b x + \gamma \cdot x^{\eta} ,
#'             \quad x \geq 0 ,
#' }
#' Lévy measure,
#' \deqn{
#'     \nu{(\mathrm{d}u)}
#'         = \frac{\eta}{\Gamma{(1 - \eta)}} \cdot u^{-\eta-1} \mathrm{d}u ,
#' }
#' and Stieljtes measure
#' \deqn{
#'     \sigma{(\mathrm{d}u)}
#'         = \frac{\sin{(\eta \pi)}}{\pi} \cdot u^{\eta - 1} \mathrm{d}u ,
#' }
#' see [AlphaStableBernsteinFunction-class].
#'
#' ### Inverse-Gaussian family
#' We have \eqn{\eta > 0}, Bernstein function
#' \deqn{
#'     \psi{(x)}
#'         = 1_{\{ x > 0\}} a + b x + \gamma \cdot
#'              {\left[ \sqrt{2 x + \eta^2} - \eta \right]},
#'             \quad x \geq 0 ,
#' }
#' Lévy measure
#' \deqn{
#'     \nu{(\mathrm{d}u)}
#'         = \frac{1}{ \sqrt{2 \pi} } \cdot
#'             \frac{ e^{-\frac{1}{2} \eta^2 u} }{ \sqrt{u^3} } \mathrm{d}u ,
#' }
#' and Stieltjes measure
#' \deqn{
#'     \sigma{(\mathrm{d}u)}
#'         = \frac{\sin{(\pi / 2)}}{\pi} \cdot
#'             \frac{\sqrt{2 u - \eta^2}} {u}
#'              1_{\{ u > \eta^2 / 2 \}} \mathrm{d}u ,
#' }
#' see [InverseGaussianBernsteinFunction-class].
#'
#' ### Gamma family
#' We have \eqn{\eta > 0}, Bernstein function
#' \deqn{
#'     \psi{(x)}
#'         = 1_{\{ x > 0\}} a + b x + \gamma \cdot
#'              \log{\left( 1 +  \frac{x}{\eta} \right)} ,
#'             \quad x \geq 0 ,
#' }
#' Lévy measure
#' \deqn{
#'     \nu{(\mathrm{d}u)}
#'         = e^{-\eta u} u^{-1} \mathrm{d}u ,
#' }
#' and Stieljtes measure
#' \deqn{
#'     \sigma{(\mathrm{d}u)}
#'         = u^{-1} 1_{\{ u > \eta \}} \mathrm{d}u ,
#' }
#' see [GammaBernsteinFunction-class].
#'
#' ## Simulation algorithms
#'
#' ### Exchangeable Marshall–Olkin simulation algorithms
#' The *MDCM*, *AM*, and *ESM* simulation algorithms for the
#' *exchangeable Marshall–Olkin distribution* can be used.
#' For this, the corresponding Bernstein function is passed to [rextmo()].
#' An exception is the *ESM* for the *Armageddon family* which uses an optimized
#' version considering only finite shock-times.
#'
#' ### Lévy-frailty simulation algorithms
#' The *Lévy-frailty model* simulates the elements of the random vector as
#' first-hitting times of a compound Poisson subordinator \eqn{\Lambda} into
#' sets \eqn{(E_i, \infty)} for iid unit exponential random variables.
#' Here, the subordinator is a linear combination of a pure-drift subordinator,
#' a pure-killing subordinator, and a pure-jump compound Poisson subordinator,
#' i.e.
#' \deqn{
#'    \Lambda_{t}
#'        = \infty \cdot 1_{\{ \epsilon > a t \}} + b t +
#'          \sum_{j=1}^{N_{\gamma t}} X_{j} ,
#'            \quad t \geq 0,
#' }
#' where \eqn{\epsilon} is a unit exponential rv, `n` is a Poisson process, and
#' \eqn{X_{1}, X_{2}, \ldots} are iid jumps from the corresponding jump
#' distribution, see \insertCite{@see pp. 140 psqq. @Mai2017a}{rmo}.
#'
#' @family sampling-algorithms
#'
#' @examples
#' ## Armageddon
#' rpextmo(10, 3, a = 0.2, b = 0.5)
#' ## comonotone
#' rpextmo(10, 3, a = 1)
#' ## independence
#' rpextmo(10, 3, b = 1)
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, method = "ESM")
#' ## comonotone
#' rpextmo(10, 3, a = 1, method = "ESM")
#' ## independence
#' rpextmo(10, 3, b = 1, method = "ESM")
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, method = "LFM")
#' ## comonotone
#' rpextmo(10, 3, a = 1, method = "LFM")
#' ## independence
#' rpextmo(10, 3, b = 1, method = "LFM")
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, method = "MDCM")
#' ## comonotone
#' rpextmo(10, 3, a = 1, method = "MDCM")
#' ## independence
#' rpextmo(10, 3, b = 1, method = "MDCM")
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, method = "AM")
#' ## comonotone
#' rpextmo(10, 3, a = 1, method = "AM")
#' ## independence
#' rpextmo(10, 3, b = 1, method = "AM")
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, family = "Armageddon")
#' ## comonotone
#' rpextmo(10, 3, a = 1, family = "Armageddon")
#' ## independence
#' rpextmo(10, 3, b = 1, family = "Armageddon")
#'
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5,
#'   family = "Armageddon",
#'   method = "ESM"
#' )
#' ## comonotone
#' rpextmo(
#'   10, 3,
#'   a = 1,
#'   family = "Armageddon",
#'   method = "ESM"
#' )
#' ## independence
#' rpextmo(
#'   10, 3,
#'   b = 1,
#'   family = "Armageddon",
#'   method = "ESM"
#' )
#'
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5,
#'   family = "Armageddon",
#'   method = "LFM"
#' )
#' ## comonotone
#' rpextmo(
#'   10, 3,
#'   a = 1,
#'   family = "Armageddon",
#'   method = "LFM"
#' )
#' ## independence
#' rpextmo(
#'   10, 3,
#'   b = 1,
#'   family = "Armageddon",
#'   method = "LFM"
#' )
#'
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5,
#'   family = "Armageddon",
#'   method = "MDCM"
#' )
#' ## comonotone
#' rpextmo(
#'   10, 3,
#'   a = 1,
#'   family = "Armageddon",
#'   method = "MDCM"
#' )
#' ## independence
#' rpextmo(
#'   10, 3,
#'   b = 1,
#'   family = "Armageddon",
#'   method = "MDCM"
#' )
#'
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5,
#'   family = "Armageddon",
#'   method = "AM"
#' )
#' ## comonotone
#' rpextmo(
#'   10, 3,
#'   a = 1,
#'   family = "Armageddon",
#'   method = "AM"
#' )
#' ## independence
#' rpextmo(
#'   10, 3,
#'   b = 1,
#'   family = "Armageddon",
#'   method = "AM"
#' )
#'
#' ## Poisson
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Poisson"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Poisson",
#'   method = "ESM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Poisson",
#'   method = "LFM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Poisson",
#'   method = "MDCM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Poisson",
#'   method = "AM"
#' )
#'
#' ## Pareto
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = c(0.5, 1e-4), family = "Pareto"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = c(0.5, 1e-4), family = "Pareto",
#'   method = "ESM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = c(0.5, 1e-4), family = "Pareto",
#'   method = "LFM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = c(0.5, 1e-4), family = "Pareto",
#'   method = "MDCM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = c(0.5, 1e-4), family = "Pareto",
#'   method = "AM"
#' )
#'
#' ## Exponential
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Exponential"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Exponential",
#'   method = "ESM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Exponential",
#'   method = "LFM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Exponential",
#'   method = "MDCM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Exponential",
#'   method = "AM"
#' )
#'
#' ## Alpha-Stable
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "AlphaStable"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "AlphaStable",
#'   method = "ESM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "AlphaStable",
#'   method = "MDCM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "AlphaStable",
#'   method = "AM"
#' )
#'
#' ## Inverse Gaussian
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "InverseGaussian"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "InverseGaussian",
#'   method = "ESM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "InverseGaussian",
#'   method = "MDCM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "InverseGaussian",
#'   method = "AM"
#' )
#'
#' ## Gamma
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Gamma"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Gamma",
#'   method = "ESM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Gamma",
#'   method = "MDCM"
#' )
#' rpextmo(
#'   10, 3,
#'   a = 0.2, b = 0.5, gamma = 2,
#'   eta = 0.5, family = "Gamma",
#'   method = "AM"
#' )
#'
#' @references
#'  \insertAllCited{}
#'
#' @importFrom checkmate qassert assert_choice
#'
#' @export
rpextmo <- function( # nolint
    n, d, a = 0, b = 0, gamma = 1, eta = NULL,
    family = c(
      "Armageddon", "Poisson", "Pareto",
      "Exponential", "AlphaStable", "InverseGaussian",
      "Gamma"
    ),
    method = c("MDCM", "LFM", "AM", "ESM")) {
  family <- match.arg(family)
  assert_choice(
    family,
    c(
      "Armageddon", "Poisson", "Pareto", "Exponential",
      "AlphaStable", "InverseGaussian", "Gamma"
    )
  )
  if (missing(method) && isTRUE("Armageddon" == family)) {
    method <- "ESM"
  } else {
    method <- match.arg(method)
  }
  assert_choice(method, c("MDCM", "LFM", "AM", "ESM"))
  qassert(n, "X1[0,)")
  qassert(d, "X1[2,)")
  qassert(a, "N1[0,)")
  qassert(b, "N1[0,)")
  qassert(gamma, "N1(0,)")
  qassert(eta, c("N+(0,)", "0"))

  if ((family == "Armageddon") && (method == "ESM")) {
    Rcpp__rarmextmo_esm(n, d, alpha = b, beta = a)
  } else if (method == "LFM") {
    assert_choice(
      family, c("Armageddon", "Poisson", "Pareto", "Exponential")
    )
    if (family == "Armageddon") {
      qassert(eta, "0")
      gamma <- 0
      rjump_name <- "rposval"
      rjump_arg_list <- list("value" = 0)
    } else if (family == "Poisson") {
      qassert(eta, "N1(0,)")
      rjump_name <- "rposval"
      rjump_arg_list <- list("value" = eta)
    } else if (family == "Pareto") {
      qassert(eta, "N2(0,)")
      rjump_name <- "rpareto"
      rjump_arg_list <- list("alpha" = eta[[1]], x0 = eta[[2]])
    } else if (family == "Exponential") {
      qassert(eta, "N1(0,)")
      rjump_name <- "rexp"
      rjump_arg_list <- list("rate" = eta)
    } else {
      stop(sprintf("Family %s not implemented for LFM", family)) # nocov
    }
    Rcpp__rextmo_lfm(
      n, d,
      rate = gamma, rate_killing = a, rate_drift = b,
      rjump_name = rjump_name, rjump_arg_list = rjump_arg_list
    )
  } else if (method %in% c("MDCM", "AM", "ESM")) {
    psi <- NULL
    if (family == "Poisson") {
      qassert(eta, "N1(0,)")
      psi <- PoissonBernsteinFunction(eta = eta)
    } else if (family == "Pareto") {
      qassert(eta, "N2")
      qassert(eta[[1]], "N1(0,1)")
      qassert(eta[[2]], "N1(0,)")
      psi <- ParetoBernsteinFunction(alpha = eta[[1]], x0 = eta[[2]])
    } else if (family == "Exponential") {
      qassert(eta, "N1(0,)")
      psi <- ExponentialBernsteinFunction(lambda = eta)
    } else if (family == "AlphaStable") {
      qassert(eta, "N1(0,)")
      psi <- AlphaStableBernsteinFunction(alpha = eta)
    } else if (family == "InverseGaussian") {
      qassert(eta, "N1(0,)")
      psi <- InverseGaussianBernsteinFunction(eta = eta)
    } else if (family == "Gamma") {
      qassert(eta, "N1(0,)")
      psi <- GammaBernsteinFunction(a = eta)
    }
    if (!(gamma == 1) && !is.null(psi)) {
      psi <- ScaledBernsteinFunction(scale = gamma, original = psi)
    }
    if (!(b == 0)) {
      if (!is.null(psi)) {
        psi <- SumOfBernsteinFunctions(
          first = LinearBernsteinFunction(scale = b),
          second = psi
        )
      } else {
        psi <- LinearBernsteinFunction(scale = b)
      }
    }
    if (!(a == 0)) {
      if (!is.null(psi)) {
        psi <- SumOfBernsteinFunctions(
          first = ConstantBernsteinFunction(constant = a),
          second = psi
        )
      } else {
        psi <- ConstantBernsteinFunction(constant = a)
      }
    }
    rextmo(n, d, bf = psi, method = method)
  } else {
    stop(sprintf("Method %s not implemented", method)) # nocov
  }
}
