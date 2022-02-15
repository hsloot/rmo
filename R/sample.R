#' Sample from a Marshall–Olkin distribution
#'
#' Draws \eqn{n} iid samples from a \eqn{d}-variate *Marshall–Olkin distribution*
#' parametrized by *shock arrival intensities*.
#'
#' @param n an integer for the *number of samples*.
#' @param d an integer for the *dimension*.
#' @param intensities a numeric vector for the *shock arrival intensities*.
#' @param method a string indicating which sampling algorithm should be used.
#'   Use "AM" for the *Arnold model* and "ESM" for the *exogenous shock model*. We recommend using
#'   the *ESM* only for small dimensions; the *AM* can be used up until dimension \eqn{30}.
#'
#' @return `rmo` returns a numeric matrix with \eqn{n} rows and \eqn{d} columns with the rows
#'   corresponding to iid distributed samples of a \eqn{d}-variate *Marshall–Olkin distribution*
#'   with *shock arrival intensities* `intensities`.
#'
#' @details # Parametrization
#' The *Marshall–Olkin distribution* has the survival function
#' \deqn{
#'   \bar{F}(t)
#'      = \exp{\left\{ - \sum_{I} \lambda_I \max_{i \in I} t_i \right\}} , \quad t > 0 ,
#' }
#' for *shock arrival intensities* \eqn{\lambda_I \geq 0},
#' \eqn{\emptyset \neq I \subseteq {\{ 1 , \ldots, d \}}}.
#'
#' ## Shock arrival intensities
#'
#' - The *shock arrival intensities* must be stored in a vector of length \eqn{2^d-1}.
#' - A *shock arrival intensity* of zero corresponds to an almost surely infinite shock arrival
#'   time.
#' - We use a binary representation to map a non-empty subset \eqn{J} of
#'   \eqn{\{ 1, \ldots, d\}}{{1, \ldots, d}} to integers \eqn{j} of \eqn{1, \ldots, 2^d-1}. In
#'   particular, \eqn{i} is a component in the set \eqn{J}, corresponding to the integer \eqn{j},
#'   if, and only if, \eqn{j = \sum_{k=0}^\infty a_k 2^k}{\sum a[k] * 2^k} and
#'   \eqn{a_{i-1} = 1}{a[i-1] = 1}.
#'
#' @details # Simulation algorithms
#'
#' ## Exogenous shock model
#' The *exogenous shock model (ESM)* simulates a Marshall–Olkin distributed random vector via
#' exponentially distributed shock times for all non-empty subsets of components and defines each
#' component as the minimum of all shock times corresponding to a subset containing this component,
#' see \insertCite{@see pp. 104 psqq. @Mai2017a}{rmo} and \insertCite{Marshall1967a}{rmo}.
#'
#' ## Arnold model
#' The *Arnold model (AM)* simulates a Marshall–Olkin distributed random vector by simulating a
#' marked homogeneous Poisson process where the inter-arrival times correspond to shock
#' inter-arrival times and the marks to the specific shocks; the process is stopped when all
#' components are *hit* by a shock. As in the *ESM*, each component is defined as the minimum of all
#' shock times corresponding to a subset containing this component, see
#' \insertCite{@see Sec. 3.1.2 @Mai2017a}{rmo} and \insertCite{Arnold1975a}{rmo}.
#'
#' @family sampling-algorithms
#'
#' @examples
#' rmo(10, 3, c(0.4, 0.4, 0.1, 0.4, 0.1, 0.1, 0.4))
#' rmo(10, 3, c(1, 1, 0, 1, 0, 0, 0))         ## independence
#' rmo(10, 3, c(0, 0, 0, 0, 0, 0, 1))         ## comonotone
#'
#' rmo(10, 3, c(0.4, 0.4, 0.1, 0.4, 0.1, 0.1, 0.4), method = "ESM")
#' rmo(10, 3, c(1, 1, 0, 1, 0, 0, 0), method = "ESM")         ## independence
#' rmo(10, 3, c(0, 0, 0, 0, 0, 0, 1), method = "ESM")         ## comonotone
#'
#' rmo(10, 3, c(0.4, 0.4, 0.1, 0.4, 0.1, 0.1, 0.4), method = "AM")
#' rmo(10, 3, c(1, 1, 0, 1, 0, 0, 0), method = "AM")         ## independence
#' rmo(10, 3, c(0, 0, 0, 0, 0, 0, 1), method = "AM")         ## comonotone
#'
#' @references
#'  \insertAllCited{}
#'
#' @importFrom checkmate qassert
#'
#' @export
rmo <- function(n, d, intensities, method = c("AM", "ESM")) {
  method <- match.arg(method)
  qassert(n, "X1[0,)")
  qassert(d, "X1[2,)")
  if (method == "ESM") {
    Rcpp__rmo_esm(n, d, intensities)
  } else if (method == "AM") {
    Rcpp__rmo_am(n, d, intensities)
  } else {
    stop(sprintf("Method %s not implemented", method)) # nocov
  }
}


#' Sample from an exchangeable Marshall–Olkin distribution
#'
#' Draws \eqn{n} iid samples from a \eqn{d}-variate *exchangeable Marshall–Olkin
#' distribution* parametrized by exchangeable *shock-size arrival rates*.
#'
#' @inheritParams rmo
#' @param ex_intensities a numeric vector with the exchangeable *shock-size arrival intensities*.
#' @param method a string indicating which sampling algorithm should be used.
#'   Use "MDCM" for the *Markovian death-counting model*, "AM" for the *Arnold model*, and "ESM" for
#'   the *exogenous shock model*. We recommend using the *ESM* only for small dimensions; the *AM*
#'   can be used up until dimension \eqn{30}.
#'
#' @return `rexmo` returns a numeric matrix with \eqn{n} rows and \eqn{d} columns with the rows
#' corresponding to iid distributed samples of a \eqn{d}-variate *exchangeable Marshall–Olkin
#' distribution* with exchangeable *shock-size arrival intensities* `ex_intensities`.
#'
#' @details # Parametrization
#' The exchangeable Marshall–Olkin distribution has the following property:
#' a *shock arrival intensity* \eqn{\lambda_I} for a set \eqn{I} only depends on the cardinality of
#' \eqn{I}, i.e., the *exchangeable shock arrival intensities* are defined for \eqn{I} with \eqn{i =
#' \lvert I\rvert} by \eqn{\lambda_i = \lambda_I}, see \insertCite{@see Sec. 3.2 @Mai2017a}{rmo}.
#' The *exchangeable Marshall–Olkin distribution* is parametrized by the exchangeable *shock-size
#' arrival intensities* defined by
#' \deqn{
#'  \eta_i
#'    = \binom{d}{i} \lambda_i ,
#'    \quad i \in {\{ 1, \ldots, d \}} .
#' }
#'
#' @details # Simulation algorithms
#'
#' ## General Marshall–Olkin sampling algorithms
#'
#' The *ESM* and *AM* simulation algorithms for the general *Marshall–Olkin distribution* can be
#' used; for this, the exchangeable *shock-size arrival intensities* are converted to the
#' corresponding *shock arrival intensities* and passed to [rmo()].
#'
#' ## Markovian death-set model
#'
#' TBD; see \insertCite{@see pp. 122 psqq. @Mai2017a}{rmo} for a similar algorithm.
#'
#' @family sampling-algorithms
#'
#' @examples
#' rexmo(10, 3, c(1.2, 0.3, 0.4))
#' rexmo(10, 3, c(3, 0, 0))          ## independence
#' rexmo(10, 3, c(0, 0, 1))          ## comonotone
#'
#' rexmo(10, 3, c(1.2, 0.3, 0.4), method = "MDCM")
#' rexmo(10, 3, c(3, 0, 0), method = "MDCM")          ## independence
#' rexmo(10, 3, c(0, 0, 1), method = "MDCM")          ## comonotone
#'
#' rexmo(10, 3, c(1.2, 0.3, 0.4), method = "AM")
#' rexmo(10, 3, c(3, 0, 0), method = "AM")          ## independence
#' rexmo(10, 3, c(0, 0, 1), method = "AM")          ## comonotone
#'
#' rexmo(10, 3, c(1.2, 0.3, 0.4), method = "ESM")
#' rexmo(10, 3, c(3, 0, 0), method = "ESM")          ## independence
#' rexmo(10, 3, c(0, 0, 1), method = "ESM")          ## comonotone
#'
#' @references
#'  \insertAllCited{}
#'
#' @importFrom checkmate qassert
#'
#' @export
rexmo <- function(n, d, ex_intensities, method = c("MDCM", "AM", "ESM")) {
  method <- match.arg(method)
  qassert(n, "X1[0,)")
  qassert(d, "X1[2,)")
  if (method == "MDCM") {
    Rcpp__rexmo_mdcm(n, d, ex_intensities)
  } else if (method %in% c("AM", "ESM")) {
    intensities <- uexi2i(
      sapply(seq_along(ex_intensities), function(i) {
        divide_binomial_coefficient(ex_intensities[[i]], d, i)
      }))
    rmo(n, d, intensities, method = method)
  } else {
    stop(sprintf("Method %s not implemented", method)) # nocov
  }
}

#' Sample from an extendible Marshall–Olkin distribution
#'
#' Draws \eqn{n} iid samples from a \eqn{d}-variate *extendible Marshall–Olkin distribution* with
#' Bernstein function `bf` by wrapping of [rexmo()].
#'
#' @inheritParams rexmo
#' @param bf a [BernsteinFunction-class]  with the *Bernstein function* of the *extendible
#'   Marshall–Olkin distribution*.
#'
#' @return `rextmo` returns a numeric matrix with \eqn{n} rows and \eqn{d} columns with the rows
#' corresponding to iid distributed samples of a \eqn{d}-variate *extendible Marshall–Olkin
#' distribution* with *Bernstein function* `bf`.
#'
#' @details # Parametrization
#' *Extendible Marshall–Olkin distributions* are parametrized by *Bernstein functions* \eqn{\psi},
#' see \insertCite{@see Sec. 3.3 @Mai2017a}{rmo}.
#' The *exchangeable shock-size arrival intensities* are defined by
#' \deqn{
#'   \eta_{i}
#'      = \binom{d}{i} {(-1)}^{i-1} \Delta{ \psi{(d-i)} } , \quad i \in {[d]} .
#' }
#'
#' @family sampling-algorithms
#'
#' @examples
#' rextmo(10, 3, AlphaStableBernsteinFunction(alpha = log2(2  - 0.5)))
#' rextmo(10, 3, LinearBernsteinFunction(scale = 1))                    # independence
#' rextmo(10, 3, ConstantBernsteinFunction(constant = 1))               # comonotone
#'
#' rextmo(10, 3, AlphaStableBernsteinFunction(alpha = log2(2  - 0.5)), method = "AM")
#' rextmo(10, 3, LinearBernsteinFunction(scale = 1), method = "AM")                   # independence
#' rextmo(10, 3, ConstantBernsteinFunction(constant = 1), method = "AM")              # comonotone
#'
#' rextmo(10, 3, AlphaStableBernsteinFunction(alpha = log2(2  - 0.5)), method = "ESM")
#' rextmo(10, 3, LinearBernsteinFunction(scale = 1), method = "ESM")                  # independence
#' rextmo(10, 3, ConstantBernsteinFunction(constant = 1), method = "ESM")             # comonotone
#'
#' @references
#'  \insertAllCited{}
#'
#' @importFrom checkmate qassert
#'
#' @export
rextmo <- function(n, d, bf, method = c("MDCM", "AM", "ESM")) {
  method <- match.arg(method)
  qassert(n, "X1[0,)")
  qassert(d, "X1[2,)")
  rexmo(n, d, exIntensities(bf, d), method = method)
}


#' Sample from parametrized ext. MO distributions
#'
#' Draws \eqn{n} iid samples from a \eqn{d}-variate *paramatrized ext. MO distribution*.
#'
#' @inheritParams rextmo
#' @param a a non-negative double for the *killing-rate* \eqn{a} of the *Bernstein function*.
#' @param b a non-negative double for the *drift* \eqn{b} of the *Bernstein function*.
#' @param gamma a position double for the scaling of the integral part of the *Bernstein function*.
#' @param eta a numeric vector for the family's parameter.
#' @param family a string indicating the parametrized family.
#'    Use "Armageddon" for the *Armageddon* family, "Poisson" for the *Poisson family*, "Pareto" for
#'    the *Pareto family*, "Exponential" for the *Exponential family*, "AlphaStable" for the
#'    *\eqn{\alpha}-stable family*, "InverseGaussian" for the *Inverse-Gaussian family*, "Gamma" for
#'    the *Gamma family*.
#' @param method a string indicating which sampling algorithm should be used.
#'   Use "MDCM" for the *Markovian death-set model*, "LFM" for the *Lévy-frailty model*,  "AM" for
#'   the *Arnold model*, and "ESM" for the *exogenous shock model* (in case of the *Armageddon
#'   family*, the algorithm is optimized to consider only finite shocks).
#'   We recommend using the *ESM* only for small dimensions; the *AM* can be used up until dimension
#'   \eqn{30}.
#'
#' @return `rpextmo` returns a numeric matrix with \eqn{n} rows and \eqn{d} columns with the rows
#' corresponding to iid distributed samples of a \eqn{d}-variate *parametrized extendible
#' Marshall–Olkin distribution* with corresponding parameters.
#'
#' @details # Parametrization
#' A *parametrized ext. MO distribution* is a family of *ext. MO distributions* corresponding to
#' *Bernstein functions* of the form
#' \deqn{
#'   \psi{(x)}
#'      = 1_{\{ x > 0 \}} a + b x + \gamma \cdot
#'      \int_{0}^{\infty}{ {[1 - e^{-ux}]} \nu{(\mathrm{d}u)} },
#'      \quad x \geq 0 ,
#' }
#' or
#' \deqn{
#'  \psi{(x)}
#'    = 1_{\{ x > 0 \}} a + b x + \gamma \cdot
#'      \int_{0}^{\infty}{ \frac{x}{x + u} \sigma{(\mathrm{d}u)} },
#'      \quad x \geq 0 ,
#' }
#' where \eqn{a, a \geq 0} and \eqn{\nu}, resp. \eqn{\sigma}, are the *Lévy measure*, resp.
#' *Stieltjes measure*, and \eqn{\alpha > 0}, \eqn{\beta > 0}, or \eqn{nu \not\equiv 0}, resp.
#' \eqn{\sigma \not\equiv 0}.
#'
#' The Lévy measure, resp. Stieltjes measure, take the forms listed in the following.
#'
#' ## Armageddon family
#' We have \eqn{nu = \sigma \equiv 0} and Bernstein function
#'   \deqn{
#'    \psi{(x)}
#'      = 1_{\{ x > 0\}} a + b x ,
#'      \quad x \geq 0 ,
#'   }
#'   see [ConstantBernsteinFunction-class] and
#'   [LinearBernsteinFunction-class].
#'
#' ## Poisson family
#' We have \eqn{\eta > 0}, Bernstein function
#'   \deqn{
#'    \psi{(x)}
#'      = 1_{\{ x > 0\}} a + b x + \gamma \cdot {[1 - e^{-\eta x}]},
#'      \quad x \geq 0 ,
#'   }
#'   and Lévy measure
#'   \deqn{
#'     \nu{(\mathrm{d}u)}
#'      = \delta_{\{ \eta \}}{(\mathrm{d}u)} ,
#'   }
#'   see [PoissonBernsteinFunction-class].
#'
#' ## Pareto family
#' We have \eqn{\eta \in \mathbb{R}^2} with \eqn{\eta_1, \eta_2 > 0} and
#'   Lévy measure
#'   \deqn{
#'     \nu{(\mathrm{d}u)}
#'      = \eta_{1} \eta_{2}^{\eta_{1}} \cdot u^{-\eta_{1}-1}  1_{\{ u > \eta_{2}\}} \mathrm{d}u ,
#'   }
#'   see [ParetoBernsteinFunction-class].
#'
#' ## Exponential family
#' We have \eqn{\eta > 0}, Bernstein function
#'   \deqn{
#'    \psi{(x)}
#'      = 1_{\{ x > 0\}} a + b x + \gamma \cdot \frac{x}{x + \eta} ,
#'      \quad x \geq 0 ,
#'   }
#'   and Lévy measure
#'   \deqn{
#'    \nu{(\mathrm{d}u)}
#'      = \eta e^{-\eta u} \mathrm{d}u ,
#'   }
#'   and Stieltjes measure
#'   \deqn{
#'    \sigma{(\mathrm{d}u)}
#'      = \delta_{\{ \eta \}}{(\mathrm{d}u)} ,
#'   }
#'  see [ExponentialBernsteinFunction-class].
#'
#' ## \eqn{\alpha}-stable family
#' We have \eqn{\eta \in {(0, 1)}}, Bernstein function
#'   \deqn{
#'    \psi{(x)}
#'      = 1_{\{ x > 0\}} a + b x + \gamma \cdot x^{\eta} ,
#'      \quad x \geq 0 ,
#'   }
#'   Lévy measure,
#'   \deqn{
#'    \nu{(\mathrm{d}u)}
#'      = \frac{\eta}{\Gamma{(1 - \eta)}} \cdot u^{-\eta-1} \mathrm{d}u ,
#'   }
#'   and Stieljtes measure
#'   \deqn{
#'    \sigma{(\mathrm{d}u)}
#'      = \frac{\sin{(\eta \pi)}}{\pi} \cdot u^{\eta - 1} \mathrm{d}u ,
#'   }
#'   see [AlphaStableBernsteinFunction-class].
#'
#' ## Inverse-Gaussian family
#' We have \eqn{\eta > 0}, Bernstein function
#'   \deqn{
#'    \psi{(x)}
#'      = 1_{\{ x > 0\}} a + b x + \gamma \cdot {\left[ \sqrt{2 x + \eta^2} - \eta \right]},
#'      \quad x \geq 0 ,
#'   }
#'   Lévy measure
#'   \deqn{
#'    \nu{(\mathrm{d}u)}
#'      = \frac{1}{ \sqrt{2 \pi} } \cdot
#'        \frac{ e^{-\frac{1}{2} \eta^2 u} }{ \sqrt{u^3} } \mathrm{d}u ,
#'   }
#'   and Stieltjes measure
#'   \deqn{
#'    \sigma{(\mathrm{d}u)}
#'      = \frac{\sin{(\pi / 2)}}{\pi} \cdot
#'        \frac{\sqrt{2 u - \eta^2}}{u} 1_{\{ u > \eta^2 / 2 \}} \mathrm{d}u ,
#'   }
#'   see [InverseGaussianBernsteinFunction-class].
#'
#' ## Gamma family
#' We have \eqn{\eta > 0}, Bernstein function
#'   \deqn{
#'    \psi{(x)}
#'      = 1_{\{ x > 0\}} a + b x + \gamma \cdot \log{\left( 1 +  \frac{x}{\eta} \right)} ,
#'      \quad x \geq 0 ,
#'   }
#'   Lévy measure
#'   \deqn{
#'    \nu{(\mathrm{d}u)}
#'      = e^{-\eta u} u^{-1} \mathrm{d}u ,
#'   }
#'   and Stieljtes measure
#'   \deqn{
#'    \sigma{(\mathrm{d}u)}
#'      = u^{-1} 1_{\{ u > \eta \}} \mathrm{d}u ,
#'   }
#'   see [GammaBernsteinFunction-class].
#'
#' @details # Simulation algorithms
#'
#' ## (General) exchangeable Marshall–Olkin sampling algorithms
#'
#' The *MDCM*, *AM*, and *ESM* simulation algorithms for the *exchangeable Marshall–Olkin
#' distribution* can be used; for this, the corresponding Bernstein function is passed to
#' [rextmo()].
#' An exception is the *ESM* for the *Armageddon family* which uses an optimized version considering
#' only finite shock times.
#'
#' ## Lévy-frailty model
#'
#' The *Lévy-frailty model* simulates the elements of the random vector as first-hitting times of a
#' compound Poisson subordinator \eqn{\Lambda} into sets \eqn{(E_i, \infty)} for iid unit
#' exponential random variables.
#' Here, the subordinator is a linear combination of a pure-drift subordinator, a pure-killing
#' subordinator, and a pure-jump compound Poisson subordinator, i.e.
#' \deqn{
#'    \Lambda_{t}
#'      = \infty \cdot 1_{\{ \epsilon > a t \}} + b t + \sum_{j=1}^{N_{\gamma t}} X_{j} ,
#'      \quad t \geq 0,
#' }
#' where \eqn{\epsilon} is a unit exponential rv, \eqn{N} is a Poisson process, and \eqn{X_{1},
#' X_{2}, \ldots} are iid jumps from the corresponding jump distribution,
#' see \insertCite{@see pp. 140 psqq. @Mai2017a}{rmo}.
#'
#' @family sampling-algorithms
#'
#' @examples
#' ## Armageddon
#' rpextmo(10, 3, a = 0.2, b = 0.5)
#' rpextmo(10, 3, a = 1)      ## comonotone
#' rpextmo(10, 3, b = 1)      ## independence
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, method = "ESM")
#' rpextmo(10, 3, a = 1, method = "ESM")      ## comonotone
#' rpextmo(10, 3, b = 1, method = "ESM")      ## independence
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, method = "LFM")
#' rpextmo(10, 3, a = 1, method = "LFM")      ## comonotone
#' rpextmo(10, 3, b = 1, method = "LFM")      ## independence
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, method = "MDCM")
#' rpextmo(10, 3, a = 1, method = "MDCM")      ## comonotone
#' rpextmo(10, 3, b = 1, method = "MDCM")      ## independence
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, method = "AM")
#' rpextmo(10, 3, a = 1, method = "AM")      ## comonotone
#' rpextmo(10, 3, b = 1, method = "AM")      ## independence
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, family = "Armageddon")
#' rpextmo(10, 3, a = 1, family = "Armageddon")      ## comonotone
#' rpextmo(10, 3, b = 1, family = "Armageddon")      ## independence
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, family = "Armageddon", method = "ESM")
#' rpextmo(10, 3, a = 1, family = "Armageddon", method = "ESM")      ## comonotone
#' rpextmo(10, 3, b = 1, family = "Armageddon", method = "ESM")      ## independence
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, family = "Armageddon", method = "LFM")
#' rpextmo(10, 3, a = 1, family = "Armageddon", method = "LFM")      ## comonotone
#' rpextmo(10, 3, b = 1, family = "Armageddon", method = "LFM")      ## independence
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, family = "Armageddon", method = "MDCM")
#' rpextmo(10, 3, a = 1, family = "Armageddon", method = "MDCM")      ## comonotone
#' rpextmo(10, 3, b = 1, family = "Armageddon", method = "MDCM")      ## independence
#'
#' rpextmo(10, 3, a = 0.2, b = 0.5, family = "Armageddon", method = "AM")
#' rpextmo(10, 3, a = 1, family = "Armageddon", method = "AM")      ## comonotone
#' rpextmo(10, 3, b = 1, family = "Armageddon", method = "AM")      ## independence
#'
#' ## Poisson
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Poisson")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Poisson", method = "ESM")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Poisson", method = "LFM")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Poisson", method = "MDCM")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Poisson", method = "AM")
#'
#' ## Pareto
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = c(0.5, 1e-4), family = "Pareto")
#' rpextmo(
#'  10, 3, a = 0.2, b = 0.5, gamma = 2, eta = c(0.5, 1e-4), family = "Pareto", method = "ESM")
#' rpextmo(
#'  10, 3, a = 0.2, b = 0.5, gamma = 2, eta = c(0.5, 1e-4), family = "Pareto", method = "LFM")
#' rpextmo(
#'  10, 3, a = 0.2, b = 0.5, gamma = 2, eta = c(0.5, 1e-4), family = "Pareto", method = "MDCM")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = c(0.5, 1e-4), family = "Pareto", method = "AM")
#'
#' ## Exponential
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Exponential")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Exponential", method = "ESM")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Exponential", method = "LFM")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Exponential", method = "MDCM")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Exponential", method = "AM")
#'
#' ## Alpha-Stable
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "AlphaStable")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "AlphaStable", method = "ESM")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "AlphaStable", method = "MDCM")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "AlphaStable", method = "AM")
#'
#' ## Inverse Gaussian
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "InverseGaussian")
#' rpextmo(
#'  10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "InverseGaussian", method = "ESM")
#' rpextmo(
#'  10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "InverseGaussian", method = "MDCM")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "InverseGaussian", method = "AM")
#'
#' ## Gamma
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Gamma")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Gamma", method = "ESM")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Gamma", method = "MDCM")
#' rpextmo(10, 3, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Gamma", method = "AM")
#'
#' @references
#'  \insertAllCited{}
#'
#' @importFrom checkmate qassert assert_choice
#'
#' @export
rpextmo <- function( # nolint
    n, d, a = 0, b = 0, gamma = 1, eta = NULL,
    family = c("Armageddon", "Poisson", "Pareto",
      "Exponential", "AlphaStable", "InverseGaussian", "Gamma"),
    method = c("MDCM", "LFM", "AM", "ESM")) {
  family <- match.arg(family)
  if (missing(method) && isTRUE("Armageddon" == family)) {
    method <- "ESM"
  } else {
    method <- match.arg(method)
  }
  qassert(n, "X1[0,)")
  qassert(d, "X1[2,)")
  qassert(a, "N1[0,)")
  qassert(b, "N1[0,)")
  qassert(gamma, "N1(0,)")
  qassert(eta, c("N+(0,)", "0"))

  if ((family == "Armageddon") && (method == "ESM")) {
    Rcpp__rarmextmo_esm(n, d, alpha = b, beta = a)
  } else if (method == "LFM") {
    assert_choice(family, c("Armageddon", "Poisson", "Pareto", "Exponential"))
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
      rjump_name = rjump_name, rjump_arg_list = rjump_arg_list)
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
          second = psi)
      } else {
        psi <- LinearBernsteinFunction(scale = b)
      }
    }
    if (!(a == 0)) {
      if (!is.null(psi)) {
        psi <- SumOfBernsteinFunctions(
          first = ConstantBernsteinFunction(constant = a),
          second = psi)
      } else {
        psi <- ConstantBernsteinFunction(constant = a)
      }
    }
    rextmo(n, d, bf = psi, method = method)
  } else {
    stop(sprintf("Method %s not implemented", method)) # nocov
  }
}
