## #### Sample from MO distributions ####
##


#' Sample from a Marshall--Olkin distribution
#'
#' Draws `n` independent samples from a `d`-variate Marshall-Olkin distribution
#' with shock arrival `intensities`.
#'
#' @param n an integer for the number of samples.
#' @param d an integer for the dimension of the sample.
#' @param intensities a numeric vector with the Marshall-Olkin shock arrival intensities.
#' @param method a character vector indicating which sampling algorithm should be used.
#'   Use "AM" for the *Arnold model* and "ESM" for the *exogenous shock model*.
#'
#' @details
#' __Parametrisation__:
#' The Marshall--Olkin distribution has the survival function
#' \deqn{
#'   \bar{F}(t)
#'      = \exp{\left\{ - \sum_{I} \lambda_I \max_{i \in I} t_i \right\}} , \quad t > 0 ,
#' }
#' for *shock arrival intensities* \eqn{\lambda_I \geq 0},
#' \eqn{\emptyset \neq I \subseteq {\{ 1 , \ldots, d \}}}.
#'
#' __The shock arrival intensities__:
#' - The shock arrival `intensities` must be stored in a vector of length
#'    \eqn{2^d-1}.
#' - A shock arrival intensity of zero corresponds to an almost surely infinite
#'    shock arrival time.
#' - We use a binary representation to map a non-empty subset \eqn{J}
#'    of \eqn{\{ 1, \ldots, d\}}{{1, \ldots, d}} to integers \eqn{j} of
#'    \eqn{1, \ldots, 2^d-1}.
#'    In particular, \eqn{i} is a component in the set \eqn{J} corresponding to
#'    the integer \eqn{j} if, and only if,
#'    \eqn{j = \sum_{k=0}^\infty a_k 2^k}{\sum a[k] * 2^k}
#'    and \eqn{a_{i-1} = 1}{a[i-1] = 1}.
#'
#' The __exogenous shock model__ simulates a Marshall--Olkin distributed random
#' vector via exponentially distributed shock times for all non-empty subsets,
#' see \insertCite{@see pp. 104 psqq. @Mai2017a}{rmo} and
#' \insertCite{Marshall1967a}{rmo}.
#'
#' The __Arnold model__ simulates a Marshall--Olkin distributed random vector
#' by simulating a marked homogeneous Poisson process and where the
#' inter-arrival times correspond to shock shock-arrival times and the marks to
#' the specific shocks, see \insertCite{@see Sec. 3.1.2 @Mai2017a}{rmo} and
#' \insertCite{Arnold1975a}{rmo}.
#'
#' @return `rmo` returns an \eqn{n \times d}{n x d} numeric matrix with the rows
#'   corresponding to independent and identically distributed samples of a
#'   \eqn{d} variate Marshall-Olkin distribution with parameters `intensities`.
#'
#' @family sampling-algorithms
#'
#' @examples
#' rmo(10L, 2L, c(0.4, 0.3, 0.2))
#' rmo(10L, 2L, c(1, 1, 0))         ## independence
#' rmo(10L, 2L, c(0, 0, 1))         ## comonotone
#'
#' rmo(10L, 2L, c(0.4, 0.3, 0.2), method = "ESM")
#' rmo(10L, 2L, c(1, 1, 0), method = "ESM")         ## independence
#' rmo(10L, 2L, c(0, 0, 1), method = "ESM")         ## comonotone
#'
#' rmo(10L, 2L, c(0.4, 0.3, 0.2), method = "AM")
#' rmo(10L, 2L, c(1, 1, 0), method = "AM")         ## independence
#' rmo(10L, 2L, c(0, 0, 1), method = "AM")         ## comonotone
#'
#' @references
#'  \insertAllCited{}
#'
#' @export
rmo <- function(n, d, intensities, method = c("AM", "ESM")) {
  method <- match.arg(method)
  if ("ESM" == method) {
    Rcpp__rmo_esm(n, d, intensities)
  } else if ("AM" == method) {
    Rcpp__rmo_am(n, d, intensities)
  }
}


## #### Sample from an exchangeable Marshall--Olkin distribution ####
##

#' Sample from an exchangeable Marshall-Olkin distribution
#'
#' Draws `n` independent samples from a `d` variate exchangeable Marshall-Olkin
#' distribution with shock-size arrival rates `ex_intensities`.
#'
#' @section References:
#' For more information on a similar algorithm, see J.-F. Mai, M. Scherer,
#' "Simulating Copulas", World Scientific (2017), pp. 122 psqq.
#'
#' @param n an integer for the number of samples.
#' @param d an integer for the dimension of the sample.
#' @param ex_intensities a numeric vector with the exchangeable Marshall-Olkin exchangeable
#'   shock-size arrival intensities.
#' @param method a character vector indicating which sampling algorithm should be used.
#'   Use "MDCM" for the *Markovian death-counting model*, "AM" for the *Arnold model*,
#'   and "ESM" for the *exogenous shock model*.
#'
#' @return
#' `rexmo` implements the Markovian model for the death-counting process of the exchangeable
#' subclass and returns an  \eqn{n \times d}{n x d} numeric matrix with the rows corresponding to
#' independent and identically distributed samples of a \eqn{d} variate exchangeable Marshall-Olkin
#' distribution with exchangeable shock-size arrival intensities `ex_intensities`.
#'
#' @details
#' __Parameterisation__:
#' The exchangeable Marshall--Olkin distribution has the property that \eqn{\lambda_I} only depends
#' on the cardinality of \eqn{I}, i.e., the *(unscaled) exchangeable shock arrival  intensities* are
#' defined for \eqn{I} with \eqn{i = \lvert I\rvert} by \eqn{\lambda_i = \lambda_I}, see [rmo()].
#' The *(scaled) exchangeable shock-size arrival intensities* are defined by
#' \eqn{\eta_i = \binom{d}{i} \lambda_i}.
#'
#' @family sampling-algorithms
#'
#' @examples
#' rexmo(10, 2, c(2 * 0.4, 0.2))
#' rexmo(10, 2, c(2, 0))          ## independence
#' rexmo(10, 2, c(0, 1))          ## comonotone
#'
#' rexmo(10, 2, c(2 * 0.4, 0.2), method = "AM")
#' rexmo(10, 2, c(2, 0), method = "AM")          ## independence
#' rexmo(10, 2, c(0, 1), method = "AM")          ## comonotone
#'
#' rexmo(10, 2, c(2 * 0.4, 0.2), method = "ESM")
#' rexmo(10, 2, c(2, 0), method = "ESM")          ## independence
#' rexmo(10, 2, c(0, 1), method = "ESM")          ## comonotone
#'
#' @export
rexmo <- function(n, d, ex_intensities, method = c("MDCM", "AM", "ESM")) {
  method <- match.arg(method)
  if ("MDCM" == method) {
    Rcpp__rexmo_mdcm(n, d, ex_intensities)
  } else if (method %in% c("AM", "ESM")) {
    intensities <- uexi2i(
      sapply(seq_along(ex_intensities), function(i) {
        divide_binomial_coefficient(ex_intensities[[i]], d, i)
      }))
    rmo(n, d, intensities, method = method)
  }
}

## #### Sample from extendible Marshall-Olkin distributions ####
##

#' Sample from an extendible Marshall--Olkin distribution
#'
#' Draws `n` independent samples from a `d`-variate extendible Marshall--Olkin distribution with
#' Bernstein function `bf` using a wrapper of [rexmo()].
#'
#' @param n an integer for the number of samples.
#' @param d an integer for the dimension of the sample.
#' @param bf a [BernsteinFunction-class]  with the Bernstein function of the extendible
#'   Marshall--Olkin distribution.
#' @param method a character indicating which sampling algorithm should be used.
#'   Use "MDCM" for the *Markovian death-counting model*, "AM" for the *Arnold model*,
#'   and "ESM" for the *exogenous shock model*.
#'
#' @return
#' `rextmo` is a wrapper arround [rexmo()] for extendible Marshall--Olkin distributions and returns
#' an  \eqn{n \times d}{n x d} numeric matrix with the rows corresponding to independent and
#' identically distributed samples of a \eqn{d} variate extendible Marshall-Olkin distribution
#' with Bernstein function `bf`.
#'
#' @details
#' __Parametrization__:
#' Extendible Marshall--Olkin distributions are parametrized by Bernstein functions \eqn{\psi} such that the
#' exchangeable shock-size arrival intensities are
#' \deqn{
#'   \eta_{i}
#'      = \binom{d}{i} {(-1)}^{i-1} \Delta{ \psi{(d-i)} } , \quad i \in {[d]} .
#' }
#'
#' @family sampling-algorithms
#'
#' @examples
#' rextmo(10, 2, AlphaStableBernsteinFunction(alpha = log2(2  - 0.5)))
#' rextmo(10, 2, LinearBernsteinFunction(scale = 1))                    # independence
#' rextmo(10, 2, ConstantBernsteinFunction(constant = 1))               # comonotone
#'
#' rextmo(10, 2, AlphaStableBernsteinFunction(alpha = log2(2  - 0.5)), method = "AM")
#' rextmo(10, 2, LinearBernsteinFunction(scale = 1), method = "AM")                    # independence
#' rextmo(10, 2, ConstantBernsteinFunction(constant = 1), method = "AM")               # comonotone
#'
#' rextmo(10, 2, AlphaStableBernsteinFunction(alpha = log2(2  - 0.5)), method = "ESM")
#' rextmo(10, 2, LinearBernsteinFunction(scale = 1), method = "ESM")                    # independence
#' rextmo(10, 2, ConstantBernsteinFunction(constant = 1), method = "ESM")               # comonotone
#'
#' @export
rextmo <- function(n, d, bf, method = c("MDCM", "AM", "ESM")) {
  rexmo(n, d, exIntensities(bf, d), method = method)
}


#' Sample from an parametrized ext. MO distribution
#'
#' Draws `n` independent samples from a `d` variate paramatrized ext. MO distribution
#' with parameter vector `eta`.
#'
#' @param n an integer for the number of samples.
#' @param d an integer for the dimension of the sample.
#' @param a a non-negative double for the killing-rate parameter \eqn{a} of the Bernstein function
#'   (default `a = 0`).
#' @param b a non-negative double for the drift parameter \eqn{b} of the Bernstein function (default
#'   `b = 0`).
#' @param gamma a position double for the scaling of the integral part of the Bernstein function
#'   (default `gamma = 1`).
#' @param eta a numeric vector for the family's parameters.
#' @param family a string indicating the parametrized family.
#'    Use "Armageddon" for the *Armageddon* family, "Poisson" for the *Poisson family*, "Pareto" for
#'    the *Pareto family*, "Exponential" for the *Exponential family*, "AlphaStable" for the
#'    *\eqn{\alpha}-stable fmaily*, "InverseGaussian" for the *Inverse-Gaussian family*, "Gamma" for
#'    the *Gamma family*.
#' @param method a string indicating which sampling algorithm should be used.
#'   Use "ESM" for the *exogenous shock model*, considering only finite shocks,
#'   "MDCM" for the *Markovian death-set model*, "LFM" for the *Lévy-frailty model*,
#'   and "AM" for the *Arnold model*.
#'
#' @return `rpextmo` implements, or wraps around, various sampling algorithms for parametrized ext.
#' MO families and returns an \eqn{n \times d}{n x d} array matrix with rows corresponding to the
#' iid samples of a \eqn{d} variate parametrized ext. MO distribution with parameters \eqn{a},
#' \eqn{b}, and \eqn{\eta}.
#'
#' @details
#' __Parameterisation__:
#' A parametrized ext. MO distribution is a family of ext. MO distribution corresponding to
#' Bernstein functions of the form
#' \deqn{
#'   \psi{(x)}
#'      = 1_{\{ x > 0 \}} a + b x + \gamma \cdot \int_{0}^{\infty}{ {[1 - e^{-ux}]} \nu{(\mathrm{d}u)} },
#'      \quad x \geq 0 ,
#' }
#' or
#' \deqn{
#'  \psi{(x)}
#'    = 1_{\{ x > 0 \}} a + b x + \gamma \cdot \int_{0}^{\infty}{ \frac{x}{x + u} \sigma{(\mathrm{d}u)} },
#'      \quad x \geq 0 ,
#' }
#' where \eqn{a, a \geq 0} and \eqn{\nu}, resp. \eqn{\sigma}, are the Lévy measure, resp. Stieltjes
#' measure, and \eqn{\alpha > 0}, \eqn{\beta > 0}, or \eqn{nu \not\equiv 0}, resp.
#' \eqn{\sigma \not\equiv 0}.
#'
#' The Lévy measure, resp. Stieltjes measure, take the following forms:
#' - *Armageddon family*: \eqn{nu = \sigma \equiv 0} and Bernstein function
#'   \deqn{
#'    \psi{(x)}
#'      = 1_{\{ x > 0\}} a + b x ,
#'      \quad x \geq 0 ,
#'   }
#'   see [ConstantBernsteinFunction-class] and
#'   [LinearBernsteinFunction-class].
#' - *Poisson family*: We have \eqn{\eta > 0}, Bernstein function
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
#' - *Pareto family*: We have \eqn{\eta \in \mathbb{R}^2} with \eqn{\eta_1, \eta_2 > 0} and
#'   Lévy measure
#'   \deqn{
#'     \nu{(\mathrm{d}u)}
#'      = \eta_{1} \eta_{2}^{\eta_{1}} \cdot u^{-\eta_{1}-1}  1_{\{ u > \eta_{2}\}} \mathrm{d}u ,
#'   }
#'   see [ParetoBernsteinFunction-class].
#' - *Exponential family*: We have \eqn{\eta > 0}, Bernstein function
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
#' - *\eqn{\alpha}-stable family*: We have \eqn{\eta \in {(0, 1)}}, Bernstein function
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
#' - *Inverse-Gaussian family*: We have \eqn{\eta > 0}, Bernstein function
#'   \deqn{
#'    \psi{(x)}
#'      = 1_{\{ x > 0\}} a + b x + \gamma \cdot {\left[ \sqrt{2 x + \eta^2} - \eta \right]},
#'      \quad x \geq 0 ,
#'   }
#'   Lévy measure
#'   \deqn{
#'    \nu{(\mathrm{d}u)}
#'      = \frac{1}{ \sqrt{2 \pi} } \cdot \frac{ e^{-\frac{1}{2} \eta^2 u} }{ \sqrt{u^3} } \mathrm{d}u ,
#'   }
#'   and Stieltjes measure
#'   \deqn{
#'    \sigma{(\mathrm{d}u)}
#'      = \frac{\sin{(\pi / 2)}}{\pi} \cdot \frac{\sqrt{2 u - \eta^2}}{u} 1_{\{ u > \eta^2 / 2 \}} \mathrm{d}u ,
#'   }
#'   see [InverseGaussianBernsteinFunction-class].
#' - *Gamma family*: We have \eqn{\eta > 0}, Bernstein function
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
#'
#' For the *Armageddon family*, an optimized version of the __exogenous shock model__ is used,
#' ignoring all infinite shocks which will not influence the final result.
#'
#' The __Lévy-frailty model__ simulates the elements of the random vector as first-hitting times
#' of a compound Poisson subordinator \eqn{\Lambda} into sets \eqn{(E_i, \infty)} for iid unit
#' exponential random variables.
#' Here, the subordinator is a linear combination of a pure-drift subordinator, a pure-killing subordinator,
#' and a pure-jump compound Poisson subordinator, i.e.
#' \deqn{
#'    \Lambda_{t}
#'      = \infty \cdot 1_{\{ \epsilon > \beta t \}} + \alpha t + \sum_{j=1}^{N_{\gamma t}} X_{j} ,
#'      \quad t \geq 0,
#' }
#' where \eqn{\epsilon} is a unit exponential rv, \eqn{N} is a Poisson process, and \eqn{X_{1},
#' X_{2}, \ldots} are iid jumps from the corresponding jump distribution.
#'
#' For the *Markovian death-set model*, the *Arnold model*, or the *exogenous shock model* a
#' suitable [BernsteinFunction-class] is created and [rextmo()] is called.
#'
#' @section References: For more information on the LFM algorithm, see J.-F. Mai,
#' M. Scherer, "Simulating Copulas", World Scientific (2017), pp. 140 psqq.
#'
#' @family sampling-algorithms
#'
#' @examples
#' ## Armageddon
#'
#' rpextmo(10, 2L, a = 0.2, b = 0.5)
#' rpextmo(10, 2, a = 1)      ## comonotone
#' rpextmo(10, 2, b = 1)      ## independence
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, method = "ESM")
#' rpextmo(10, 2, a = 1, method = "ESM")      ## comonotone
#' rpextmo(10, 2, b = 1, method = "ESM")      ## independence
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, method = "LFM")
#' rpextmo(10L, 2, a = 1, method = "LFM")      ## comonotone
#' rpextmo(10L, 2, b = 1, method = "LFM")      ## independence
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, method = "MDCM")
#' rpextmo(10, 2, a = 1, method = "MDCM")      ## comonotone
#' rpextmo(10, 2, b = 1, method = "MDCM")      ## independence
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, method = "AM")
#' rpextmo(10, 2, a = 1, method = "AM")      ## comonotone
#' rpextmo(10, 2, b = 1, method = "AM")      ## independence
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, family = "Armageddon")
#' rpextmo(10, 2, a = 1, family = "Armageddon")      ## comonotone
#' rpextmo(10, 2, b = 1, family = "Armageddon")      ## independence
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, family = "Armageddon", method = "ESM")
#' rpextmo(10, 2, a = 1, family = "Armageddon", method = "ESM")      ## comonotone
#' rpextmo(10, 2, b = 1, family = "Armageddon", method = "ESM")      ## independence
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, family = "Armageddon", method = "LFM")
#' rpextmo(10, 2, a = 1, family = "Armageddon", method = "LFM")      ## comonotone
#' rpextmo(10, 2, b = 1, family = "Armageddon", method = "LFM")      ## independence
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, family = "Armageddon", method = "MDCM")
#' rpextmo(10, 2, a = 1, family = "Armageddon", method = "MDCM")      ## comonotone
#' rpextmo(10, 2, b = 1, family = "Armageddon", method = "MDCM")      ## independence
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, family = "Armageddon", method = "AM")
#' rpextmo(10, 2, a = 1, family = "Armageddon", method = "AM")      ## comonotone
#' rpextmo(10, 2, b = 1, family = "Armageddon", method = "AM")      ## independence
#'
#' ## Poisson
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Poisson")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Poisson", method = "ESM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Poisson", method = "LFM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Poisson", method = "MDCM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Poisson", method = "AM")
#'
#' ## Pareto
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = c(0.5, 1e-4), family = "Pareto")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = c(0.5, 1e-4), family = "Pareto", method = "ESM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = c(0.5, 1e-4), family = "Pareto", method = "LFM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = c(0.5, 1e-4), family = "Pareto", method = "MDCM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = c(0.5, 1e-4), family = "Pareto", method = "AM")
#'
#' ## Exponential
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Exponential")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Exponential", method = "ESM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Exponential", method = "LFM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Exponential", method = "MDCM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Exponential", method = "AM")
#'
#' ## Alpha-Stable
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "AlphaStable")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "AlphaStable", method = "ESM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "AlphaStable", method = "MDCM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "AlphaStable", method = "AM")
#'
#' ## Inverse Gaussian
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "InverseGaussian")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "InverseGaussian", method = "ESM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "InverseGaussian", method = "MDCM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "InverseGaussian", method = "AM")
#'
#' ## Gamma
#'
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Gamma")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Gamma", method = "ESM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Gamma", method = "MDCM")
#' rpextmo(10, 2, a = 0.2, b = 0.5, gamma = 2, eta = 0.5, family = "Gamma", method = "AM")
#'
#'
#' @importFrom checkmate qassert assert_choice
#'
#' @export
rpextmo <- function(n, d, a = 0, b = 0, gamma = 1, eta = NULL,
    family = c("Armageddon", "Poisson", "Pareto",
      "Exponential", "AlphaStable", "InverseGaussian", "Gamma"),
    method = c("MDCM", "LFM", "AM", "ESM")) {
  family <- match.arg(family)
  if (missing(method) && isTRUE("Armageddon" == family)) {
    method <- "ESM"
  } else {
    method <- match.arg(method)
  }
  qassert(a, "N1[0,)")
  qassert(b, "N1[0,)")
  qassert(gamma, "N1(0,)")
  qassert(eta, c("N+(0,)", "0"))

  if (isTRUE("Armageddon" == family) && isTRUE("ESM" == method)) {
    Rcpp__rarmextmo_esm(n, d, alpha = b, beta = a)
  } else if (isTRUE("LFM" == method)) {
    assert_choice(family, c("Armageddon", "Poisson", "Pareto", "Exponential"))
    if (isTRUE("Armageddon" == family)) {
      qassert(eta, "0")
      gamma <- 0
      rjump_name <- "rposval"
      rjump_arg_list <- list("value" = 0)
    } else if (isTRUE("Poisson" == family)) {
      qassert(eta, "N1(0,)")
      rjump_name <- "rposval"
      rjump_arg_list <- list("value" = eta)
    } else if (isTRUE("Pareto" == family)) {
      qassert(eta, "N2(0,)")
      rjump_name <- "rpareto"
      rjump_arg_list <- list("alpha" = eta[[1]], x0 = eta[[2]])
    } else if (isTRUE("Exponential" == family)) {
      qassert(eta, "N1(0,)")
      rjump_name <- "rexp"
      rjump_arg_list <- list("rate" = eta)
    } else {
      stop(sprintf("Family %s not implemented for LFM", family))
    }
    Rcpp__rextmo_lfm(
      n, d,
      rate = gamma, rate_killing = a, rate_drift = b,
      rjump_name = rjump_name, rjump_arg_list = rjump_arg_list)
  } else if (method %in% c("MDCM", "AM", "ESM")) {
    psi <- NULL
    if (isTRUE("Poisson" == family)) {
      qassert(eta, "N1(0,)")
      psi <- PoissonBernsteinFunction(eta = eta)
    } else if (isTRUE("Pareto" == family)) {
      qassert(eta, "N2")
      qassert(eta[[1]], "N1(0,1)")
      qassert(eta[[2]], "N1(0,)")
      psi <- ParetoBernsteinFunction(alpha = eta[[1]], x0 = eta[[2]])
    } else if (isTRUE("Exponential" == family)) {
      qassert(eta, "N1(0,)")
      psi <- ExponentialBernsteinFunction(lambda = eta)
    } else if (isTRUE("AlphaStable" == family)) {
      qassert(eta, "N1(0,)")
      psi <- AlphaStableBernsteinFunction(alpha = eta)
    } else if (isTRUE("InverseGaussian" == family)) {
      qassert(eta, "N1(0,)")
      psi <- InverseGaussianBernsteinFunction(eta = eta)
    } else if (isTRUE("Gamma" == family)) {
      qassert(eta, "N1(0,)")
      psi <- GammaBernsteinFunction(a = eta)
    }
    if (isFALSE(gamma == 1) && !is.null(psi)) {
      psi <- ScaledBernsteinFunction(scale = gamma, original = psi)
    }
    if (isFALSE(b == 0)) {
      if (!is.null(psi)) {
        psi <- SumOfBernsteinFunctions(
          first = LinearBernsteinFunction(scale = b),
          second = psi)
      } else {
        psi <- LinearBernsteinFunction(scale = b)
      }
    }
    if (isFALSE(a == 0)) {
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
    stop(sprintf("Method %s not implemented", method))
  }
}
