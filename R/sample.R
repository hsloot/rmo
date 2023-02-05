#' Sample from Marshall–Olkin distributions
#'
#' Draws `n` iid samples from a `d`-variate *Marshall–Olkin distribution* parametrized by
#' *shock-arrival intensities*.
#'
#' @param n an integer for the *number of samples*.
#' @param d an integer for the *dimension*.
#' @param intensities a numeric vector for the *shock-arrival intensities*.
#' @param method a string indicating which sampling algorithm should be used.
#'   Use "AM" for the *Arnold model* and "ESM" for the *exogenous shock model*. We recommend using
#'   the *ESM* only for small dimensions; the *AM* can be used up until dimension \eqn{30}.
#'
#' @return `rmo` returns a numeric matrix with `n` rows and `d` columns with the rows corresponding
#'   to iid distributed samples of a `d`-variate *Marshall–Olkin distribution* with *shock-arrival
#'   intensities* `intensities`.
#'
#'
#' @details
#' The *Marshall–Olkin distribution* was introduced in \insertCite{Marshall1967a}{rmo} and has the
#' survival function
#' \deqn{
#'     \bar{F}{(t)}
#'         = \exp{\left\{ - \sum_{I} \lambda_I \max_{i \in I} t_i \right\}} ,
#'             \quad t = {(t_{1}, \ldots, t_{d})} > 0 ,
#' }
#' for *shock-arrival intensities* \eqn{\lambda_I \geq 0},
#' \eqn{\emptyset \neq I \subseteq {\{ 1 , \ldots, d \}}}.
#' They are called *shock-arrival intensities* as they correspond to the rates of independent
#' exponential random variables from the *exogenous shock model (ESM)*, and a shock-arrival
#' intensity \eqn{\lambda_{I}} of shock \eqn{I} equal to zero implies that the the shock \eqn{I}
#' never arrives.
#' We use the following binary representation to map a subsets of \eqn{\{ 1, \ldots, d\}}{{1,
#' \ldots, d}} to integers \eqn{0, \ldots, 2^d-1}:
#' \deqn{
#'   I \equiv \sum_{k \in I}{ 2^{k-1} }
#' }
#'
#' ## Simulation algorithms
#'
#' ### Exogenous shock model
#' The *exogenous shock model (ESM)* simulates a Marshall–Olkin distributed random vector via
#' independent exponentially distributed shock times for all non-empty subsets of components and
#' defines each component as the minimum of all shock times corresponding to a subset containing
#' this component, see \insertCite{@see pp. 104 psqq. @Mai2017a}{rmo} and
#' \insertCite{Marshall1967a}{rmo}.
#'
#' ### Arnold model
#' The *Arnold model (AM)* simulates a Marshall–Olkin distributed random vector by simulating a
#' marked homogeneous Poisson process with set-valued marks. The process is stopped when all
#' components are *hit* by a shock, see \insertCite{@see Sec. 3.1.2 @Mai2017a}{rmo} and
#' \insertCite{Arnold1975a}{rmo}.
#'
#' @family sampling-algorithms
#'
#' @examples
#' rmo(
#'     10, 3,
#'     c(0.4, 0.4, 0.1, 0.4, 0.1, 0.1, 0.4)
#' )
#' ## independence
#' rmo(
#'     10, 3,
#'     c(1, 1, 0, 1, 0, 0, 0)
#' )
#' ## comonotone
#' rmo(
#'     10, 3,
#'     c(0, 0, 0, 0, 0, 0, 1)
#' )
#'
#' rmo(
#'     10, 3,
#'     c(0.4, 0.4, 0.1, 0.4, 0.1, 0.1, 0.4),
#'     method = "ESM"
#' )
#' ## independence
#' rmo(
#'     10, 3,
#'     c(1, 1, 0, 1, 0, 0, 0),
#'     method = "ESM"
#' )
#' ## comonotone
#' rmo(
#'     10, 3,
#'     c(0, 0, 0, 0, 0, 0, 1),
#'     method = "ESM"
#' )
#'
#' rmo(
#'     10, 3,
#'     c(0.4, 0.4, 0.1, 0.4, 0.1, 0.1, 0.4),
#'     method = "AM"
#' )
#' ## independence
#' rmo(
#'     10, 3,
#'     c(1, 1, 0, 1, 0, 0, 0),
#'     method = "AM"
#' )
#' ## comonotone
#' rmo(
#'     10, 3,
#'     c(0, 0, 0, 0, 0, 0, 1),
#'     method = "AM"
#' )
#'
#' @references
#'  \insertAllCited{}
#'
#' @importFrom checkmate qassert assert_choice
#'
#' @export
rmo <- function(n, d, intensities, method = c("AM", "ESM")) {
    method <- match.arg(method)
    qassert(n, "X1[0,)")
    qassert(d, "X1[2,)")
    assert_choice(method, c("AM", "ESM"))

    if (method == "ESM") {
        Rcpp__rmo_esm(n, d, intensities)
    } else if (method == "AM") {
        Rcpp__rmo_am(n, d, intensities)
    } else {
        stop(sprintf("Method %s not implemented", method)) # nocov
    }
}


#' Sample from exchangeable Marshall–Olkin distributions
#'
#' Draws `n` iid samples from a `d`-variate *exchangeable Marshall–Olkin distribution* parametrized
#' by exchangeable *shock-size-arrival intensities*.
#'
#' @inheritParams rmo
#' @param ex_intensities a numeric vector with the exchangeable *shock-size-arrival intensities*.
#' @param method a string indicating which sampling algorithm should be used.
#'   Use "MDCM" for the *Markovian death-counting model*, "AM" for the *Arnold model*, and "ESM" for
#'   the *exogenous shock model*. We recommend using the *ESM* only for small dimensions; the *AM*
#'   can be used up until dimension \eqn{30}.
#'
#' @return `rexmo` returns a numeric matrix with `n` rows and `d` columns with the rows
#' corresponding to iid distributed samples of a `d`-variate *exchangeable Marshall–Olkin
#' distribution* with exchangeable *shock-size-arrival intensities* `ex_intensities`.
#'
#' @details
#' The *exchangeable Marshall–Olkin distribution* has the survival function
#' \deqn{
#'     \bar{F}{(t)}
#'         = \exp{\left\{ -\sum_{i=1}^{d}{ \eta_{i} \tau_{[i]} } \right\}} ,
#'             \quad t = {(t_{1}, \ldots, t_{d})} > 0 ,
#' }
#' for *exchangeable shock-size-arrival intensities* \eqn{\eta_{i} \geq 0}, \eqn{1 \leq i \leq d}
#' and \eqn{t_{[1]} \geq \cdots \geq t_{[d]}}, see \insertCite{Mai2017a}{rmo}.
#' They are called *shock-size-arrival intensities* as they correspond to the rates of the minimums
#' of all independent exponential random variables corresponding to \eqn{I}-sized shocks from the
#' *exogenous shock model (ESM)*, and a shock-size-arrival intensity \eqn{\eta_{i}} of shock-size
#' \eqn{i} equal to zero implies that no shock with size \eqn{i} occurs.
#' The relationsip of *exchangeable shock-size-arrival intensities* to the *shock-arrival
#' intensities* of the *Marshall–Olkin distribution*, see [rmo()], is as follows:
#' \deqn{
#'     \eta_{i}
#'         = \binom{d}{i} \lambda_{i} .
#' }
#'
#' ## Simulation algorithms
#'
#' ### General Marshall–Olkin sampling algorithms
#' The *exogenous shock model (ESM)* and *Arnold model (AM)* simulation algorithms for the general
#' *Marshall–Olkin distribution* can be used.
#' For this, the exchangeable *shock-size-arrival intensities* are converted to the
#' corresponding *shock-arrival intensities* and passed to [rmo()].
#'
#' ### Markovian death-set model
#' TBD; see \insertCite{@see pp. 122 psqq. @Mai2017a}{rmo} for a similar algorithm.
#'
#' @family sampling-algorithms
#'
#' @examples
#' rexmo(
#'     10, 3,
#'     c(1.2, 0.3, 0.4)
#' )
#' ## independence
#' rexmo(
#'     10, 3,
#'     c(3, 0, 0)
#' )
#' ## comonotone
#' rexmo(
#'     10, 3,
#'     c(0, 0, 1)
#' )
#'
#' rexmo(
#'     10, 3,
#'     c(1.2, 0.3, 0.4),
#'     method = "MDCM"
#' )
#' ## independence
#' rexmo(
#'     10, 3,
#'     c(3, 0, 0),
#'     method = "MDCM"
#' )
#' ## comonotone
#' rexmo(
#'     10, 3,
#'     c(0, 0, 1),
#'     method = "MDCM"
#' )
#'
#' rexmo(
#'     10, 3,
#'     c(1.2, 0.3, 0.4),
#'     method = "AM"
#' )
#' ## independence
#' rexmo(
#'     10, 3,
#'     c(3, 0, 0),
#'     method = "AM"
#' )
#' ## comonotone
#' rexmo(
#'     10, 3,
#'     c(0, 0, 1),
#'     method = "AM"
#' )
#'
#' rexmo(
#'     10, 3,
#'     c(1.2, 0.3, 0.4),
#'     method = "ESM"
#' )
#' ## independence
#' rexmo(
#'     10, 3,
#'     c(3, 0, 0),
#'     method = "ESM"
#' )
#' ## comonotone
#' rexmo(
#'     10, 3,
#'     c(0, 0, 1),
#'     method = "ESM"
#' )
#'
#' @references
#'  \insertAllCited{}
#'
#' @importFrom checkmate qassert assert_choice
#'
#' @export
rexmo <- function(n, d, ex_intensities, method = c("MDCM", "AM", "ESM")) {
    method <- match.arg(method)
    qassert(n, "X1[0,)")
    qassert(d, "X1[2,)")
    assert_choice(method, c("MDCM", "AM", "ESM"))

    if (method == "MDCM") {
        Rcpp__rexmo_mdcm(n, d, ex_intensities)
    } else if (method %in% c("AM", "ESM")) {
        intensities <- uexi2i(
            sapply(seq_along(ex_intensities), function(i) {
                divide_binomial_coefficient(ex_intensities[[i]], d, i)
            })
        )
        rmo(n, d, intensities, method = method)
    } else {
        stop(sprintf("Method %s not implemented", method)) # nocov
    }
}

#' Sample from extendible Marshall–Olkin distributions
#'
#' Draws `n` iid samples from a `d`-variate *extendible Marshall–Olkin distribution* parametrized by
#' Bernstein functions `bf`, essentially wrapping [rexmo()] by generating suitable *exchangeable
#' shock-size-arrival intensities*.
#'
#' @inheritParams rexmo
#' @param bf a [BernsteinFunction-class] with the *Bernstein function* of a *extendible
#'   Marshall–Olkin distribution*.
#'
#' @return `rextmo` returns a numeric matrix with `n` rows and `d` columns with the rows
#' corresponding to iid distributed samples of a `d`-variate *extendible Marshall–Olkin
#' distribution* with *Bernstein function* `bf`.
#'
#' @details
#' The *extendible Marshall–Olkin distribution* has the survival function
#' \deqn{
#'     \bar{F}{(t)}
#'         = \exp{\left\{ - \sum_{i=1}^{d}{ {[ \psi{(i)} - \psi{(i-1)} ]} t_{[i]} } \right\}} ,
#'             \quad t = {(t_{1}, \ldots, t_{d})} > 0 ,
#' }
#' for *Bernstein functions* \eqn{\psi}, see [BernsteinFunction-class], and \eqn{t_{[1]} \geq \cdots
#' \geq t_{[d]}}, see \insertCite{Mai2017a}{rmo}.
#' The relationship between *Bernstein functions* and *exchangeable shock-size-arrival intensities*
#' of the *exchangeable Marshall–Olkin distribution*, see [rexmo()], is as follows:
#' \deqn{
#'     \eta_{i}
#'         = \binom{d}{i} {(-1)}^{i-1} \Delta{ \psi{(d-i)} } ,
#'             \quad i \in {\{ 1 , \ldots , d \}} .
#' }
#'
#' @family sampling-algorithms
#'
#' @examples
#' rextmo(
#'     10, 3,
#'     AlphaStableBernsteinFunction(alpha = log2(2 - 0.5))
#' )
#' # independence
#' rextmo(
#'     10, 3,
#'     LinearBernsteinFunction(scale = 1)
#' )
#' # comonotone
#' rextmo(
#'     10, 3,
#'     ConstantBernsteinFunction(constant = 1)
#' )
#'
#' rextmo(
#'     10, 3,
#'     AlphaStableBernsteinFunction(alpha = log2(2 - 0.5)),
#'     method = "AM"
#' )
#' # independence
#' rextmo(
#'     10, 3,
#'     LinearBernsteinFunction(scale = 1),
#'     method = "AM"
#' )
#' # comonotone
#' rextmo(
#'     10, 3,
#'     ConstantBernsteinFunction(constant = 1),
#'     method = "AM"
#' )
#'
#' rextmo(
#'     10, 3,
#'     AlphaStableBernsteinFunction(alpha = log2(2 - 0.5)),
#'     method = "ESM"
#' )
#' # independence
#' rextmo(
#'     10, 3,
#'     LinearBernsteinFunction(scale = 1),
#'     method = "ESM"
#' )
#' # comonotone
#' rextmo(
#'     10, 3,
#'     ConstantBernsteinFunction(constant = 1),
#'     method = "ESM"
#' )
#'
#' @references
#'  \insertAllCited{}
#'
#' @importFrom checkmate qassert assert_choice
#'
#' @export
rextmo <- function(n, d, bf, method = c("MDCM", "AM", "ESM")) {
    method <- match.arg(method)
    qassert(n, "X1[0,)")
    qassert(d, "X1[2,)")
    assert_choice(method, c("MDCM", "AM", "ESM"))

    rexmo(n, d, exIntensities(bf, d), method = method)
}


#' Sample from parametrized extendible MO distributions
#'
#' Draws `n` iid samples from a `d`-variate *paramatrized extendible MO distribution*.
#'
#' @inheritParams rextmo
#' @param a a non-negative double for the *killing-rate* \eqn{a} of the *Bernstein function*.
#' @param b a non-negative double for the *drift* \eqn{b} of the *Bernstein function*.
#' @param gamma a position double for the scaling of the integral part of the *Bernstein function*.
#' @param eta a numeric vector for the family's parameters, see Details.
#' @param family a string indicating the parametrized family.
#'    Use "Armageddon" for the *Armageddon* family, "Poisson" for the *Poisson family*, "Pareto" for
#'    the *Pareto family*, "Exponential" for the *Exponential family*, "AlphaStable" for the
#'    *\eqn{\alpha}-stable family*, "InverseGaussian" for the *Inverse-Gaussian family*, "Gamma" for
#'    the *Gamma family*.
#' @param method a string indicating which sampling algorithm should be used.
#'   Use "MDCM" for the *Markovian death-set model*, "LFM" for the *Lévy–frailty model*,  "AM" for
#'   the *Arnold model*, and "ESM" for the *exogenous shock model* (in case of the *Armageddon
#'   family*, the algorithm is optimized to consider only finite shocks).
#'   We recommend using the *ESM* only for small dimensions; the *AM* can be used up until dimension
#'   \eqn{30}.
#'
#' @return `rpextmo` returns a numeric matrix with `n` rows and `d` columns with the rows
#' corresponding to iid distributed samples of a `d`-variate *parametrized extendible
#' Marshall–Olkin distribution* with corresponding parameters.
#'
#' @details
#' A *parametrized ext. MO distribution* is a family of *ext. MO distributions*, see [rextmo()],
#' corresponding to *Bernstein functions* of the form
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
#' where \eqn{a, b \geq 0} and \eqn{\nu}, resp. \eqn{\sigma}, are the *Lévy measure*, resp.
#' *Stieltjes measure*, and \eqn{a > 0}, \eqn{b > 0}, or \eqn{\nu \not\equiv 0}, resp.
#' \eqn{\sigma \not\equiv 0}.
#'
#' ## Families
#'
#' All implemented families are listed in the following; some re-combinations are possible, see
#' [ScaledBernsteinFunction-class], [SumOfBernsteinFunctions-class], and
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
#' We have \eqn{\eta \in \mathbb{R}^2} with \eqn{\eta_1 \in {(0, 1)}, \eta_2 > 0} and Lévy measure
#' \deqn{
#'     \nu{(\mathrm{d}u)}
#'         = \eta_{1} \eta_{2}^{\eta_{1}} \cdot u^{-\eta_{1}-1}  1_{\{ u > \eta_{2}\}} \mathrm{d}u ,
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
#'         = 1_{\{ x > 0\}} a + b x + \gamma \cdot {\left[ \sqrt{2 x + \eta^2} - \eta \right]},
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
#'             \frac{\sqrt{2 u - \eta^2}}{u} 1_{\{ u > \eta^2 / 2 \}} \mathrm{d}u ,
#' }
#' see [InverseGaussianBernsteinFunction-class].
#'
#' ### Gamma family
#' We have \eqn{\eta > 0}, Bernstein function
#' \deqn{
#'     \psi{(x)}
#'         = 1_{\{ x > 0\}} a + b x + \gamma \cdot \log{\left( 1 +  \frac{x}{\eta} \right)} ,
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
#' The *MDCM*, *AM*, and *ESM* simulation algorithms for the *exchangeable Marshall–Olkin
#' distribution* can be used.
#' For this, the corresponding Bernstein function is passed to [rextmo()].
#' An exception is the *ESM* for the *Armageddon family* which uses an optimized version considering
#' only finite shock-times.
#'
#' ### Lévy-frailty simulation algorithms
#' The *Lévy-frailty model* simulates the elements of the random vector as first-hitting times of a
#' compound Poisson subordinator \eqn{\Lambda} into sets \eqn{(E_i, \infty)} for iid unit
#' exponential random variables.
#' Here, the subordinator is a linear combination of a pure-drift subordinator, a pure-killing
#' subordinator, and a pure-jump compound Poisson subordinator, i.e.
#' \deqn{
#'    \Lambda_{t}
#'        = \infty \cdot 1_{\{ \epsilon > a t \}} + b t + \sum_{j=1}^{N_{\gamma t}} X_{j} ,
#'            \quad t \geq 0,
#' }
#' where \eqn{\epsilon} is a unit exponential rv, `n` is a Poisson process, and \eqn{X_{1},
#' X_{2}, \ldots} are iid jumps from the corresponding jump distribution,
#' see \insertCite{@see pp. 140 psqq. @Mai2017a}{rmo}.
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
#'     10, 3,
#'     a = 0.2, b = 0.5,
#'     family = "Armageddon",
#'     method = "ESM"
#' )
#' ## comonotone
#' rpextmo(
#'     10, 3,
#'     a = 1,
#'     family = "Armageddon",
#'     method = "ESM"
#' )
#' ## independence
#' rpextmo(
#'     10, 3,
#'     b = 1,
#'     family = "Armageddon",
#'     method = "ESM"
#' )
#'
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5,
#'     family = "Armageddon",
#'     method = "LFM"
#' )
#' ## comonotone
#' rpextmo(
#'     10, 3,
#'     a = 1,
#'     family = "Armageddon",
#'     method = "LFM"
#' )
#' ## independence
#' rpextmo(
#'     10, 3,
#'     b = 1,
#'     family = "Armageddon",
#'     method = "LFM"
#' )
#'
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5,
#'     family = "Armageddon",
#'     method = "MDCM"
#' )
#' ## comonotone
#' rpextmo(
#'     10, 3,
#'     a = 1,
#'     family = "Armageddon",
#'     method = "MDCM"
#' )
#' ## independence
#' rpextmo(
#'     10, 3,
#'     b = 1,
#'     family = "Armageddon",
#'     method = "MDCM"
#' )
#'
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5,
#'     family = "Armageddon",
#'     method = "AM"
#' )
#' ## comonotone
#' rpextmo(
#'     10, 3,
#'     a = 1,
#'     family = "Armageddon",
#'     method = "AM"
#' )
#' ## independence
#' rpextmo(
#'     10, 3,
#'     b = 1,
#'     family = "Armageddon",
#'     method = "AM"
#' )
#'
#' ## Poisson
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Poisson"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Poisson",
#'     method = "ESM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Poisson",
#'     method = "LFM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Poisson",
#'     method = "MDCM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Poisson",
#'     method = "AM"
#' )
#'
#' ## Pareto
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = c(0.5, 1e-4), family = "Pareto"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = c(0.5, 1e-4), family = "Pareto",
#'     method = "ESM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = c(0.5, 1e-4), family = "Pareto",
#'     method = "LFM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = c(0.5, 1e-4), family = "Pareto",
#'     method = "MDCM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = c(0.5, 1e-4), family = "Pareto",
#'     method = "AM"
#' )
#'
#' ## Exponential
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Exponential"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Exponential",
#'     method = "ESM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Exponential",
#'     method = "LFM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Exponential",
#'     method = "MDCM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Exponential",
#'     method = "AM"
#' )
#'
#' ## Alpha-Stable
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "AlphaStable"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "AlphaStable",
#'     method = "ESM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "AlphaStable",
#'     method = "MDCM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "AlphaStable",
#'     method = "AM"
#' )
#'
#' ## Inverse Gaussian
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "InverseGaussian"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "InverseGaussian",
#'     method = "ESM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "InverseGaussian",
#'     method = "MDCM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "InverseGaussian",
#'     method = "AM"
#' )
#'
#' ## Gamma
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Gamma"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Gamma",
#'     method = "ESM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Gamma",
#'     method = "MDCM"
#' )
#' rpextmo(
#'     10, 3,
#'     a = 0.2, b = 0.5, gamma = 2,
#'     eta = 0.5, family = "Gamma",
#'     method = "AM"
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
                        "Exponential", "AlphaStable", "InverseGaussian", "Gamma"
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
