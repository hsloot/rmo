% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample-rpextmo.R
\name{rpextmo}
\alias{rpextmo}
\title{Sample from parametrized extendible MO distributions}
\usage{
rpextmo(
  n,
  d,
  a = 0,
  b = 0,
  gamma = 1,
  eta = NULL,
  family = c("Armageddon", "Poisson", "Pareto", "Exponential", "AlphaStable",
    "InverseGaussian", "Gamma"),
  method = c("MDCM", "LFM", "AM", "ESM")
)
}
\arguments{
\item{n}{an integer for the \emph{number of samples}.}

\item{d}{an integer for the \emph{dimension}.}

\item{a}{a non-negative double for the \emph{killing-rate} \eqn{a} of the
\emph{Bernstein function}.}

\item{b}{a non-negative double for the \emph{drift} \eqn{b} of the
\emph{Bernstein function}.}

\item{gamma}{a position double for the scaling of the integral part of the
\emph{Bernstein function}.}

\item{eta}{a numeric vector for the family's parameters, see Details.}

\item{family}{a string indicating the parametrized family.
Use "Armageddon" for the \emph{Armageddon} family, "Poisson" for the
\emph{Poisson family}, "Pareto" for the \emph{Pareto family}, "Exponential" for the
\emph{Exponential family}, "AlphaStable" for the \emph{\eqn{\alpha}-stable family},
"InverseGaussian" for the \emph{Inverse-Gaussian family}, "Gamma" for
the \emph{Gamma family}.}

\item{method}{a string indicating which sampling algorithm should be used.
Use "MDCM" for the \emph{Markovian death-set model}, "LFM" for the
\emph{Lévy–frailty model}, "AM" for the \emph{Arnold model}, and "ESM" for the
\emph{exogenous shock model} (in case of the \emph{Armageddon family}, the algorithm
is optimized to consider only finite shocks). We recommend using the \emph{ESM}
only for small dimensions; the \emph{AM} can be used up until dimension
\eqn{30}.}
}
\value{
\code{rpextmo} returns a numeric matrix with \code{n} rows and \code{d} columns with
the rows corresponding to iid distributed samples of a \code{d}-variate
\emph{parametrized extendible Marshall–Olkin distribution} with corresponding
parameters.
}
\description{
Draws \code{n} iid samples from a \code{d}-variate
\emph{paramatrized extendible MO distribution}.
}
\details{
A \emph{parametrized ext. MO distribution} is a family of \emph{ext. MO distributions},
see \code{\link[=rextmo]{rextmo()}}, corresponding to \emph{Bernstein functions} of the form
\deqn{
    \psi{(x)}
        = 1_{\{ x > 0 \}} a + b x + \gamma \cdot
            \int_{0}^{\infty}{ {[1 - e^{-ux}]} \nu{(\mathrm{d}u)} },
                \quad x \geq 0 ,
}
or
\deqn{
   \psi{(x)}
       = 1_{\{ x > 0 \}} a + b x + \gamma \cdot
           \int_{0}^{\infty}{ \frac{x}{x + u} \sigma{(\mathrm{d}u)} },
               \quad x \geq 0 ,
}
where \eqn{a, b \geq 0} and \eqn{\nu}, resp. \eqn{\sigma}, are the
\emph{Lévy measure}, resp. \emph{Stieltjes measure}, and \eqn{a > 0}, \eqn{b > 0}, or
\eqn{\nu \not\equiv 0}, resp. \eqn{\sigma \not\equiv 0}.
\subsection{Families}{

All implemented families are listed in the following; some re-combinations
are possible, see \linkS4class{ScaledBernsteinFunction},
\linkS4class{SumOfBernsteinFunctions}, and
\linkS4class{CompositeScaledBernsteinFunction}.
\subsection{Armageddon family}{

We have \eqn{\nu = \sigma \equiv 0} and Bernstein function
\deqn{
    \psi{(x)}
        = 1_{\{ x > 0\}} a + b x ,
            \quad x \geq 0 ,
}
see \linkS4class{ConstantBernsteinFunction} and \linkS4class{LinearBernsteinFunction}.
}

\subsection{Poisson family}{

We have \eqn{\eta > 0}, Bernstein function
\deqn{
    \psi{(x)}
        = 1_{\{ x > 0\}} a + b x + \gamma \cdot {[1 - e^{-\eta x}]},
            \quad x \geq 0 ,
}
and Lévy measure
\deqn{
    \nu{(\mathrm{d}u)}
        = \delta_{\{ \eta \}}{(\mathrm{d}u)} ,
}
see \linkS4class{PoissonBernsteinFunction}.
}

\subsection{Pareto family}{

We have \eqn{\eta \in \mathbb{R}^2} with
\eqn{\eta_1 \in {(0, 1)}, \eta_2 > 0} and Lévy measure
\deqn{
    \nu{(\mathrm{d}u)}
        = \eta_{1} \eta_{2}^{\eta_{1}} \cdot
         u^{-\eta_{1}-1}  1_{\{ u > \eta_{2}\}} \mathrm{d}u ,
}
see \linkS4class{ParetoBernsteinFunction}.
}

\subsection{Exponential family}{

We have \eqn{\eta > 0}, Bernstein function
\deqn{
    \psi{(x)}
        = 1_{\{ x > 0\}} a + b x + \gamma \cdot \frac{x}{x + \eta} ,
            \quad x \geq 0 ,
}
and Lévy measure
\deqn{
    \nu{(\mathrm{d}u)}
        = \eta e^{-\eta u} \mathrm{d}u ,
}
and Stieltjes measure
\deqn{
    \sigma{(\mathrm{d}u)}
        = \delta_{\{ \eta \}}{(\mathrm{d}u)} ,
}
see \linkS4class{ExponentialBernsteinFunction}.
}

\subsection{\eqn{\alpha}-stable family}{

We have \eqn{\eta \in {(0, 1)}}, Bernstein function
\deqn{
    \psi{(x)}
        = 1_{\{ x > 0\}} a + b x + \gamma \cdot x^{\eta} ,
            \quad x \geq 0 ,
}
Lévy measure,
\deqn{
    \nu{(\mathrm{d}u)}
        = \frac{\eta}{\Gamma{(1 - \eta)}} \cdot u^{-\eta-1} \mathrm{d}u ,
}
and Stieljtes measure
\deqn{
    \sigma{(\mathrm{d}u)}
        = \frac{\sin{(\eta \pi)}}{\pi} \cdot u^{\eta - 1} \mathrm{d}u ,
}
see \linkS4class{AlphaStableBernsteinFunction}.
}

\subsection{Inverse-Gaussian family}{

We have \eqn{\eta > 0}, Bernstein function
\deqn{
    \psi{(x)}
        = 1_{\{ x > 0\}} a + b x + \gamma \cdot
             {\left[ \sqrt{2 x + \eta^2} - \eta \right]},
            \quad x \geq 0 ,
}
Lévy measure
\deqn{
    \nu{(\mathrm{d}u)}
        = \frac{1}{ \sqrt{2 \pi} } \cdot
            \frac{ e^{-\frac{1}{2} \eta^2 u} }{ \sqrt{u^3} } \mathrm{d}u ,
}
and Stieltjes measure
\deqn{
    \sigma{(\mathrm{d}u)}
        = \frac{\sin{(\pi / 2)}}{\pi} \cdot
            \frac{\sqrt{2 u - \eta^2}} {u}
             1_{\{ u > \eta^2 / 2 \}} \mathrm{d}u ,
}
see \linkS4class{InverseGaussianBernsteinFunction}.
}

\subsection{Gamma family}{

We have \eqn{\eta > 0}, Bernstein function
\deqn{
    \psi{(x)}
        = 1_{\{ x > 0\}} a + b x + \gamma \cdot
             \log{\left( 1 +  \frac{x}{\eta} \right)} ,
            \quad x \geq 0 ,
}
Lévy measure
\deqn{
    \nu{(\mathrm{d}u)}
        = e^{-\eta u} u^{-1} \mathrm{d}u ,
}
and Stieljtes measure
\deqn{
    \sigma{(\mathrm{d}u)}
        = u^{-1} 1_{\{ u > \eta \}} \mathrm{d}u ,
}
see \linkS4class{GammaBernsteinFunction}.
}

}

\subsection{Simulation algorithms}{
\subsection{Exchangeable Marshall–Olkin simulation algorithms}{

The \emph{MDCM}, \emph{AM}, and \emph{ESM} simulation algorithms for the
\emph{exchangeable Marshall–Olkin distribution} can be used.
For this, the corresponding Bernstein function is passed to \code{\link[=rextmo]{rextmo()}}.
An exception is the \emph{ESM} for the \emph{Armageddon family} which uses an optimized
version considering only finite shock-times.
}

\subsection{Lévy-frailty simulation algorithms}{

The \emph{Lévy-frailty model} simulates the elements of the random vector as
first-hitting times of a compound Poisson subordinator \eqn{\Lambda} into
sets \eqn{(E_i, \infty)} for iid unit exponential random variables.
Here, the subordinator is a linear combination of a pure-drift subordinator,
a pure-killing subordinator, and a pure-jump compound Poisson subordinator,
i.e.
\deqn{
   \Lambda_{t}
       = \infty \cdot 1_{\{ \epsilon > a t \}} + b t +
         \sum_{j=1}^{N_{\gamma t}} X_{j} ,
           \quad t \geq 0,
}
where \eqn{\epsilon} is a unit exponential rv, \code{n} is a Poisson process, and
\eqn{X_{1}, X_{2}, \ldots} are iid jumps from the corresponding jump
distribution, see \insertCite{@see pp. 140 psqq. @Mai2017a}{rmo}.
}

}
}
\examples{
## Armageddon
rpextmo(10, 3, a = 0.2, b = 0.5)
## comonotone
rpextmo(10, 3, a = 1)
## independence
rpextmo(10, 3, b = 1)

rpextmo(10, 3, a = 0.2, b = 0.5, method = "ESM")
## comonotone
rpextmo(10, 3, a = 1, method = "ESM")
## independence
rpextmo(10, 3, b = 1, method = "ESM")

rpextmo(10, 3, a = 0.2, b = 0.5, method = "LFM")
## comonotone
rpextmo(10, 3, a = 1, method = "LFM")
## independence
rpextmo(10, 3, b = 1, method = "LFM")

rpextmo(10, 3, a = 0.2, b = 0.5, method = "MDCM")
## comonotone
rpextmo(10, 3, a = 1, method = "MDCM")
## independence
rpextmo(10, 3, b = 1, method = "MDCM")

rpextmo(10, 3, a = 0.2, b = 0.5, method = "AM")
## comonotone
rpextmo(10, 3, a = 1, method = "AM")
## independence
rpextmo(10, 3, b = 1, method = "AM")

rpextmo(10, 3, a = 0.2, b = 0.5, family = "Armageddon")
## comonotone
rpextmo(10, 3, a = 1, family = "Armageddon")
## independence
rpextmo(10, 3, b = 1, family = "Armageddon")

rpextmo(
    10, 3,
    a = 0.2, b = 0.5,
    family = "Armageddon",
    method = "ESM"
)
## comonotone
rpextmo(
    10, 3,
    a = 1,
    family = "Armageddon",
    method = "ESM"
)
## independence
rpextmo(
    10, 3,
    b = 1,
    family = "Armageddon",
    method = "ESM"
)

rpextmo(
    10, 3,
    a = 0.2, b = 0.5,
    family = "Armageddon",
    method = "LFM"
)
## comonotone
rpextmo(
    10, 3,
    a = 1,
    family = "Armageddon",
    method = "LFM"
)
## independence
rpextmo(
    10, 3,
    b = 1,
    family = "Armageddon",
    method = "LFM"
)

rpextmo(
    10, 3,
    a = 0.2, b = 0.5,
    family = "Armageddon",
    method = "MDCM"
)
## comonotone
rpextmo(
    10, 3,
    a = 1,
    family = "Armageddon",
    method = "MDCM"
)
## independence
rpextmo(
    10, 3,
    b = 1,
    family = "Armageddon",
    method = "MDCM"
)

rpextmo(
    10, 3,
    a = 0.2, b = 0.5,
    family = "Armageddon",
    method = "AM"
)
## comonotone
rpextmo(
    10, 3,
    a = 1,
    family = "Armageddon",
    method = "AM"
)
## independence
rpextmo(
    10, 3,
    b = 1,
    family = "Armageddon",
    method = "AM"
)

## Poisson
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Poisson"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Poisson",
    method = "ESM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Poisson",
    method = "LFM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Poisson",
    method = "MDCM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Poisson",
    method = "AM"
)

## Pareto
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = c(0.5, 1e-4), family = "Pareto"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = c(0.5, 1e-4), family = "Pareto",
    method = "ESM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = c(0.5, 1e-4), family = "Pareto",
    method = "LFM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = c(0.5, 1e-4), family = "Pareto",
    method = "MDCM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = c(0.5, 1e-4), family = "Pareto",
    method = "AM"
)

## Exponential
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Exponential"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Exponential",
    method = "ESM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Exponential",
    method = "LFM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Exponential",
    method = "MDCM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Exponential",
    method = "AM"
)

## Alpha-Stable
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "AlphaStable"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "AlphaStable",
    method = "ESM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "AlphaStable",
    method = "MDCM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "AlphaStable",
    method = "AM"
)

## Inverse Gaussian
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "InverseGaussian"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "InverseGaussian",
    method = "ESM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "InverseGaussian",
    method = "MDCM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "InverseGaussian",
    method = "AM"
)

## Gamma
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Gamma"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Gamma",
    method = "ESM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Gamma",
    method = "MDCM"
)
rpextmo(
    10, 3,
    a = 0.2, b = 0.5, gamma = 2,
    eta = 0.5, family = "Gamma",
    method = "AM"
)

}
\references{
\insertAllCited{}
}
\seealso{
Other sampling-algorithms: 
\code{\link{rexmo}()},
\code{\link{rextmo}()},
\code{\link{rmo}()}
}
\concept{sampling-algorithms}
