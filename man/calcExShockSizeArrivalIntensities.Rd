% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s4-calcExShockSizeArrivalIntensities.R
\name{calcExShockSizeArrivalIntensities}
\alias{calcExShockSizeArrivalIntensities}
\title{Calculate exchangeable shock-size-arrival intensities}
\usage{
calcExShockSizeArrivalIntensities(object, d, cscale = 1, ...)
}
\arguments{
\item{object}{An object deriving from the class \linkS4class{BernsteinFunction}.}

\item{d}{A positive integer, larger than two, for the \emph{dimension}.}

\item{cscale}{A positive number for the \emph{composite scaling factor}.}

\item{...}{pass-through parameter.}
}
\description{
Calculates \emph{exchangeable shock-size-arrival intensities}, the
\code{theta} parameter for \code{\link[=rexmo]{rexmo()}}.
}
\details{
For a given Bernstein function, the exchangeable shock-size-arrival
intensities are defined as
\deqn{
  \eta_{i}
    = \binom{d}{i} {(-1)}^{i-1} \Delta^{i}{ \psi{(d-i)} } ,
      \quad 1 \leq i \leq d .
}
The calculation of the exchangeable shock-size-arrival intensities using this
formula is usually not numerically stable. Consequently, the various
alternative approaches are used dependent on the class of the Bernstein
function.
}
\examples{
bf <- AlphaStableBernsteinFunction(alpha = 0.4)
calcExShockSizeArrivalIntensities(bf, 3)
}
\seealso{
\code{\link[=rexmo]{rexmo()}}

Other Bernstein function generics: 
\code{\link{calcExShockArrivalIntensities}()},
\code{\link{calcIterativeDifference}()},
\code{\link{calcMDCMGeneratorMatrix}()},
\code{\link{calcShockArrivalIntensities}()},
\code{\link{calcValue}()},
\code{\link{getDefaultMethodString}()},
\code{\link{getLevyDensity}()},
\code{\link{getStieltjesDensity}()}
}
\concept{Bernstein function generics}
