% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s4-CompositeScaledBernsteinFunction.R
\docType{class}
\name{CompositeScaledBernsteinFunction-class}
\alias{CompositeScaledBernsteinFunction-class}
\alias{CompositeScaledBernsteinFunction}
\title{Class for composite scaled Bernstein functions}
\description{
Bernstein functions are stable under composition, i.e. if \eqn{\psi} is
a Bernstein function and \code{c > 0}, then
\deqn{
  x \mapsto \psi(c x)
}
is also a Bernstein function.
}
\section{Slots}{

\describe{
\item{\code{cscale}}{The scale of the inner linear Bernstein function of the
composition.}

\item{\code{original}}{The original Bernstein function.}
}}

\examples{
# Create an object of class CompositeScaleBernsteinFunction
CompositeScaledBernsteinFunction()
CompositeScaledBernsteinFunction(
  cscale = 0.5,
  original = AlphaStableBernsteinFunction(0.5)
)
}
\seealso{
\code{\link[=calcIterativeDifference]{calcIterativeDifference()}}, \code{\link[=calcShockArrivalIntensities]{calcShockArrivalIntensities()}},
\code{\link[=calcExShockArrivalIntensities]{calcExShockArrivalIntensities()}}, \code{\link[=calcExShockSizeArrivalIntensities]{calcExShockSizeArrivalIntensities()}},
\code{\link[=calcMDCMGeneratorMatrix]{calcMDCMGeneratorMatrix()}}, \code{\link[=rextmo]{rextmo()}}, \code{\link[=rpextmo]{rpextmo()}}

Other Bernstein function classes: 
\code{\link{AlphaStableBernsteinFunction-class}},
\code{\link{BernsteinFunction-class}},
\code{\link{CompleteBernsteinFunction-class}},
\code{\link{ConstantBernsteinFunction-class}},
\code{\link{ConvexCombinationOfBernsteinFunctions-class}},
\code{\link{ExponentialBernsteinFunction-class}},
\code{\link{GammaBernsteinFunction-class}},
\code{\link{InverseGaussianBernsteinFunction-class}},
\code{\link{LevyBernsteinFunction-class}},
\code{\link{LinearBernsteinFunction-class}},
\code{\link{ParetoBernsteinFunction-class}},
\code{\link{PoissonBernsteinFunction-class}},
\code{\link{ScaledBernsteinFunction-class}},
\code{\link{SumOfBernsteinFunctions-class}}

Other Bernstein function transformer classes: 
\code{\link{ConvexCombinationOfBernsteinFunctions-class}},
\code{\link{ScaledBernsteinFunction-class}},
\code{\link{SumOfBernsteinFunctions-class}}
}
\concept{Bernstein function classes}
\concept{Bernstein function transformer classes}
