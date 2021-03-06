% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allClass-S4.R, R/allInitialize-S4.R,
%   R/allShow-S4.R, R/valueOf-S4.R
\docType{class}
\name{ScaledBernsteinFunction-class}
\alias{ScaledBernsteinFunction-class}
\alias{ScaledBernsteinFunction}
\alias{initialize,ScaledBernsteinFunction-method}
\alias{initialize,ScaledBernsteinFunction,ANY-method}
\alias{show,ScaledBernsteinFunction-method}
\alias{valueOf,ScaledBernsteinFunction-method}
\alias{valueOf,ScaledBernsteinFunction,ANY-method}
\title{Class for \emph{scaled Bernstein functions}}
\usage{
\S4method{initialize}{ScaledBernsteinFunction}(.Object, scale, original)

\S4method{show}{ScaledBernsteinFunction}(object)

\S4method{valueOf}{ScaledBernsteinFunction}(object, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...)
}
\arguments{
\item{.Object}{ An object:  see the \dQuote{Initialize Methods} section.}

\item{scale}{Positive number.}

\item{original}{Derives from \linkS4class{BernsteinFunction}.}

\item{object}{An object deriving from \linkS4class{LevyBernsteinFunction}
(for \code{levyDensity}) or \linkS4class{CompleteBernsteinFunction}
(for \code{stieltjesDensity}).}

\item{x}{Non-negativ numeric vector at which the iterated difference of
the Bernstein function is evaluated.}

\item{difference_order}{The order of the alternating iterated forward
differences taken on the Bernstein function (\eqn{j} in
the representation).}

\item{n}{Non-negative numbers for the binomial factors.}

\item{k}{Non-negative numbers for the binomial factors.}

\item{cscale}{Positive number, the composit scaling factor.}

\item{...}{arguments to specify properties of the new object, to
      be passed to \code{initialize()}.}
}
\description{
Berstein functions are stable under (non-negative) scalar multiplication,
i.e. if \eqn{\psi} is a Bernstein function and \eqn{\lambda \geq 0} , then
\deqn{
  x \mapsto \lambda \psi(x), x > 0,
}
is also a Bernstein function.
}
\section{Functions}{
\itemize{
\item \code{initialize,ScaledBernsteinFunction-method}: Constructor

\item \code{show,ScaledBernsteinFunction-method}: Display the object.

\item \code{valueOf,ScaledBernsteinFunction-method}: Calculates the iterated differences of the Bernstein function, see \code{\link[=valueOf]{valueOf()}}
}}

\section{Slots}{

\describe{
\item{\code{scale}}{The scalar factor with which the original Bernstein function
is to be multiplied.}

\item{\code{original}}{The original Bernstein function which is to be multiplied.}
}}

\examples{
ScaledBernsteinFunction()
original_bf <- AlphaStableBernsteinFunction(alpha = 0.5)
ScaledBernsteinFunction(scale = 2, original = original_bf)
}
\seealso{
\linkS4class{BernsteinFunction},
\code{\link[=valueOf]{valueOf()}}
}
